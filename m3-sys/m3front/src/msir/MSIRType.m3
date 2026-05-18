MODULE MSIRType;

IMPORT MSIR, Type, Int, LInt, Bool, Target;
IMPORT Addr, Reff, Charr, WCharr, Reel, LReel, EReel;
IMPORT RecordType, Field, M3ID, Value, Text, RefType, ArrayType, TInt;

PROCEDURE Translate(t: Type.T): MSIR.T =
  VAR base: Type.T;  info: Type.Info;  nameId: M3ID.T;  typeName: TEXT;
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    (* Capture the human-readable name BEFORE stripping the Named wrapper.
       Typename uses underscores for dots (e.g. "Main__Point").
       Strip the "Module__" prefix so we show just "Point". *)
    Type.Typename(t, nameId);
    IF nameId = M3ID.NoID THEN
      typeName := "";
    ELSE
      typeName := M3ID.ToText(nameId);
      VAR n := Text.Length(typeName);  i := 0;
      BEGIN
        WHILE i + 1 < n DO
          IF Text.GetChar(typeName, i) = '_'
             AND Text.GetChar(typeName, i + 1) = '_' THEN
            typeName := Text.Sub(typeName, i + 2);
            EXIT
          END;
          INC(i);
        END;
      END;
    END;
    base := Type.Base(t);  (* strips Named, Packed, Subrange layers *)

    (* Integer-like builtins — check identity before falling to class *)
    IF base = Int.T   THEN RETURN MSIR.TI(Target.Integer.size) END;
    IF base = LInt.T  THEN RETURN MSIR.TI(Target.Longint.size) END;
    IF base = Bool.T  THEN RETURN MSIR.TI1() END;
    IF base = Charr.T THEN RETURN MSIR.TI(Target.Char.size) END;
    IF base = WCharr.T THEN
      (* WIDECHAR size from CheckInfo since Target has no WChar constant *)
      EVAL Type.CheckInfo(base, info);
      RETURN MSIR.TI(info.size);
    END;

    (* Floating-point builtins *)
    IF base = Reel.T  THEN RETURN MSIR.TF(32) END;
    IF base = LReel.T THEN RETURN MSIR.TF(64) END;
    IF base = EReel.T THEN RETURN MSIR.TF(Target.Extended.size) END;

    (* Pointer / reference builtins *)
    IF base = Addr.T  THEN RETURN MSIR.TPtr(MSIR.TVoid()) END;
    IF base = Reff.T  THEN RETURN MSIR.TGcRef(MSIR.TVoid()) END;

    (* Remaining types: dispatch on class *)
    EVAL Type.CheckInfo(base, info);
    CASE info.class OF

    | Type.Class.Integer, Type.Class.Longint =>
        (* Some non-builtin integer-class type (unusual but possible). *)
        IF info.size <= 0 THEN RETURN NIL END;
        RETURN MSIR.TI(info.size);

    | Type.Class.Enum =>
        IF info.size <= 0 THEN RETURN NIL END;
        RETURN MSIR.TI(info.size);

    | Type.Class.Real     => RETURN MSIR.TF(32);
    | Type.Class.Longreal => RETURN MSIR.TF(64);
    | Type.Class.Extended => RETURN MSIR.TF(Target.Extended.size);

    | Type.Class.Record =>
        RETURN TranslateRecord(base, typeName);

    | Type.Class.Set =>
        (* All set sizes map to iN — LLVM supports arbitrary-width integers. *)
        IF info.size > 0 THEN
          RETURN MSIR.TI (info.size);
        END;
        RETURN NIL;

    | Type.Class.Array =>
        RETURN TranslateFixedArray(base);

    | Type.Class.OpenArray =>
        RETURN TranslateOpenArray(base);

    | Type.Class.Procedure =>
        RETURN MSIR.TPtr(MSIR.TVoid());   (* function pointer, opaque *)

    | Type.Class.Object, Type.Class.Opaque =>
        (* Use isTraced so that UNTRACED OBJECT and untraced opaque supertypes
           get TPtr rather than TGcRef.  Vtable-aware typed refs are future work. *)
        IF info.isTraced
          THEN RETURN MSIR.TGcRef(MSIR.TVoid());
          ELSE RETURN MSIR.TPtr (MSIR.TVoid());
        END;

    | Type.Class.Ref =>
        VAR target: Type.T;  targetMsir: MSIR.T := MSIR.TVoid();
        BEGIN
          (* Try to thread a typed target so we get e.g. TPtr(i64) for
             UNTRACED REF INTEGER. Conservatively fall back to TVoid for
             pointer-of-pointer or pointer-to-record/object cases — those
             can be self-referential and would recurse without a visited
             set. *)
          IF RefType.Split(base, target) AND target # NIL THEN
            VAR ti: Type.Info;
            BEGIN
              EVAL Type.CheckInfo(target, ti);
              CASE ti.class OF
              | Type.Class.Integer, Type.Class.Longint,
                Type.Class.Enum, Type.Class.Real,
                Type.Class.Longreal, Type.Class.Extended =>
                  VAR tt := Translate(target);
                  BEGIN
                    IF tt # NIL THEN targetMsir := tt END;
                  END;
              ELSE
                (* Record/Object/Ref/etc.: leave as void to avoid recursion. *)
              END;
            END;
          END;
          IF info.isTraced
            THEN RETURN MSIR.TGcRef(targetMsir);
            ELSE RETURN MSIR.TPtr(targetMsir);
          END;
        END;

    ELSE
      RETURN NIL;
    END;
  END Translate;

PROCEDURE TranslateRecord(t: Type.T;  name: TEXT): MSIR.T =
  VAR
    fields: Value.T;
    n:      INTEGER := 0;
    v:      Value.T;
    finfo:  Field.Info;
    fti:    Type.Info;
  BEGIN
    IF NOT RecordType.Split(t, fields) THEN RETURN NIL END;
    v := fields;
    WHILE v # NIL DO INC(n);  v := v.next END;
    (* Pre-check: if any field has a sub-byte offset or size (packed record),
       fall back to [N x i8] so the variable can still enter the varMap.
       Field accesses on such records emit a more specific abandon. *)
    v := fields;
    WHILE v # NIL DO
      Field.Split(v, finfo);
      EVAL Type.CheckInfo(finfo.type, fti);
      IF finfo.offset MOD Target.Byte # 0 OR fti.size MOD Target.Byte # 0 THEN
        RETURN ByteArrayFallback(t);
      END;
      v := v.next;
    END;
    VAR msirFields := NEW(REF ARRAY OF MSIR.Field, n);
    BEGIN
      v := fields;
      FOR i := 0 TO n - 1 DO
        Field.Split(v, finfo);
        EVAL Type.CheckInfo(finfo.type, fti);
        VAR ft := Translate(finfo.type);
        BEGIN
          IF ft = NIL THEN RETURN NIL END;
          (* When actual storage width differs from natural MSIR type (e.g.
             [0..255] stored as 8 bits but Translate gives i64), use TI(size).
             Guard on BitWidth > 0 to leave non-scalar types (GcRef, Ptr, …)
             unchanged — their CM3 size matches pointer width but they carry
             GC metadata that must not be erased. *)
          IF fti.size > 0 AND MSIR.BitWidth(ft) > 0
                          AND fti.size # MSIR.BitWidth(ft) THEN
            ft := MSIR.TI(fti.size);
          END;
          msirFields[i].name := M3ID.ToText(finfo.name);
          msirFields[i].type := ft;
        END;
        v := v.next;
      END;
      RETURN MSIR.TStruct(name, msirFields^);
    END;
  END TranslateRecord;

PROCEDURE TranslateOpenArray(t: Type.T): MSIR.T =
  VAR indexT, eltT: Type.T;  eltMsir: MSIR.T;
  BEGIN
    IF NOT ArrayType.Split(t, indexT, eltT) THEN RETURN NIL END;
    (* indexT = NIL for open arrays at every level of nesting. *)
    eltMsir := Translate(eltT);
    IF eltMsir = NIL THEN RETURN NIL END;
    RETURN MSIR.TOpenArray(1, eltMsir);
  END TranslateOpenArray;

PROCEDURE ByteArrayFallback(t: Type.T): MSIR.T =
  (* Return [N x i8] for a type whose contents can't be directly represented.
     Used for packed arrays/records so variables enter the varMap even when
     element-level access will emit a more-specific abandon. *)
  VAR tinfo: Type.Info;  nb: INTEGER;
  BEGIN
    EVAL Type.CheckInfo(t, tinfo);
    nb := (tinfo.size + Target.Byte - 1) DIV Target.Byte;
    IF nb <= 0 THEN RETURN NIL END;
    RETURN MSIR.TFixedArray(nb, MSIR.TI1());
  END ByteArrayFallback;

PROCEDURE TranslateFixedArray(t: Type.T): MSIR.T =
  VAR
    indexT, eltT : Type.T;
    nElts        : INTEGER;
    eltMsir      : MSIR.T;
    eltPack      : INTEGER;
  BEGIN
    IF NOT ArrayType.Split(t, indexT, eltT) THEN RETURN NIL END;
    IF indexT = NIL THEN RETURN NIL END;  (* open: should not reach here *)
    IF NOT TInt.ToInt(Type.Number(indexT), nElts) THEN RETURN NIL END;
    IF ArrayType.EltsAreBitAddressed(t) THEN
      (* Sub-byte elements: fall back to [N x i8] so the variable enters the
         varMap; element-level access will emit a more-specific abandon. *)
      RETURN ByteArrayFallback(t);
    END;
    eltMsir := Translate(eltT);
    IF eltMsir = NIL THEN
      (* Element type (e.g. a packed sub-array) not directly translatable:
         fall back to [N x i8] for the same reason. *)
      RETURN ByteArrayFallback(t);
    END;
    eltPack := ArrayType.EltPack(t);
    IF eltPack > 0 AND MSIR.BitWidth(eltMsir) > 0
                   AND eltPack # MSIR.BitWidth(eltMsir) THEN
      (* Actual storage width differs from the natural expression type.
         Use the packed width so GEP strides match actual memory layout.
         This covers BITS-annotated packed types AND compact subranges like
         [0..255] (which CM3 stores as a byte without an explicit BITS annotation)
         and BOOLEAN (enum size=8 stored in a byte, but Translate gives i1).
         Guard: only override scalar element types (BitWidth > 0).  For aggregate
         element types (FixedArray, GcRef, …) BitWidth = -1; their natural MSIR
         size already matches EltPack, so no override is needed — and replacing
         them with TI(eltPack) destroys type info needed for nested subscripts. *)
      eltMsir := MSIR.TI(eltPack);
    END;
    RETURN MSIR.TFixedArray(nElts, eltMsir);
  END TranslateFixedArray;

PROCEDURE TranslateResult(t: Type.T): MSIR.T =
  BEGIN
    IF t = NIL THEN RETURN MSIR.TVoid() END;
    RETURN Translate(t);
  END TranslateResult;

BEGIN
END MSIRType.
