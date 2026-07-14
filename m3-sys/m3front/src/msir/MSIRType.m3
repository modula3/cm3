MODULE MSIRType;

IMPORT MSIR, Type, Int, LInt, Bool, Target;
IMPORT Addr, Reff, Charr, WCharr, Reel, LReel, EReel;
IMPORT RecordType, Field, M3ID, Value, Text, RefType, ArrayType, TInt;
IMPORT EnumType, ObjectType;

(* Per-module translation cache: maps base Type.T pointer → MSIR.T.
   Ensures repeated calls for the same M3 type return the same MSIR.T
   object so that pointer-identity checks in MSIRToLLVM dedup correctly. *)
CONST MaxTypeCache = 512;
TYPE CacheEntry = RECORD key: Type.T;  val: MSIR.T END;
VAR cache : ARRAY [0..MaxTypeCache-1] OF CacheEntry;
    cacheN: INTEGER := 0;

PROCEDURE Reset() =
  BEGIN cacheN := 0 END Reset;

PROCEDURE StripModulePrefix(raw: TEXT): TEXT =
  (* Given "Module__Name", return "Name".  Returns raw unchanged if no "__". *)
  VAR n := Text.Length(raw);  i := 0;
  BEGIN
    WHILE i + 1 < n DO
      IF Text.GetChar(raw, i) = '_'
         AND Text.GetChar(raw, i + 1) = '_' THEN
        RETURN Text.Sub(raw, i + 2);
      END;
      INC(i);
    END;
    RETURN raw;
  END StripModulePrefix;

PROCEDURE ObjectShortName(t: Type.T;  hint: TEXT): TEXT =
  (* Returns the user-visible short name for an object base type.
     Uses hint if non-empty, otherwise falls back to ObjectType.UserName.
     ObjectType.UserName uses GlobalName(dots=TRUE,with_module=TRUE) which
     returns "Module.Name"; strip the "Module." prefix at the last dot. *)
  VAR raw: TEXT;  n: INTEGER;  last: INTEGER;
  BEGIN
    IF hint # NIL AND Text.Length(hint) > 0 THEN RETURN hint END;
    raw := ObjectType.UserName(t);
    IF raw = NIL THEN RETURN "" END;
    (* Find the last '.' and return everything after it. *)
    n := Text.Length(raw);
    last := -1;
    FOR i := 0 TO n - 1 DO
      IF Text.GetChar(raw, i) = '.' THEN last := i END;
    END;
    IF last >= 0 THEN RETURN Text.Sub(raw, last + 1) END;
    RETURN raw;
  END ObjectShortName;

PROCEDURE Translate(t: Type.T): MSIR.T =
  VAR base: Type.T;  info: Type.Info;  nameId: M3ID.T;  typeName: TEXT;
      origInfo: Type.Info;
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    (* Capture the original type's size BEFORE stripping Named/Packed/Subrange.
       Subranges of Int.T like Ctypes.int (32-bit) must use their own size, not
       Target.Integer.size (64-bit), so that LOOPHOLE and field accesses emit the
       correct bitwidth. *)
    EVAL Type.CheckInfo(t, origInfo);
    (* Capture the human-readable name BEFORE stripping the Named wrapper.
       Type.Typename returns "Module__Name"; strip the "Module__" prefix. *)
    Type.Typename(t, nameId);
    IF nameId = M3ID.NoID
      THEN typeName := ""
      ELSE typeName := StripModulePrefix(M3ID.ToText(nameId))
    END;
    base := Type.Base(t);  (* strips Named, Packed, Subrange layers *)

    (* Integer-like builtins — use the original type's size so that narrow
       subranges of INTEGER (e.g. Ctypes.int = 32 bits) get TI(32) not TI(64).
       For non-negative subranges (CARDINAL, [0..N], CHAR, Word-family), emit
       TW (unsigned word) so that CoerceToMSIR can distinguish signed widening
       (SExt) from unsigned widening (ZExt) purely from the MSIR kind, without
       needing M3 type bounds at widening time.  M3 subranges with lo ≥ 0 are
       semantically unsigned even when stored in a signed INTEGER representation. *)
    (* Integer-like builtins — use the original type's size so that narrow
       subranges of INTEGER (e.g. Ctypes.int = 32 bits) get TI(32) not TI(64).
       Non-negative subranges (CARDINAL, [0..N]) use TW (unsigned word) so that
       CoerceToMSIR can use ZExt (correct for unsigned) purely from the MSIR kind,
       rather than needing to consult M3 type bounds at widening time.
       Word.T = INTEGER at 64-bit → no widening needed (both are full word size). *)
    IF base = Int.T OR base = LInt.T THEN
      VAR sz : INTEGER;
          lo, hi: Target.Int;
      BEGIN
        IF base = Int.T THEN sz := Target.Integer.size ELSE sz := Target.Longint.size END;
        IF origInfo.size > 0 THEN sz := origInfo.size END;
        IF (t # base)           (* it's a subrange, not the raw INTEGER type *)
           AND Type.GetBounds(t, lo, hi)
           AND TInt.LE(TInt.Zero, lo) THEN
          (* Non-negative subrange → unsigned word type (mirrors CG Word8/Word16/...) *)
          IF sz <= 8    THEN RETURN MSIR.TW(8)  END;
          IF sz <= 16   THEN RETURN MSIR.TW(16) END;
          IF sz <= 32   THEN RETURN MSIR.TW(32) END;
          RETURN MSIR.TW(64);
        END;
        RETURN MSIR.TI(sz);
      END;
    END;
    IF base = Bool.T  THEN RETURN MSIR.TI1() END;  (* BOOLEAN: I1, not W1 — no W1 kind exists *)
    IF base = Charr.T THEN RETURN MSIR.TW(Target.Char.size) END; (* CHAR unsigned *)
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
        RETURN TranslateEnum(base, typeName, info.size);

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
        (* Build a typed object ref so DWARF can show fields.
           typeName may be empty when the base type has no Named wrapper;
           fall back to the user_name recorded by NoteRefName. *)
        VAR mt := TranslateObject(base, ObjectShortName(base, typeName));
        BEGIN
          IF mt = NIL THEN
            IF info.isTraced THEN RETURN MSIR.TGcRef(MSIR.TVoid())
            ELSE RETURN MSIR.TPtr(MSIR.TVoid())
            END;
          END;
          IF info.isTraced THEN RETURN MSIR.TGcRef(mt)
          ELSE RETURN MSIR.TPtr(mt)
          END;
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

PROCEDURE TranslateObject(t: Type.T;  name: TEXT): MSIR.T =
  (* Build MSIR.TObject for an OBJECT or Opaque type, with own fields at their
     absolute bit offsets.  Returns NIL if the type is opaque or offsets unknown.
     Pre-registers a NIL sentinel in the cache before recursing to break cycles
     (e.g. TYPE T = OBJECT next: T END). *)
  VAR
    flds     : Value.T;
    finfo    : Field.Info;
    n        : INTEGER := 0;
    bitOff   : INTEGER;
    align    : INTEGER;
    v        : Value.T;
    super    : Type.T;
    superM   : MSIR.T := NIL;
    ft       : MSIR.T;
    fi       : INTEGER;
    selfIdx  : INTEGER := -1;
  BEGIN
    FOR k := 0 TO cacheN - 1 DO
      IF cache[k].key = t THEN RETURN cache[k].val END;
    END;
    ObjectType.GetFieldsOffsetAndAlign(t, bitOff, align);
    IF bitOff < 0 THEN RETURN NIL END;  (* opaque — offsets unknown *)
    (* Pre-register NIL to break self-referential cycles. *)
    IF cacheN < MaxTypeCache THEN
      selfIdx := cacheN;
      cache[cacheN].key := t;
      cache[cacheN].val := NIL;
      INC(cacheN);
    END;
    flds := ObjectType.FieldList(t);
    v := flds;
    WHILE v # NIL DO INC(n);  v := v.next END;
    (* Translate super type recursively, extracting its user-visible name
       so the MSIR.TObject gets a proper name regardless of call order. *)
    super := ObjectType.Super(t);
    IF super # NIL THEN
      superM := TranslateObject(super, ObjectShortName(super, ""));
    END;
    (* Build own-field array with absolute bit offsets. *)
    VAR msirFs := NEW(REF ARRAY OF MSIR.Field, n);
    BEGIN
      fi := 0;
      v  := flds;
      WHILE v # NIL DO
        Field.Split(v, finfo);
        ft := Translate(finfo.type);
        IF ft = NIL THEN ft := MSIR.TPtr(MSIR.TVoid()) END;  (* opaque / cycle *)
        msirFs[fi].name   := M3ID.ToText(finfo.name);
        IF msirFs[fi].name = NIL THEN msirFs[fi].name := "" END;
        msirFs[fi].type   := ft;
        msirFs[fi].offset := bitOff + finfo.offset;
        INC(fi);
        v := v.next;
      END;
      VAR objName : TEXT;
          result  : MSIR.T;
      BEGIN
        IF name = NIL OR Text.Length(name) = 0
          THEN objName := Type.Name(t)
          ELSE objName := name
        END;
        IF objName = NIL THEN objName := "" END;
        result := MSIR.TObject(
                    objName,
                    superM, msirFs^, ARRAY OF MSIR.Method{}, "");
        IF selfIdx >= 0 THEN
          cache[selfIdx].val := result;  (* update sentinel with real type *)
        END;
        RETURN result;
      END;
    END;
  END TranslateObject;

PROCEDURE TranslateEnum(t: Type.T;  name: TEXT;  bits: INTEGER): MSIR.T =
  (* VAR inits that call into the type system are deferred until after the
     NIL guard and cache check so that a NIL or cache-hit t does not crash
     before we have a chance to handle it. *)
  VAR n      : INTEGER;
      ls     : REF ARRAY OF TEXT;
      v      : Value.T;
      i      : INTEGER;
      result : MSIR.T;
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    FOR k := 0 TO cacheN - 1 DO
      IF cache[k].key = t THEN RETURN cache[k].val END;
    END;
    n  := EnumType.NumElts(t);
    ls := NEW(REF ARRAY OF TEXT, MAX(n, 0));
    i  := 0;
    v  := EnumType.EltList(t);
    WHILE v # NIL AND i < n DO
      ls[i] := M3ID.ToText(Value.CName(v));
      v := v.next;  INC(i);
    END;
    result := MSIR.TEnum(name, bits, ls^);
    IF cacheN < MaxTypeCache THEN
      cache[cacheN].key := t;
      cache[cacheN].val := result;
      INC(cacheN);
    END;
    RETURN result;
  END TranslateEnum;

PROCEDURE TranslateRecord(t: Type.T;  name: TEXT): MSIR.T =
  VAR
    fields: Value.T;
    n:      INTEGER := 0;
    v:      Value.T;
    finfo:  Field.Info;
    fti:    Type.Info;
  BEGIN
    FOR k := 0 TO cacheN - 1 DO
      IF cache[k].key = t THEN RETURN cache[k].val END;
    END;
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
          msirFields[i].name   := M3ID.ToText(finfo.name);
          IF msirFields[i].name = NIL THEN msirFields[i].name := "" END;
          msirFields[i].type   := ft;
          msirFields[i].offset := finfo.offset; (* bit offset for DWARF *)
        END;
        v := v.next;
      END;
      VAR result := MSIR.TStruct(name, msirFields^);
      BEGIN
        MSIR.SetTypeUID(result, Type.GlobalUID(t));
        IF cacheN < MaxTypeCache THEN
          cache[cacheN].key := t;  cache[cacheN].val := result;
          INC(cacheN);
        END;
        RETURN result;
      END;
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
     element-level access will emit a more-specific abandon.
     INVARIANT: callers detect ByteArrayFallback outputs by checking
     ArrayType.EltsAreBitAddressed on the M3 source type, NOT by inspecting
     the MSIR element kind (I8), because ARRAY OF BYTE-SIZED-T also produces
     [N x i8] naturally (e.g. ARRAY OF CHAR → [N x w8], ARRAY OF BYTE → [N x i8]
     after eltPack override in TranslateFixedArray).  The M3 predicate is the
     only reliable discriminant.  See SubscriptExpr.LValueMSIR for usage. *)
  VAR tinfo: Type.Info;  nb: INTEGER;
  BEGIN
    EVAL Type.CheckInfo(t, tinfo);
    nb := (tinfo.size + Target.Byte - 1) DIV Target.Byte;
    IF nb <= 0 THEN RETURN NIL END;
    RETURN MSIR.TFixedArray(nb, MSIR.TI(Target.Byte));
  END ByteArrayFallback;

PROCEDURE TranslateFixedArray(t: Type.T): MSIR.T =
  VAR
    indexT, eltT : Type.T;
    nElts        : INTEGER;
    eltMsir      : MSIR.T;
    eltPack      : INTEGER;
    lo, hi       : Target.Int;
    loI          : INTEGER;
  BEGIN
    FOR k := 0 TO cacheN - 1 DO
      IF cache[k].key = t THEN RETURN cache[k].val END;
    END;
    IF NOT ArrayType.Split(t, indexT, eltT) THEN RETURN NIL END;
    IF indexT = NIL THEN RETURN NIL END;  (* open: should not reach here *)
    IF NOT TInt.ToInt(Type.Number(indexT), nElts) THEN RETURN NIL END;
    loI := 0;
    IF Type.GetBounds(indexT, lo, hi) THEN
      IF NOT TInt.ToInt(lo, loI) THEN loI := 0 END;
    END;
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
    VAR result := MSIR.TFixedArray(nElts, eltMsir);
    BEGIN
      MSIR.SetTypeUID(result, Type.GlobalUID(t));
      IF loI # 0 THEN MSIR.SetFixedArrayLo(result, loI) END;
      IF cacheN < MaxTypeCache THEN
        cache[cacheN].key := t;  cache[cacheN].val := result;
        INC(cacheN);
      END;
      RETURN result;
    END;
  END TranslateFixedArray;

PROCEDURE TranslateResult(t: Type.T): MSIR.T =
  BEGIN
    IF t = NIL THEN RETURN MSIR.TVoid() END;
    (* Ordinal result types (BOOLEAN, CHAR, enumerations, subranges) use the
       computation width i64, matching loads, alloca slots, and formal params.
       Without this, an enum proc like Stack_Get: ST = {...} would have result
       type TEnum (kind Enum > W64) and ReturnStmt's i64→narrow coercions
       wouldn't fire — triggering "return type mismatch not yet supported". *)
    IF Type.IsOrdinal(t) THEN RETURN MSIR.TI(Target.Integer.size) END;
    RETURN Translate(t);
  END TranslateResult;

BEGIN
END MSIRType.
