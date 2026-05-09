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

    | Type.Class.Array =>
        RETURN TranslateFixedArray(base);

    | Type.Class.OpenArray =>
        RETURN TranslateOpenArray(base);

    | Type.Class.Object, Type.Class.Opaque =>
        (* Conservative: all object and opaque types are traced references.
           Vtable-aware typed descriptors are future work. *)
        RETURN MSIR.TGcRef(MSIR.TVoid());

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
  BEGIN
    IF NOT RecordType.Split(t, fields) THEN RETURN NIL END;
    v := fields;
    WHILE v # NIL DO INC(n);  v := v.next END;
    VAR msirFields := NEW(REF ARRAY OF MSIR.Field, n);
    BEGIN
      v := fields;
      FOR i := 0 TO n - 1 DO
        Field.Split(v, finfo);
        VAR ft := Translate(finfo.type);
        BEGIN
          IF ft = NIL THEN RETURN NIL END;
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

PROCEDURE TranslateFixedArray(t: Type.T): MSIR.T =
  VAR
    indexT, eltT: Type.T;
    nElts:        INTEGER;
    eltMsir:      MSIR.T;
  BEGIN
    IF NOT ArrayType.Split(t, indexT, eltT) THEN RETURN NIL END;
    IF indexT = NIL THEN RETURN NIL END;  (* open: should not reach here *)
    eltMsir := Translate(eltT);
    IF eltMsir = NIL THEN RETURN NIL END;
    IF NOT TInt.ToInt(Type.Number(indexT), nElts) THEN RETURN NIL END;
    RETURN MSIR.TFixedArray(VAL(nElts, LONGINT), eltMsir);
  END TranslateFixedArray;

PROCEDURE TranslateResult(t: Type.T): MSIR.T =
  BEGIN
    IF t = NIL THEN RETURN MSIR.TVoid() END;
    RETURN Translate(t);
  END TranslateResult;

BEGIN
END MSIRType.
