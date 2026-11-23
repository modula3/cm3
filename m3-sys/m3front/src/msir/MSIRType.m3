MODULE MSIRType;

IMPORT MSIR, Type, Int, LInt, Bool, Target;
IMPORT Addr, Reff, Charr, WCharr, Reel, LReel, EReel;
IMPORT RecordType, Field, M3ID, Value, Text, RefType, ArrayType, TInt;
IMPORT OpenArrayType;
IMPORT EnumType, ObjectType, Fmt, NamedType;

(* Per-module translation cache: maps base Type.T pointer → MSIR.T.
   Ensures repeated calls for the same M3 type return the same MSIR.T
   object so that pointer-identity checks in MSIRToLLVM dedup correctly. *)
CONST MaxTypeCache = 512;
TYPE CacheEntry = RECORD key: Type.T;  val: MSIR.T END;
VAR cache : ARRAY [0..MaxTypeCache-1] OF CacheEntry;
    cacheN: INTEGER := 0;

(* Per-module debug-type cache: Type.T -> MSIR.DbgT. *)
CONST MaxDbgCache = 1024;
TYPE DbgCacheEntry = RECORD key: Type.T;  val: MSIR.DbgT END;
VAR dcache : ARRAY [0..MaxDbgCache-1] OF DbgCacheEntry;
    dcacheN: INTEGER := 0;

PROCEDURE Reset() =
  BEGIN cacheN := 0;  dcacheN := 0 END Reset;

PROCEDURE ObjectName(t: Type.T;  hint: TEXT): TEXT =
  (* The qualified name of an object base type ("Module.Name", as its
     typecell names it): hint if non-empty, otherwise ObjectType.UserName. *)
  VAR raw: TEXT;
  BEGIN
    IF hint # NIL AND Text.Length(hint) > 0 THEN RETURN hint END;
    raw := ObjectType.UserName(t);
    IF raw = NIL THEN RETURN "" END;
    RETURN raw;
  END ObjectName;

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
       Type.Typename returns "Module__Name"; make it "Module.Name". *)
    Type.Typename(t, nameId);
    IF nameId = M3ID.NoID
      THEN typeName := ""
      ELSE typeName := Dotted(M3ID.ToText(nameId))
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
        VAR mt := TranslateObject(base, ObjectName(base, typeName));
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
      superM := TranslateObject(super, ObjectName(super, ""));
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
  (* FLAT rank-N model, matching the M3 runtime dope contract: an open array of
     M3 open depth N is ONE fat pointer { data, n0, .., n_{N-1} } over flat
     row-major data whose element is the nearest NON-open element type.  The old
     per-level nesting (openarray<1> openarray<1> elt) implied the outer data
     region held inner dope structs — no M3 producer (heap NEW, fixed->open
     conversion, VALUE copies) builds that, so depth>=2 readers dereferenced
     flat data bytes as dope pointers (p277 OArrsInOArr SEGV, p269 D3..D6). *)
  VAR
    depth   := OpenArrayType.OpenDepth(t);
    eltT    := OpenArrayType.NonopenEltType(t);
    eltMsir : MSIR.T;
    eltPack : INTEGER;
  BEGIN
    IF depth <= 0 THEN RETURN NIL END;
    eltMsir := Translate(eltT);
    IF eltMsir = NIL THEN RETURN NIL END;
    (* Element storage-width override, mirroring TranslateFixedArray: when the
       packed element width differs from the natural scalar width AND is a whole
       number of bytes, use TI(eltPack) so elem_addr GEP strides match memory
       (e.g. BITS 16 FOR [0..N] stored 2 bytes, natural i64).  Sub-byte packs
       keep the nominal element type — bit-addressed access goes through
       Extract/InsertBitFieldDyn which ignores the element type entirely. *)
    eltPack := OpenArrayType.EltPack(t);
    IF eltPack >= Target.Byte AND eltPack MOD Target.Byte = 0
       AND MSIR.BitWidth(eltMsir) > 0 AND eltPack # MSIR.BitWidth(eltMsir) THEN
      eltMsir := MSIR.TI(eltPack);
    END;
    RETURN MSIR.TOpenArray(depth, eltMsir);
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

PROCEDURE ComputeType(t: Type.T): MSIR.T =
  (* ZType: machine INTEGER width for any ordinal (BOOLEAN, CHAR, WIDECHAR,
     enumerations, subranges); Translate(t) otherwise.  See MSIRType.i3. *)
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    IF Type.IsOrdinal(t) THEN RETURN MSIR.TI(Target.Integer.size) END;
    RETURN Translate(t);
  END ComputeType;

PROCEDURE TranslateResult(t: Type.T): MSIR.T =
  BEGIN
    IF t = NIL THEN RETURN MSIR.TVoid() END;
    (* CG-faithful: a procedure result is the type's NATURAL (MType) width with
       signedness, exactly like CG's ProcType.CGResult = CGType(in_memory).  The
       callee's ReturnStmt narrows the i64 (ZType) computation value to this
       width (its Trunc branch), and every call site widens the result back to
       ZType (CoerceToMSIR, SExt/ZExt by the MType kind) — the same MType<->ZType
       discipline as memory stores/loads.  This is required for correctness of
       calls to <*EXTERNAL*> C functions returning sub-word ints (int8_t etc.),
       whose ABI returns only the natural width; declaring the call i64 read
       garbage upper bits (m3tests p228).
       Translate already encodes signedness (signed subrange -> TI, non-negative
       -> TW).  An enum yields TEnum (kind Enum, outside I1..W64), which would
       stop ReturnStmt's narrow branch from firing — so map it to a same-width
       unsigned TW (enum values are 0..n-1). *)
    IF Type.IsOrdinal(t) THEN
      VAR mt := Translate(t);
      BEGIN
        IF mt # NIL AND MSIR.Kind(mt) = MSIR.TypeKind.Enum THEN
          mt := MSIR.TW(MSIR.BitWidth(mt));
        END;
        RETURN mt;
      END;
    END;
    RETURN Translate(t);
  END TranslateResult;

(*------------------------------------------------------------ debug types *)

PROCEDURE DbgCached (t: Type.T): MSIR.DbgT =
  BEGIN
    FOR k := 0 TO dcacheN - 1 DO
      IF dcache[k].key = t THEN RETURN dcache[k].val END;
    END;
    RETURN NIL;
  END DbgCached;

PROCEDURE DbgRemember (t: Type.T;  d: MSIR.DbgT) =
  BEGIN
    IF dcacheN < MaxDbgCache THEN
      dcache[dcacheN].key := t;  dcache[dcacheN].val := d;  INC(dcacheN);
    END;
  END DbgRemember;

(* A translation already made for t or its base, or NIL: never a new one,
   so that describing a type has no side effects. *)
PROCEDURE TranslatedAlready (t: Type.T): MSIR.T =
  VAR b := Type.Base(t);
  BEGIN
    FOR k := 0 TO cacheN - 1 DO
      IF cache[k].key = t OR cache[k].key = b THEN RETURN cache[k].val END;
    END;
    RETURN NIL;
  END TranslatedAlready;

PROCEDURE DbgObject (t: Type.T): MSIR.DbgT =
  (* A reference to the structure: the lowering builds an object's
     structure as the referent of a reference. *)
  VAR r := Translate(t);
  BEGIN
    IF r # NIL AND MSIR.Kind(r) IN SET OF MSIR.TypeKind{MSIR.TypeKind.Ptr,
                                                       MSIR.TypeKind.GcRef} THEN
      r := MSIR.EltType(r);
    END;
    IF r = NIL OR MSIR.Kind(r) # MSIR.TypeKind.Object THEN RETURN NIL END;
    RETURN MSIR.DbgFromType(MSIR.TGcRef(r));
  END DbgObject;

PROCEDURE NameOf (t: Type.T;  default: TEXT): TEXT =
  (* The qualified name a user's named type was declared with
     ("Main.T", as its typecell names it, so a debugger can map one to
     the other), or a builtin's, which Tipe.Define records in the type
     itself.  (For user types that record is made only when the target
     asks for type names, as the C backend does, so they are named from
     the NamedType node.) *)
  VAR v: Value.T;  id: M3ID.T;
  BEGIN
    IF NamedType.SplitV(t, v) AND v # NIL THEN
      RETURN Value.GlobalName(v, dots := TRUE);
    END;
    Type.Typename(t, id);
    IF id # M3ID.NoID THEN RETURN Dotted(M3ID.ToText(id)) END;
    RETURN default;
  END NameOf;

PROCEDURE Dotted (raw: TEXT): TEXT =
  (* "Module__Name" as "Module.Name" *)
  VAR i := 0;  n := Text.Length(raw);
  BEGIN
    WHILE i + 1 < n DO
      IF Text.GetChar(raw, i) = '_' AND Text.GetChar(raw, i + 1) = '_' THEN
        RETURN Text.Sub(raw, 0, i) & "." & Text.Sub(raw, i + 2);
      END;
      INC(i);
    END;
    RETURN raw;
  END Dotted;

PROCEDURE DbgType(t: Type.T): MSIR.DbgT =
  VAR d: MSIR.DbgT;  u, b: Type.T;  info: Type.Info;  name: TEXT;
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    d := DbgCached(t);
    IF d # NIL THEN RETURN d END;
    u := Type.StripPacked(t);     (* renaming and packing, not subranges *)
    u := Type.CheckInfo(u, info);
    name := NameOf(t, NIL);
    IF info.class = Type.Class.Record OR info.class = Type.Class.Enum THEN
      (* One description however the type is reached, named or not; a name
         seen on any route is kept. *)
      d := DbgCached(u);
      IF d # NIL THEN
        IF name # NIL AND Text.Length(MSIR.DbgName(d)) = 0 THEN
          MSIR.DbgSetName(d, name);
        END;
        DbgRemember(t, d);
        RETURN d;
      END;
    END;
    CASE info.class OF
    | Type.Class.Integer =>
        d := MSIR.DbgBasic(Pick(name, "INTEGER"), MSIR.DbgEncoding.Signed, info.size);
    | Type.Class.Longint =>
        d := MSIR.DbgBasic(Pick(name, "LONGINT"), MSIR.DbgEncoding.Signed, info.size);
    | Type.Class.Real =>
        d := MSIR.DbgBasic(Pick(name, "REAL"), MSIR.DbgEncoding.Float, info.size);
    | Type.Class.Longreal =>
        d := MSIR.DbgBasic(Pick(name, "LONGREAL"), MSIR.DbgEncoding.Float, info.size);
    | Type.Class.Extended =>
        d := MSIR.DbgBasic(Pick(name, "EXTENDED"), MSIR.DbgEncoding.Float, info.size);
    | Type.Class.Subrange =>
        VAR lo, hi: Target.Int;  ok := Type.GetBounds(u, lo, hi);
            enc := MSIR.DbgEncoding.Signed;
        BEGIN
          b := Type.Base(u);
          IF ok AND TInt.LE(TInt.Zero, lo) THEN enc := MSIR.DbgEncoding.Unsigned END;
          IF b = Int.T OR b = LInt.T THEN
            d := MSIR.DbgBasic(Pick(name, Bounds(ok, lo, hi)), enc, info.size);
          ELSE  (* a subrange of an enumeration or CHAR *)
            d := MSIR.DbgAlias(Pick(name, Bounds(ok, lo, hi)), DbgType(b));
          END;
        END;
    | Type.Class.Enum =>
        IF u = Bool.T THEN
          d := MSIR.DbgBasic("BOOLEAN", MSIR.DbgEncoding.Boolean, info.size);
        ELSIF u = Charr.T THEN
          d := MSIR.DbgBasic("CHAR", MSIR.DbgEncoding.Char, info.size);
        ELSIF u = WCharr.T THEN
          d := MSIR.DbgBasic("WIDECHAR", MSIR.DbgEncoding.Unsigned, info.size);
        ELSE
          VAR n := EnumType.NumElts(u);
              ls := NEW(REF ARRAY OF TEXT, MAX(n, 0));
              v := EnumType.EltList(u);
              i := 0;
          BEGIN
            WHILE v # NIL AND i < n DO
              ls[i] := M3ID.ToText(Value.CName(v));  v := v.next;  INC(i);
            END;
            d := MSIR.DbgEnum(Pick(name, ""), info.size, ls^);
            IF u # t THEN DbgRemember(u, d) END;
          END;
        END;
    | Type.Class.Record =>
        VAR fields: Value.T;  v: Value.T;  n := 0;  finfo: Field.Info;
            fti: Type.Info;
        BEGIN
          d := MSIR.DbgStruct(Pick(name, ""), info.size);
          DbgRemember(t, d);       (* before the fields: recursive types *)
          IF u # t THEN DbgRemember(u, d) END;
          IF RecordType.Split(u, fields) THEN
            v := fields;
            WHILE v # NIL DO INC(n);  v := v.next END;
            VAR fs := NEW(REF ARRAY OF MSIR.DbgField, n);
            BEGIN
              v := fields;
              FOR i := 0 TO n - 1 DO
                Field.Split(v, finfo);
                EVAL Type.CheckInfo(finfo.type, fti);
                fs[i].name := M3ID.ToText(finfo.name);
                IF fs[i].name = NIL THEN fs[i].name := "" END;
                fs[i].type := DbgType(finfo.type);
                fs[i].bitOffset := finfo.offset;
                fs[i].bitSize := fti.size;
                v := v.next;
              END;
              MSIR.DbgStructSetFields(d, fs^);
            END;
          END;
          NoteUID(d, t);
          RETURN d;
        END;
    | Type.Class.Array =>
        VAR index, elt: Type.T;  lo, hi: Target.Int;  loI, n: INTEGER;
        BEGIN
          IF ArrayType.Split(u, index, elt) AND index # NIL
             AND Type.GetBounds(index, lo, hi)
             AND TInt.ToInt(lo, loI) AND TInt.ToInt(Type.Number(index), n) THEN
            d := MSIR.DbgArray(DbgType(elt), loI, n);
            IF name # NIL THEN d := MSIR.DbgAlias(name, d) END;
          ELSE
            d := MSIR.DbgPointer(Pick(name, ""), NIL);
          END;
        END;
    | Type.Class.OpenArray =>
        (* the lowering's dope type needs only the rank *)
        d := MSIR.DbgFromType(MSIR.TOpenArray(OpenArrayType.OpenDepth(u),
                                              MSIR.TVoid()));
    | Type.Class.Object =>
        (* a reference to the object type's translation, if there is one *)
        VAR mt := TranslatedAlready(u);
        BEGIN
          IF mt # NIL AND MSIR.Kind(mt) = MSIR.TypeKind.Object THEN
            d := MSIR.DbgFromType(MSIR.TGcRef(mt));
          ELSIF mt # NIL THEN
            d := MSIR.DbgFromType(mt);
          ELSE
            d := MSIR.DbgPointer(Pick(name, ""), NIL);
          END;
        END;
    | Type.Class.Ref =>
        VAR target: Type.T;
        BEGIN
          d := MSIR.DbgPointer(Pick(name, ""), NIL);
          DbgRemember(t, d);       (* before the target: recursive types *)
          IF RefType.Split(u, target) AND target # NIL THEN
            MSIR.DbgSetBase(d, DbgType(target));
          END;
          NoteUID(d, t);
          RETURN d;
        END;
    | Type.Class.Opaque =>
        (* Its revelation is not looked up: that would mark it used.  A
           typedef keeps its name visible to debuggers, which ignore a name
           on a pointer type.  An alias of a builtin (Text.T) takes the
           builtin's name, TEXT, which the debugger shims recognise. *)
        VAR id: M3ID.T;
        BEGIN
          (* a builtin's own name (TEXT for Text.T) *)
          Type.Typename(u, id);
          IF id # M3ID.NoID THEN name := Dotted(M3ID.ToText(id)) END;
        END;
        d := MSIR.DbgPointer("", NIL);
        IF name # NIL THEN d := MSIR.DbgAlias(name, d) END;
    | Type.Class.Procedure =>
        d := MSIR.DbgProc(Pick(name, ""));
    | Type.Class.Set =>
        IF info.size <= Target.Integer.size THEN
          d := MSIR.DbgBasic(Pick(name, "SET"), MSIR.DbgEncoding.Unsigned, info.size);
        ELSE
          d := MSIR.DbgArray(MSIR.DbgBasic("SET", MSIR.DbgEncoding.Unsigned,
                                           Target.Byte),
                             0, info.size DIV Target.Byte);
          IF name # NIL THEN d := MSIR.DbgAlias(name, d) END;
        END;
    ELSE
        RETURN NIL;
    END;
    DbgRemember(t, d);
    NoteUID(d, t);
    RETURN d;
  END DbgType;

(* The UID of the type d describes, for the m3.uid annotation. *)
PROCEDURE NoteUID (d: MSIR.DbgT;  t: Type.T) =
  BEGIN
    IF d # NIL AND MSIR.DbgUID(d) = 0 THEN
      MSIR.DbgSetUID(d, Type.GlobalUID(t));
    END;
  END NoteUID;

PROCEDURE Pick (name, default: TEXT): TEXT =
  BEGIN
    IF name # NIL THEN RETURN name END;
    RETURN default;
  END Pick;

PROCEDURE Bounds (ok: BOOLEAN;  READONLY lo, hi: Target.Int): TEXT =
  VAR l, h: INTEGER;
  BEGIN
    IF ok AND TInt.ToInt(lo, l) AND TInt.ToInt(hi, h) THEN
      RETURN "[" & Fmt.Int(l) & ".." & Fmt.Int(h) & "]";
    END;
    RETURN "INTEGER";
  END Bounds;

BEGIN
END MSIRType.
