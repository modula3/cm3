MODULE MSIRToLLVM;

IMPORT MSIR, Wr, Fmt, Thread, Text, RefSeq, TextWr, Word;
IMPORT Target, TFloat, M3RT;
<*FATAL Thread.Alerted, Wr.Failure*>

(* RT0.RefHeaderBits bit-field offsets — derived from the BITS declarations:
     forwarded : BITS 1; typecode : BITS 20; dirty : BITS 1; gray : BITS 1 *)
CONST
  RH_typecode_offset = 1;           (* after 1-bit forwarded *)
  RH_dirty_offset    = RH_typecode_offset + 20;
  RH_gray_offset     = RH_dirty_offset + 1;

(* TEXT is always typecode 1: it is the first type registered at runtime. *)
CONST TEXT_typecode = 1;

(*----------------------------------------------------- module-level state *)

TYPE TCEntry = REF RECORD
  name : TEXT;                   (* "@tc.table.N" *)
  uids : REF ARRAY OF INTEGER;   (* type UIDs; last entry is 0 (ELSE) *)
END;

(*------------------------------------------------------ debug info state *)

CONST MaxDbgFiles    = 64;
CONST MaxDbgEntries  = 2048;
CONST MaxDbgVars     = 4096;
CONST MaxDbgLocs     = 16384;
CONST MaxDbgTypes    = 1024;
CONST MaxDbgChildren = 8192;
CONST NBT            = 9;    (* number of fixed DIBasicType nodes emitted *)

TYPE DbgFileEntry = RECORD name: TEXT; metaIdx: INTEGER END;
TYPE DbgLocEntry  = RECORD spIdx, line, metaIdx: INTEGER END;
TYPE DbgEntry     = RECORD
  proc:    MSIR.Proc;
  metaIdx: INTEGER;    (* !DISubprogram index *)
  locIdx:  INTEGER;    (* !DILocation(line:0, scope:subprogram) index *)
END;
TYPE DbgVarEntry  = RECORD
  allocaVal:   MSIR.Value;  (* alloca result or (isParam) param value *)
  varName:     TEXT;        (* display name (stripped of % and .slot) *)
  spIdx:       INTEGER;     (* DISubprogram metadata index for scope *)
  fileIdx:     INTEGER;     (* DIFile metadata index *)
  line:        INTEGER;     (* declaration line *)
  typeMetaIdx: INTEGER;     (* DWARF type metadata index (-1 to skip) *)
  metaIdx:     INTEGER;     (* !DILocalVariable metadata index *)
  isParam:     BOOLEAN;     (* TRUE => allocaVal is a param value, not alloca *)
END;
(* Phase 4: composite type (Struct / FixedArray) DWARF nodes. *)
TYPE DbgTypeEntry = RECORD
  msirType:      MSIR.T;    (* pointer identity key *)
  metaIdx:       INTEGER;   (* !DICompositeType node index *)
  elemsTupleIdx: INTEGER;   (* !{child1, child2, ...} tuple node index *)
  baseTypeRef:   INTEGER;   (* FixedArray: element type metaIdx; Struct: -1 *)
  kind:          INTEGER;   (* 0 = Struct, 1 = FixedArray, 2 = Enum *)
  childBase:     INTEGER;   (* first index in dbgChildren[] *)
  childCount:    INTEGER;
  totalBits:     INTEGER;   (* total size in bits *)
END;
TYPE DbgChildEntry = RECORD
  kind:    INTEGER;      (* 0 = DW_TAG_member, 1 = DISubrange, 2 = DIEnumerator *)
  (* member (kind=0): *)
  name:    TEXT;
  typeRef: INTEGER;      (* base type metadata index *)
  size:    INTEGER;      (* field size in bits *)
  offset:  INTEGER;      (* field bit offset in containing struct *)
  (* subrange (kind=1): *)
  count:   INTEGER;      (* FixedArray element count *)
  (* offset field (shared with kind=0) stores lowerBound for kind=1 *)
  (* enumerator (kind=2): *)
  value:   INTEGER;      (* ordinal value *)
  (* shared: *)
  metaIdx: INTEGER;
END;

VAR
  dbgFiles:      ARRAY [0..MaxDbgFiles-1]    OF DbgFileEntry;
  dbgFileN:      INTEGER := 0;
  dbgEntries:    ARRAY [0..MaxDbgEntries-1]  OF DbgEntry;
  dbgEntryN:     INTEGER := 0;
  dbgVars:       ARRAY [0..MaxDbgVars-1]     OF DbgVarEntry;
  dbgVarN:       INTEGER := 0;
  dbgLocs:       ARRAY [0..MaxDbgLocs-1]     OF DbgLocEntry;
  dbgLocN:       INTEGER := 0;
  dbgTypes:      ARRAY [0..MaxDbgTypes-1]    OF DbgTypeEntry;
  dbgTypeN:      INTEGER := 0;
  dbgChildren:   ARRAY [0..MaxDbgChildren-1] OF DbgChildEntry;
  dbgChildN:     INTEGER := 0;
  dbgEnabled:    BOOLEAN := FALSE;
  curDbgLocIdx:  INTEGER := -1;    (* line:0 loc index for current proc *)
  curEmitProc:   MSIR.Proc := NIL; (* proc being emitted — per-insn !dbg lookup *)
  (* pre-computed metadata indices set by BuildDebugInfo: *)
  dbgNsIdx:      INTEGER := -1;    (* DINamespace *)
  dbgNlIdx:      INTEGER := -1;    (* null-list !{null} *)
  dbgStIdx:      INTEGER := -1;    (* shared DISubroutineType *)
  dbgBtBase:     INTEGER := -1;    (* first DIBasicType (slots 0..NBT-1) *)

VAR
  auxN:          INTEGER     := 0;
  tcN:           INTEGER     := 0;   (* module-level counter for TYPECASE tables *)
  curEmitModule: MSIR.Module := NIL;
  pendingTC:     RefSeq.T    := NIL;   (* TCEntry list, built during EmitInsn *)

(*------------------------------------------------------ auxiliary naming *)

PROCEDURE NewAux(): TEXT =
  BEGIN INC(auxN); RETURN "%__ll" & Fmt.Int(auxN) END NewAux;

(* Return the LLVM symbol name for a module hook proc.
   If the proc was set via MSIR.SetModuleHooks (from RunTyme lookup in
   MSIREmit), use LLSymbol so the name comes from the actual interface.
   Falls back to the hardcoded name only when the proc is NIL. *)
PROCEDURE LLHookName(p: MSIR.Proc;  fallback: TEXT): TEXT =
  BEGIN
    IF p # NIL THEN RETURN LLSymbol(p) END;
    RETURN fallback;
  END LLHookName;

(* Render an MSIR type as TEXT (for use in alias declarations etc.) *)
PROCEDURE LLTypeStr(t: MSIR.T): TEXT =
  VAR wr := TextWr.New();
  BEGIN
    LLType(wr, t);
    RETURN TextWr.ToText(wr);
  END LLTypeStr;

(* Capture LLOpVal(v) as a TEXT for use in barrier string templates. *)
PROCEDURE LLOpValStr(v: MSIR.Value): TEXT =
  VAR wr2 := TextWr.New();
  BEGIN
    LLOpVal(wr2, v);
    RETURN TextWr.ToText(wr2);
  END LLOpValStr;

(*------------------------------------------------------ symbol mangling *)

(* TRUE if n contains the substring "__". *)
PROCEDURE ContainsDunder(n: TEXT): BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    IF n = NIL THEN RETURN FALSE END;
    i := Text.FindChar(n, '_');
    WHILE i >= 0 AND i + 1 < Text.Length(n) DO
      IF Text.GetChar(n, i + 1) = '_' THEN RETURN TRUE END;
      i := Text.FindChar(n, '_', i + 1);
    END;
    RETURN FALSE;
  END ContainsDunder;

(* Replace every '.' in n with '__' — M3 module-separator to C ABI. *)
PROCEDURE DotsToUnderscore(n: TEXT): TEXT =
  VAR result := "";  start, dot: INTEGER;
  BEGIN
    start := 0;
    LOOP
      dot := Text.FindChar(n, '.', start);
      IF dot < 0 THEN RETURN result & Text.Sub(n, start) END;
      result := result & Text.Sub(n, start, dot - start) & "__";
      start  := dot + 1;
    END;
  END DotsToUnderscore;

(* LLVM symbol name for a procedure:
   - module-internal: <Module>__<Proc>
   - M3 extern (name has '.'): replace dots with '__'
   - C extern (no dot): use as-is *)
PROCEDURE LLSymbol(p: MSIR.Proc): TEXT =
  VAR n := MSIR.ProcName(p);
  BEGIN
    IF n = NIL THEN n := "" END;
    IF IsModuleProc(curEmitModule, p) THEN
      (* Name may already carry a module/interface prefix (e.g.
         "ExprRep__DefaultCheckUseFailure" compiled in Expr.m3 which
         EXPORTS ExprRep).  Only prepend when the name is unqualified. *)
      IF ContainsDunder(n) THEN
        RETURN n;
      ELSE
        RETURN MSIR.ModuleName(curEmitModule) & "__" & n;
      END;
    ELSIF Text.FindChar(n, '.') >= 0 THEN
      (* Fully qualified M3 name ("Fmt.Int") → replace dots with __ *)
      RETURN DotsToUnderscore(n);
    ELSIF IsModuleProcByName(curEmitModule, n) THEN
      (* Extern stub whose scope name was empty: unqualified name matches a
         module proc — prepend the module prefix. *)
      IF ContainsDunder(n) THEN
        RETURN n;
      ELSE
        RETURN MSIR.ModuleName(curEmitModule) & "__" & n;
      END;
    ELSE
      RETURN n;  (* C extern or truly external M3 symbol *)
    END;
  END LLSymbol;

(*------------------------------------------------------- type emission *)

PROCEDURE LLType(wr: Wr.T;  t: MSIR.T) =
  BEGIN
    IF t = NIL THEN Wr.PutText(wr, "void"); RETURN END;
    CASE MSIR.Kind(t) OF
    | MSIR.TypeKind.Void   => Wr.PutText(wr, "void");
    | MSIR.TypeKind.I1     => Wr.PutText(wr, "i1");
    | MSIR.TypeKind.I8     => Wr.PutText(wr, "i8");
    | MSIR.TypeKind.I16    => Wr.PutText(wr, "i16");
    | MSIR.TypeKind.I32    => Wr.PutText(wr, "i32");
    | MSIR.TypeKind.I64    => Wr.PutText(wr, "i64");
    | MSIR.TypeKind.IWide  =>
        Wr.PutText(wr, "i");
        Wr.PutText(wr, Fmt.Int(MSIR.BitWidth(t)));
    | MSIR.TypeKind.W8     => Wr.PutText(wr, "i8");
    | MSIR.TypeKind.W16    => Wr.PutText(wr, "i16");
    | MSIR.TypeKind.W32    => Wr.PutText(wr, "i32");
    | MSIR.TypeKind.W64    => Wr.PutText(wr, "i64");
    | MSIR.TypeKind.F32    => Wr.PutText(wr, "float");
    | MSIR.TypeKind.F64    => Wr.PutText(wr, "double");
    | MSIR.TypeKind.F128   => Wr.PutText(wr, "fp128");
    | MSIR.TypeKind.Ptr,
      MSIR.TypeKind.GcRef,
      MSIR.TypeKind.GcSlot => Wr.PutText(wr, "ptr");
    | MSIR.TypeKind.Struct =>
        Wr.PutText(wr, "{ ");
        FOR i := 0 TO MSIR.StructFieldCount(t) - 1 DO
          IF i > 0 THEN Wr.PutText(wr, ", ") END;
          LLType(wr, MSIR.StructField(t, i).type);
        END;
        Wr.PutText(wr, " }");
    | MSIR.TypeKind.Object =>
        Wr.PutText(wr, "ptr");  (* opaque in LLVM v0 *)
    | MSIR.TypeKind.OpenArray =>
        Wr.PutText(wr, "{ ptr");
        FOR k := 0 TO MSIR.OpenArrayRank(t) - 1 DO
          Wr.PutText(wr, ", i" & Fmt.Int(Target.IntegerSize()));
        END;
        Wr.PutText(wr, " }");
    | MSIR.TypeKind.HeapArray =>
        Wr.PutText(wr, "ptr");  (* opaque heap-array dope pointer *)
    | MSIR.TypeKind.FixedArray =>
        Wr.PutText(wr, "[");
        Wr.PutText(wr, Fmt.Int(MSIR.FixedArrayLen(t)));
        Wr.PutText(wr, " x ");
        LLType(wr, MSIR.FixedArrayElt(t));
        Wr.PutText(wr, "]");
    | MSIR.TypeKind.Subrange =>
        LLType(wr, MSIR.SubrangeParent(t));
    | MSIR.TypeKind.Enum =>
        Wr.PutText(wr, "i");
        Wr.PutText(wr, Fmt.Int(MSIR.BitWidth(t)));
    | MSIR.TypeKind.ProcType =>
        Wr.PutText(wr, "ptr");  (* function pointer *)
    ELSE
        Wr.PutText(wr, "ptr");  (* fallback *)
    END;
  END LLType;

(*------------------------------------------------------- value emission *)

CONST HexDigit = ARRAY [0..15] OF CHAR {
  '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f' };

(* Emit a float constant as LLVM hex.
   LLVM always requires 0x + 16 hex chars for any FP type.
   For float (32-bit): 16 hex chars must represent the VALUE as a 64-bit
   IEEE 754 double (widened from float32 — not the raw float bits padded).
   For double (64-bit): 16 hex chars of the native 64-bit IEEE pattern. *)
PROCEDURE EmitFloatHex(wr: Wr.T;  v: MSIR.Value) =
  VAR
    f      : Target.Float;
    buf    : ARRAY [0..15] OF TFloat.Byte;
    nBytes : INTEGER;
    bits64 : Word.T;
  BEGIN
    MSIR.GetFloatVal(v, f);
    nBytes := TFloat.ToBytes(f, buf);
    IF nBytes = 4 THEN
      (* Widen float32 → double64.  Reconstruct 32-bit IEEE from little-endian
         bytes, then rebias the exponent and extend the mantissa. *)
      VAR
        b32    : Word.T;
        sign, exp32, mant32, exp64, mant64 : Word.T;
      BEGIN
        b32 := Word.Or(Word.Or(Word.Or(
                 Word.Shift(VAL(buf[3], Word.T), 24),
                 Word.Shift(VAL(buf[2], Word.T), 16)),
                 Word.Shift(VAL(buf[1], Word.T),  8)),
                 VAL(buf[0], Word.T));
        sign  := Word.Shift(b32, -31);
        exp32 := Word.And(Word.Shift(b32, -23), 16_FF);
        mant32 := Word.And(b32, 16_7FFFFF);
        IF exp32 = 16_FF THEN
          exp64  := 16_7FF;                    (* infinity or NaN *)
          mant64 := Word.Shift(mant32, 29);
        ELSIF exp32 = 0 THEN
          IF mant32 = 0 THEN
            exp64 := 0;  mant64 := 0;          (* ±0 *)
          ELSE
            (* Subnormal float -> NORMAL double: double's exponent range covers
               it, so it is NOT a subnormal double.  Normalize: find the leading
               1 of the 23-bit mantissa (bit h), derive the double exponent from
               its position, and place the trailing fraction at the top of the
               52-bit double mantissa field.  (The old code kept exp64=0 and just
               shifted the mantissa, emitting a subnormal double with the wrong
               value — e.g. 2^-149 became 0x..20000000 instead of
               0x36A0000000000000.) *)
            VAR h := 22;
            BEGIN
              WHILE h > 0 AND Word.And(mant32, Word.Shift(1, h)) = 0 DO
                h := h - 1;
              END;
              exp64  := h + 874;                (* (h - 149) + 1023 *)
              mant64 := Word.Shift(Word.And(mant32, Word.Shift(1, h) - 1),
                                   52 - h);
            END;
          END;
        ELSE
          exp64  := exp32 + 896;                (* rebias: 1023 - 127 *)
          mant64 := Word.Shift(mant32, 29);     (* float 23 mant bits; double 52 *)
        END;
        bits64 := Word.Or(Word.Or(
                    Word.Shift(sign, 63),
                    Word.Shift(exp64, 52)),
                    mant64);
      END;
    ELSE
      (* Double (8 bytes), little-endian → Word.T *)
      bits64 := 0;
      FOR i := 7 TO 0 BY -1 DO
        bits64 := Word.Or(Word.Shift(bits64, 8), VAL(buf[i], Word.T));
      END;
    END;
    (* Emit big-endian 16 hex digits *)
    Wr.PutText(wr, "0x");
    FOR i := 7 TO 0 BY -1 DO
      VAR byte := Word.And(Word.Shift(bits64, -(8*i)), 16_FF);
      BEGIN
        Wr.PutChar(wr, HexDigit[Word.And(Word.Shift(byte, -4), 16_F)]);
        Wr.PutChar(wr, HexDigit[Word.And(byte, 16_F)]);
      END;
    END;
  END EmitFloatHex;

(* Emit a float constant.  LLVM 22+ rejects decimal literals for float (32-bit)
   so we always use hex notation via EmitFloatHex, which handles both F32 and F64. *)
PROCEDURE EmitFloatDecimal(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    EmitFloatHex(wr, v);
  END EmitFloatDecimal;

(* Emit just the LLVM name/constant for a value (no type prefix). *)
PROCEDURE LLOpVal(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "undef"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Wr.PutText(wr, Fmt.Int(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstFloat =>
        EmitFloatDecimal(wr, v);
    | MSIR.ValueKind.ConstNil =>
        (* Aggregate types (FixedArray, Struct): emit "zeroinitializer" so that
           `store [N x T] zeroinitializer, ptr %alloca` works correctly.
           Pointer/ref types: emit "null" (the standard LLVM representation). *)
        IF MSIR.Kind(MSIR.ValueType(v)) = MSIR.TypeKind.FixedArray
           OR MSIR.Kind(MSIR.ValueType(v)) = MSIR.TypeKind.Struct THEN
          Wr.PutText(wr, "zeroinitializer");
        ELSE
          Wr.PutText(wr, "null");
        END;
    | MSIR.ValueKind.ConstProc =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, LLSymbol(MSIR.GetConstProc(v)));
    | MSIR.ValueKind.ConstTextLit =>
        (* Emit as a constant-expression GEP: no separate instruction needed. *)
        VAR ap := "i" & Fmt.Int(Target.AddressSize());
        BEGIN
          Wr.PutText(wr, "getelementptr inbounds (i8, ptr @textlit_");
          Wr.PutText(wr, Fmt.Int(MSIR.GetTextLitUID(v)));
          Wr.PutText(wr, ", " & ap & " " & Fmt.Int(Target.AddressBytes()) & ")");
        END;
    | MSIR.ValueKind.GlobalRef =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, MSIR.ValueName(v));
    | MSIR.ValueKind.ConstStruct =>
        (* Inline constant struct: { field0, field1, ... } *)
        VAR n := MSIR.GetConstStructFieldCount(v);
        BEGIN
          Wr.PutText(wr, "{ ");
          FOR i := 0 TO n - 1 DO
            IF i > 0 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.GetConstStructField(v, i));
          END;
          Wr.PutText(wr, " }");
        END;
    | MSIR.ValueKind.ConstAggArray =>
        (* Inline constant array literal: [ elt0, elt1, ... ] *)
        VAR n := MSIR.GetConstStructFieldCount(v);
        BEGIN
          Wr.PutText(wr, "[");
          FOR i := 0 TO n - 1 DO
            IF i > 0 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.GetConstStructField(v, i));
          END;
          Wr.PutText(wr, "]");
        END;
    | MSIR.ValueKind.StructFieldRef =>
        (* getelementptr inbounds (i8, ptr @Mod_M3_info, i{AP} N) *)
        VAR ap := "i" & Fmt.Int(Target.AddressSize());
        BEGIN
          Wr.PutText(wr, "getelementptr inbounds (i8, ptr ");
          Wr.PutText(wr, MSIR.ValueName(v));   (* "@Mod_M3_info" *)
          Wr.PutText(wr, ", " & ap & " ");
          Wr.PutText(wr, Fmt.Int(MSIR.GetStructFieldOffset(v)));
          Wr.PutText(wr, ")");
        END;
    ELSE
        (* InsnResult/Param names: % prefix; bare param names get % added.
           ExcDesc values start with @ (full symbol) and are emitted as-is. *)
        VAR n: TEXT := MSIR.ValueName(v);  c0: CHAR;
        BEGIN
          IF Text.Length(n) = 0 THEN
            Wr.PutText(wr, "undef");
          ELSE
            c0 := Text.GetChar(n, 0);
            IF c0 = '%' OR c0 = '@' THEN
              (* Already has the right sigil. *)
            ELSE
              Wr.PutText(wr, "%");
            END;
            Wr.PutText(wr, n);
          END;
        END;
    END;
  END LLOpVal;

(* Emit "type val" — used as the first (type-carrying) operand. *)
PROCEDURE LLTypedVal(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    LLType(wr, MSIR.ValueType(v));
    Wr.PutText(wr, " ");
    LLOpVal(wr, v);
  END LLTypedVal;

(*-------------------------------------------- struct field index lookup *)

PROCEDURE FieldIndex(structType: MSIR.T;  name: TEXT): INTEGER =
  BEGIN
    FOR i := 0 TO MSIR.StructFieldCount(structType) - 1 DO
      IF Text.Equal(MSIR.StructField(structType, i).name, name) THEN
        RETURN i;
      END;
    END;
    RETURN -1;
  END FieldIndex;

(*-------------------------------------------- GC read-barrier emission *)

(* Emit the CM3 read barrier for a newly loaded traced reference.
   refName is the SSA name of the loaded value (e.g. "%t1").
   The barrier preserves the strong tricolor invariant by scanning
   gray objects before the mutator can observe white references through them.

   Barrier structure (all as inline LLVM blocks):
     nil check:        skip if ref is null
     misaligned check: skip if low bit is set (not a real heap ref)
     gray-bit check:   skip if header gray bit is clear (object is clean)
     slow path:        call RTHooks__CheckLoadTracedRef(ref)

   Header layout (RT0.RefHeader = Target.AddressBytes() before object ptr):
     bit RH_gray_offset = gray bit (mask = 1 << RH_gray_offset) *)
PROCEDURE EmitGcReadBarrier(wr: Wr.T;  refName: TEXT;  locIdx: INTEGER := -1) =
  VAR
    n   : TEXT;
    ap  := "i" & Fmt.Int(Target.AddressSize());
    dbg := "";
  BEGIN
    IF dbgEnabled AND locIdx >= 0 THEN dbg := ", !dbg !" & Fmt.Int(locIdx) END;
    INC(auxN);
    n := Fmt.Int(auxN);
    (* nil check *)
    Wr.PutText(wr, "  %__gc_nil." & n & " = icmp eq ptr " & refName & ", null\n");
    Wr.PutText(wr, "  br i1 %__gc_nil." & n
                   & ", label %gc.skip." & n & ", label %gc.check." & n & "\n");
    (* misaligned check: low bit set → not a real heap pointer *)
    Wr.PutText(wr, "gc.check." & n & ":\n");
    Wr.PutText(wr, "  %__gc_int." & n & " = ptrtoint ptr " & refName & " to " & ap & "\n");
    Wr.PutText(wr, "  %__gc_low." & n & " = and " & ap & " %__gc_int." & n & ", 1\n");
    Wr.PutText(wr, "  %__gc_ma."  & n & " = icmp ne " & ap & " %__gc_low." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_ma." & n
                   & ", label %gc.skip." & n & ", label %gc.gray." & n & "\n");
    (* gray-bit check: read object header word (8 bytes before object ptr) *)
    Wr.PutText(wr, "gc.gray." & n & ":\n");
    Wr.PutText(wr, "  %__gc_hptr." & n
                   & " = getelementptr i8, ptr " & refName
                   & ", " & ap & " -" & Fmt.Int(Target.AddressBytes()) & "\n");
    Wr.PutText(wr, "  %__gc_hdr."  & n
                   & " = load " & ap & ", ptr %__gc_hptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_gb."   & n
                   & " = and " & ap & " %__gc_hdr." & n
                   & ", " & Fmt.Int(Word.Shift(1, RH_gray_offset)) & "\n");
    Wr.PutText(wr, "  %__gc_gr."   & n
                   & " = icmp ne " & ap & " %__gc_gb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_gr." & n
                   & ", label %gc.slow." & n & ", label %gc.skip." & n & "\n");
    (* slow path *)
    Wr.PutText(wr, "gc.slow." & n & ":\n");
    Wr.PutText(wr, "  call void @"
                   & LLHookName(MSIR.ModuleGCLoadBarrier(curEmitModule),
                                 "RTHooks__CheckLoadTracedRef")
                   & "(ptr " & refName & ")" & dbg & "\n");
    Wr.PutText(wr, "  br label %gc.skip." & n & "\n");
    (* barrier exit — subsequent insns continue here *)
    Wr.PutText(wr, "gc.skip." & n & ":\n");
  END EmitGcReadBarrier;

(*------------------------------------------ GC write-barrier emission *)

(* Emit the CM3 write barrier for a gc.store to a heap object field.
   containerName is the SSA name of the containing heap object.
   The actual store follows immediately after this call.

   The barrier marks the containing object and its page as dirty so the
   GC will re-scan the object's reference fields in the next sweep.

   Fast path: read the header word (Target.AddressBytes() before the object
   pointer); if the dirty bit (bit RH_dirty_offset, 1<<RH_dirty_offset)
   is already set, skip the slow-path call.
   Otherwise call RTHooks__CheckStoreTraced.

   Header layout matches RT0.RefHeaderBits:
     bit 0:                forwarded  (RH_forwarded_offset)
     bits 1-20:            typecode   (RH_typecode_offset, RH_typecode_size)
     bit RH_dirty_offset:  dirty
     bit RH_gray_offset:   gray *)
PROCEDURE EmitGcWriteBarrier(wr: Wr.T;  containerName: TEXT;  locIdx: INTEGER := -1) =
  VAR
    n   : TEXT;
    ap  := "i" & Fmt.Int(Target.AddressSize());
    dbg := "";
  BEGIN
    IF dbgEnabled AND locIdx >= 0 THEN dbg := ", !dbg !" & Fmt.Int(locIdx) END;
    INC(auxN);
    n := Fmt.Int(auxN);
    (* Read object header; skip barrier if already dirty. *)
    Wr.PutText(wr, "  %__gc_whptr." & n
                   & " = getelementptr i8, ptr " & containerName
                   & ", " & ap & " -" & Fmt.Int(Target.AddressBytes()) & "\n");
    Wr.PutText(wr, "  %__gc_whdr."  & n
                   & " = load " & ap & ", ptr %__gc_whptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_wdb."   & n
                   & " = and " & ap & " %__gc_whdr." & n
                   & ", " & Fmt.Int(Word.Shift(1, RH_dirty_offset)) & "\n");
    Wr.PutText(wr, "  %__gc_wdirty." & n
                   & " = icmp ne " & ap & " %__gc_wdb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_wdirty." & n
                   & ", label %gc.wskip." & n & ", label %gc.wslow." & n & "\n");
    Wr.PutText(wr, "gc.wslow." & n & ":\n");
    Wr.PutText(wr, "  call void @"
                   & LLHookName(MSIR.ModuleGCStoreBarrier(curEmitModule),
                                 "RTHooks__CheckStoreTraced")
                   & "(ptr " & containerName & ")" & dbg & "\n");
    Wr.PutText(wr, "  br label %gc.wskip." & n & "\n");
    (* Store follows immediately after gc.wskip.N: label. *)
    Wr.PutText(wr, "gc.wskip." & n & ":\n");
  END EmitGcWriteBarrier;

(*----------------------------------------------- floor div/mod helpers *)

(* Emit Modula-3 floor division: q = floor(a / b) using sdiv + correction. *)
PROCEDURE EmitFloorDiv(wr: Wr.T;  res: MSIR.Value;  a, b: MSIR.Value) =
  VAR
    t      := MSIR.ValueType(res);
    resN   := MSIR.ValueName(res);
    q      := NewAux();
    r      := NewAux();
    xorv   := NewAux();
    ne     := NewAux();
    ng     := NewAux();
    adj1   := NewAux();
    adj    := NewAux();
  BEGIN
    Wr.PutText(wr, "  " & q & " = sdiv "); LLType(wr, t);
    Wr.PutText(wr, " "); LLOpVal(wr, a); Wr.PutText(wr, ", "); LLOpVal(wr, b); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & r & " = srem "); LLType(wr, t);
    Wr.PutText(wr, " "); LLOpVal(wr, a); Wr.PutText(wr, ", "); LLOpVal(wr, b); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & xorv & " = xor "); LLType(wr, t);
    Wr.PutText(wr, " "); LLOpVal(wr, a); Wr.PutText(wr, ", "); LLOpVal(wr, b); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & ne & " = icmp ne "); LLType(wr, t);
    Wr.PutText(wr, " " & r & ", 0\n");

    Wr.PutText(wr, "  " & ng & " = icmp slt "); LLType(wr, t);
    Wr.PutText(wr, " " & xorv & ", 0\n");

    Wr.PutText(wr, "  " & adj1 & " = and i1 " & ne & ", " & ng & "\n");

    Wr.PutText(wr, "  " & adj & " = zext i1 " & adj1 & " to "); LLType(wr, t); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & resN & " = sub "); LLType(wr, t);
    Wr.PutText(wr, " " & q & ", " & adj & "\n");
  END EmitFloorDiv;

(* Emit Modula-3 floor mod: m = a - floor(a/b)*b *)
PROCEDURE EmitFloorMod(wr: Wr.T;  res: MSIR.Value;  a, b: MSIR.Value) =
  VAR
    t      := MSIR.ValueType(res);
    resN   := MSIR.ValueName(res);
    r      := NewAux();
    xorv   := NewAux();
    ng     := NewAux();
    ne     := NewAux();
    adj1   := NewAux();
    corr   := NewAux();
  BEGIN
    Wr.PutText(wr, "  " & r & " = srem "); LLType(wr, t);
    Wr.PutText(wr, " "); LLOpVal(wr, a); Wr.PutText(wr, ", "); LLOpVal(wr, b); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & xorv & " = xor "); LLType(wr, t);
    Wr.PutText(wr, " "); LLOpVal(wr, a); Wr.PutText(wr, ", "); LLOpVal(wr, b); Wr.PutText(wr, "\n");

    Wr.PutText(wr, "  " & ng & " = icmp slt "); LLType(wr, t);
    Wr.PutText(wr, " " & xorv & ", 0\n");

    Wr.PutText(wr, "  " & ne & " = icmp ne "); LLType(wr, t);
    Wr.PutText(wr, " " & r & ", 0\n");

    Wr.PutText(wr, "  " & adj1 & " = and i1 " & ng & ", " & ne & "\n");

    Wr.PutText(wr, "  " & corr & " = select i1 " & adj1 & ", ");
    LLType(wr, t); Wr.PutText(wr, " "); LLOpVal(wr, b);
    Wr.PutText(wr, ", "); LLType(wr, t); Wr.PutText(wr, " 0\n");

    Wr.PutText(wr, "  " & resN & " = add "); LLType(wr, t);
    Wr.PutText(wr, " " & r & ", " & corr & "\n");
  END EmitFloorMod;

(*----------------------------------------------- open-array elem addr *)

PROCEDURE EmitOAElemAddr(wr: Wr.T;  i: MSIR.Insn) =
  VAR
    res     := MSIR.InsnResult(i);
    oaV     := MSIR.InsnOperand(i, 0);
    oaT     := MSIR.ValueType(oaV);
    rank    := MSIR.OpenArrayRank(oaT);
    eltType := MSIR.OpenArrayElt(oaT);
    dataPtr := NewAux();
  BEGIN
    Wr.PutText(wr, "  " & dataPtr & " = extractvalue ");
    LLType(wr, oaT); Wr.PutText(wr, " "); LLOpVal(wr, oaV);
    Wr.PutText(wr, ", 0\n");

    Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = getelementptr inbounds ");
    LLType(wr, eltType);
    Wr.PutText(wr, ", ptr " & dataPtr);
    FOR k := 0 TO rank - 1 DO
      Wr.PutText(wr, ", ");
      LLTypedVal(wr, MSIR.InsnOperand(i, 1 + k));
    END;
    Wr.PutText(wr, "\n");
  END EmitOAElemAddr;

(*------------------------------------------------------ insn emission *)

PROCEDURE CmpPredText(p: MSIR.CmpPred): TEXT =
  BEGIN
    CASE p OF
    | MSIR.CmpPred.Eq  => RETURN "eq";
    | MSIR.CmpPred.Ne  => RETURN "ne";
    | MSIR.CmpPred.Slt => RETURN "slt";
    | MSIR.CmpPred.Sle => RETURN "sle";
    | MSIR.CmpPred.Sgt => RETURN "sgt";
    | MSIR.CmpPred.Sge => RETURN "sge";
    | MSIR.CmpPred.Ult => RETURN "ult";
    | MSIR.CmpPred.Ule => RETURN "ule";
    | MSIR.CmpPred.Ugt => RETURN "ugt";
    | MSIR.CmpPred.Uge => RETURN "uge";
    END;
  END CmpPredText;

PROCEDURE FCmpPredText(p: MSIR.FCmpPred): TEXT =
  BEGIN
    CASE p OF
    | MSIR.FCmpPred.OEq => RETURN "oeq";
    | MSIR.FCmpPred.ONe => RETURN "one";
    | MSIR.FCmpPred.OLt => RETURN "olt";
    | MSIR.FCmpPred.OLe => RETURN "ole";
    | MSIR.FCmpPred.OGt => RETURN "ogt";
    | MSIR.FCmpPred.OGe => RETURN "oge";
    | MSIR.FCmpPred.ORd => RETURN "ord";
    | MSIR.FCmpPred.UNe => RETURN "une";
    | MSIR.FCmpPred.ULt => RETURN "ult";
    | MSIR.FCmpPred.ULe => RETURN "ule";
    | MSIR.FCmpPred.UGt => RETURN "ugt";
    | MSIR.FCmpPred.UGe => RETURN "uge";
    END;
  END FCmpPredText;

PROCEDURE MemOrderStr(ord: MSIR.MemOrder): TEXT =
  BEGIN
    CASE ord OF
    | MSIR.MemOrder.Relaxed => RETURN "monotonic";
    | MSIR.MemOrder.Release => RETURN "release";
    | MSIR.MemOrder.Acquire => RETURN "acquire";
    | MSIR.MemOrder.AcqRel  => RETURN "acq_rel";
    | MSIR.MemOrder.SeqCst  => RETURN "seq_cst";
    END;
  END MemOrderStr;

PROCEDURE AtomicRMWOpStr(op: MSIR.AtomicRMWOp): TEXT =
  BEGIN
    CASE op OF
    | MSIR.AtomicRMWOp.Xchg => RETURN "xchg";
    | MSIR.AtomicRMWOp.Add  => RETURN "add";
    | MSIR.AtomicRMWOp.Sub  => RETURN "sub";
    | MSIR.AtomicRMWOp.And  => RETURN "and";
    | MSIR.AtomicRMWOp.Or   => RETURN "or";
    | MSIR.AtomicRMWOp.Xor  => RETURN "xor";
    END;
  END AtomicRMWOpStr;

PROCEDURE AtomicAlign(t: MSIR.T): INTEGER =
  VAR w := MSIR.BitWidth(t);
  BEGIN
    IF w <= 0 THEN RETURN 8 END;   (* pointer types → 8 bytes *)
    RETURN (w + 7) DIV 8;
  END AtomicAlign;

PROCEDURE EmitBinop(wr: Wr.T;  llop: TEXT;  res, a, b: MSIR.Value) =
  BEGIN
    Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & llop & " ");
    LLType(wr, MSIR.ValueType(res));
    Wr.PutText(wr, " "); LLOpVal(wr, a);
    Wr.PutText(wr, ", "); LLOpVal(wr, b);
    Wr.PutText(wr, "\n");
  END EmitBinop;

PROCEDURE EmitInsn(wr: Wr.T;  i: MSIR.Insn) =
  VAR
    op     := MSIR.InsnOp(i);
    res    := MSIR.InsnResult(i);
    nOps   := MSIR.InsnOperandCount(i);
    ip     := "i" & Fmt.Int(Target.IntegerSize());
    ap     := "i" & Fmt.Int(Target.AddressSize());
    locIdx := InsnDbgLocIdx(i);    (* per-instruction !dbg index (Phase 3) *)
  BEGIN
    CASE op OF

    (* --- multi-instruction expansions (no leading indent) --- *)

    | MSIR.Op.IDiv =>
        EmitFloorDiv(wr, res, MSIR.InsnOperand(i, 0), MSIR.InsnOperand(i, 1));

    | MSIR.Op.IMod =>
        EmitFloorMod(wr, res, MSIR.InsnOperand(i, 0), MSIR.InsnOperand(i, 1));

    | MSIR.Op.OpenArrayElemAddr =>
        EmitOAElemAddr(wr, i);

    (* --- single-instruction ops --- *)

    | MSIR.Op.Alloca =>
        VAR cnt    := MSIR.InsnExtractIdx(i);
            varIdx := GetDbgVarMetaIdx(res);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = alloca ");
          LLType(wr, MSIR.InsnTargetType(i));
          IF cnt > 1 THEN
            Wr.PutText(wr, ", " & ip & " " & Fmt.Int(cnt));
          END;
          Wr.PutText(wr, "\n");
          IF varIdx >= 0 AND locIdx >= 0 THEN
            Wr.PutText(wr, "  call void @llvm.dbg.declare(metadata ptr "
              & MSIR.ValueName(res)
              & ", metadata !" & Fmt.Int(varIdx)
              & ", metadata !DIExpression()), !dbg !"
              & Fmt.Int(locIdx) & "\n");
          END;
        END;

    | MSIR.Op.AllocaDyn =>
        (* alloca i8, i{IP} %byteCount — dynamic stack buffer *)
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = alloca i8, ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.Load =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = load ");
        LLType(wr, MSIR.ValueType(res));
        Wr.PutText(wr, ", ptr ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.GcLoad =>
        (* Load the traced reference from the slot, then apply the CM3
           read barrier: nil check → misaligned check → gray-bit check →
           conditional call to RTHooks__CheckLoadTracedRef.
           The barrier preserves the strong tricolor invariant by scanning
           any gray object before the mutator uses it. *)
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = load ptr, ptr ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");
        EmitGcReadBarrier(wr, MSIR.ValueName(res), locIdx);

    | MSIR.Op.Store =>
        Wr.PutText(wr, "  store ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ptr ");
        LLOpVal(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, "\n");

    | MSIR.Op.GcStore =>
        (* ops[0]=value, ops[1]=slot, ops[2]=container (optional).
           If container is present (heap field store), emit the CM3 dirty-bit
           write barrier before the store.  Module-global stores (no container)
           skip the barrier — they are GC roots tracked via module descriptor. *)
        IF nOps = 3 THEN
          EmitGcWriteBarrier(wr, LLOpValStr(MSIR.InsnOperand(i, 2)), locIdx);
        END;
        Wr.PutText(wr, "  store ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ptr ");
        LLOpVal(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, "\n");

    | MSIR.Op.IAdd  => EmitBinop(wr, "add",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.ISub  => EmitBinop(wr, "sub",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IMul  => EmitBinop(wr, "mul",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IAnd  => EmitBinop(wr, "and",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IOr   => EmitBinop(wr, "or",   res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IXor  => EmitBinop(wr, "xor",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IShl  => EmitBinop(wr, "shl",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.ILShr => EmitBinop(wr, "lshr", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IAShr => EmitBinop(wr, "ashr", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IUDiv => EmitBinop(wr, "udiv", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IURem => EmitBinop(wr, "urem", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));

    | MSIR.Op.IRotL, MSIR.Op.IRotR =>
        VAR
          x      := MSIR.InsnOperand(i, 0);
          n      := MSIR.InsnOperand(i, 1);
          xt     := MSIR.ValueType(x);
          bits   := MSIR.BitWidth(xt);
          fsuf   := "i" & Fmt.Int(bits);
          iname  : TEXT;
        BEGIN
          IF MSIR.InsnOp(i) = MSIR.Op.IRotL
            THEN iname := "llvm.fshl." & fsuf;
            ELSE iname := "llvm.fshr." & fsuf;
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = call ");
          LLType(wr, xt);
          Wr.PutText(wr, " @" & iname & "(");
          LLTypedVal(wr, x);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, x);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, n);
          Wr.PutText(wr, ")\n");
        END;

    | MSIR.Op.Select =>
        VAR
          cond    := MSIR.InsnOperand(i, 0);
          ifTrue  := MSIR.InsnOperand(i, 1);
          ifFalse := MSIR.InsnOperand(i, 2);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = select ");
          LLTypedVal(wr, cond);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, ifTrue);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, ifFalse);
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.AtomicFence =>
        Wr.PutText(wr, "  fence " & MemOrderStr(MSIR.InsnMemOrder(i)) & "\n");

    | MSIR.Op.AtomicLoad =>
        VAR
          ptr   := MSIR.InsnOperand(i, 0);
          elemT := MSIR.InsnTargetType(i);
          align := AtomicAlign(elemT);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = load atomic ");
          LLType(wr, elemT);
          Wr.PutText(wr, ", ptr ");
          LLOpVal(wr, ptr);
          Wr.PutText(wr, " " & MemOrderStr(MSIR.InsnMemOrder(i)));
          Wr.PutText(wr, ", align " & Fmt.Int(align) & "\n");
          IF MSIR.Kind(elemT) = MSIR.TypeKind.GcRef THEN
            EmitGcReadBarrier(wr, MSIR.ValueName(res), locIdx);
          END;
        END;

    | MSIR.Op.AtomicStore =>
        VAR
          val   := MSIR.InsnOperand(i, 0);
          ptr   := MSIR.InsnOperand(i, 1);
          elemT := MSIR.ValueType(val);
          align := AtomicAlign(elemT);
        BEGIN
          IF nOps = 3 THEN
            EmitGcWriteBarrier(wr, LLOpValStr(MSIR.InsnOperand(i, 2)), locIdx);
          END;
          Wr.PutText(wr, "  store atomic ");
          LLTypedVal(wr, val);
          Wr.PutText(wr, ", ptr ");
          LLOpVal(wr, ptr);
          Wr.PutText(wr, " " & MemOrderStr(MSIR.InsnMemOrder(i)));
          Wr.PutText(wr, ", align " & Fmt.Int(align) & "\n");
        END;

    | MSIR.Op.AtomicRMW =>
        VAR
          ptr    := MSIR.InsnOperand(i, 0);
          val    := MSIR.InsnOperand(i, 1);
          elemT  := MSIR.InsnTargetType(i);
        BEGIN
          IF nOps = 3 THEN
            EmitGcWriteBarrier(wr, LLOpValStr(MSIR.InsnOperand(i, 2)), locIdx);
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = atomicrmw ");
          Wr.PutText(wr, AtomicRMWOpStr(MSIR.InsnAtomicOp(i)) & " ptr ");
          LLOpVal(wr, ptr);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, val);
          Wr.PutText(wr, " " & MemOrderStr(MSIR.InsnMemOrder(i)) & "\n");
          IF MSIR.Kind(elemT) = MSIR.TypeKind.GcRef THEN
            EmitGcReadBarrier(wr, MSIR.ValueName(res), locIdx);
          END;
        END;

    | MSIR.Op.AtomicCmpXchg =>
        VAR
          varPtr  := MSIR.InsnOperand(i, 0);
          expPtr  := MSIR.InsnOperand(i, 1);
          desired := MSIR.InsnOperand(i, 2);
          elemT   := MSIR.InsnTargetType(i);
          align   := AtomicAlign(elemT);
          rn      := MSIR.ValueName(res);
          succOrd := MemOrderStr(MSIR.InsnMemOrder(i));
          failOrd := MemOrderStr(MSIR.InsnMemOrder2(i));
        BEGIN
          IF nOps = 4 THEN
            EmitGcWriteBarrier(wr, LLOpValStr(MSIR.InsnOperand(i, 3)), locIdx);
          END;
          (* Load expected value from expected_ptr *)
          Wr.PutText(wr, "  " & rn & ".exp = load ");
          LLType(wr, elemT);
          Wr.PutText(wr, ", ptr ");
          LLOpVal(wr, expPtr);
          Wr.PutText(wr, ", align " & Fmt.Int(align) & "\n");
          (* Do cmpxchg; rn already starts with '%' *)
          Wr.PutText(wr, "  " & rn & ".cx = cmpxchg ptr ");
          LLOpVal(wr, varPtr);
          Wr.PutText(wr, ", ");
          LLType(wr, elemT);
          Wr.PutText(wr, " " & rn & ".exp, ");
          LLTypedVal(wr, desired);
          Wr.PutText(wr, " " & succOrd & " " & failOrd & "\n");
          (* Extract old value *)
          Wr.PutText(wr, "  " & rn & ".old = extractvalue {");
          LLType(wr, elemT);
          Wr.PutText(wr, ", i1} " & rn & ".cx, 0\n");
          (* Extract success flag — this is the MSIR result *)
          Wr.PutText(wr, "  " & rn & " = extractvalue {");
          LLType(wr, elemT);
          Wr.PutText(wr, ", i1} " & rn & ".cx, 1\n");
          (* Store old value back to expected_ptr; apply read barrier if traced ref *)
          Wr.PutText(wr, "  store ");
          LLType(wr, elemT);
          Wr.PutText(wr, " " & rn & ".old, ptr ");
          LLOpVal(wr, expPtr);
          Wr.PutText(wr, ", align " & Fmt.Int(align) & "\n");
          IF MSIR.Kind(elemT) = MSIR.TypeKind.GcRef THEN
            EmitGcReadBarrier(wr, rn & ".old", locIdx);
          END;
        END;

    | MSIR.Op.FAdd => EmitBinop(wr, "fadd", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FSub => EmitBinop(wr, "fsub", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FMul => EmitBinop(wr, "fmul", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FDiv => EmitBinop(wr, "fdiv", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FNeg =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = fneg ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.FPFloor, MSIR.Op.FPCeil, MSIR.Op.FPRound, MSIR.Op.FPAbs =>
        VAR
          src   := MSIR.InsnOperand(i, 0);
          ftype := MSIR.ValueType(src);
          bits  := MSIR.BitWidth(ftype);
          iname : TEXT;
          fsuf  := "f" & Fmt.Int(bits);
        BEGIN
          CASE MSIR.InsnOp(i) OF
          | MSIR.Op.FPFloor     => iname := "llvm.floor.";
          | MSIR.Op.FPCeil      => iname := "llvm.ceil.";
          | MSIR.Op.FPAbs       => iname := "llvm.fabs.";
          ELSE                     iname := "llvm.roundeven."; (* NearestElseEven = FloatMode.RoundDefault *)
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = call ");
          LLType(wr, ftype);
          Wr.PutText(wr, " @" & iname & fsuf & "(");
          LLTypedVal(wr, src);
          Wr.PutText(wr, ")\n");
        END;

    | MSIR.Op.ICmp =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = icmp ");
        Wr.PutText(wr, CmpPredText(MSIR.InsnCmpPred(i)));
        Wr.PutText(wr, " ");
        LLType(wr, MSIR.ValueType(MSIR.InsnOperand(i, 0)));
        Wr.PutText(wr, " ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        LLOpVal(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, "\n");

    | MSIR.Op.FCmp =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = fcmp ");
        Wr.PutText(wr, FCmpPredText(MSIR.InsnFCmpPred(i)));
        Wr.PutText(wr, " ");
        LLType(wr, MSIR.ValueType(MSIR.InsnOperand(i, 0)));
        Wr.PutText(wr, " ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        LLOpVal(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, "\n");

    | MSIR.Op.Br =>
        Wr.PutText(wr, "  br label %");
        Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, 0)));
        Wr.PutText(wr, "\n");

    | MSIR.Op.CondBr =>
        Wr.PutText(wr, "  br i1 ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", label %");
        Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, 0)));
        Wr.PutText(wr, ", label %");
        Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, 1)));
        Wr.PutText(wr, "\n");

    | MSIR.Op.Ret =>
        IF nOps = 0 THEN
          Wr.PutText(wr, "  ret void\n");
        ELSE
          Wr.PutText(wr, "  ret ");
          LLTypedVal(wr, MSIR.InsnOperand(i, 0));
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.Unreachable =>
        Wr.PutText(wr, "  unreachable\n");

    | MSIR.Op.Call =>
        VAR callee := MSIR.InsnCallee(i);
        BEGIN
          Wr.PutText(wr, "  ");
          IF res # NIL THEN Wr.PutText(wr, MSIR.ValueName(res) & " = ") END;
          Wr.PutText(wr, "call ");
          LLType(wr, MSIR.ProcResultType(callee));
          Wr.PutText(wr, " @");
          Wr.PutText(wr, LLSymbol(callee));
          Wr.PutText(wr, "(");
          FOR k := 0 TO nOps - 1 DO
            IF k > 0 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.InsnOperand(i, k));
          END;
          IF locIdx >= 0
            THEN Wr.PutText(wr, "), !dbg !" & Fmt.Int(locIdx) & "\n");
            ELSE Wr.PutText(wr, ")\n");
          END;
        END;

    | MSIR.Op.Invoke =>
        VAR
          callee  := MSIR.InsnCallee(i);
          normalB := MSIR.InsnBrTarget(i, 0);
          unwindB := MSIR.InsnBrTarget(i, 1);
        BEGIN
          Wr.PutText(wr, "  ");
          IF res # NIL THEN Wr.PutText(wr, MSIR.ValueName(res) & " = ") END;
          Wr.PutText(wr, "invoke ");
          LLType(wr, MSIR.ProcResultType(callee));
          Wr.PutText(wr, " @");
          Wr.PutText(wr, LLSymbol(callee));
          Wr.PutText(wr, "(");
          FOR k := 0 TO nOps - 1 DO
            IF k > 0 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.InsnOperand(i, k));
          END;
          Wr.PutText(wr, ")\n");
          Wr.PutText(wr, "          to label %");
          Wr.PutText(wr, MSIR.BlockLabel(normalB));
          Wr.PutText(wr, " unwind label %");
          Wr.PutText(wr, MSIR.BlockLabel(unwindB));
          IF locIdx >= 0
            THEN Wr.PutText(wr, ", !dbg !" & Fmt.Int(locIdx) & "\n");
            ELSE Wr.PutText(wr, "\n");
          END;
        END;

    | MSIR.Op.PtrAdd =>
        (* getelementptr i8, ptr %base, i{AP} N — advances base by N bytes *)
        VAR
          baseV := MSIR.InsnOperand(i, 0);
          idx   := MSIR.InsnExtractIdx(i);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = getelementptr i8, ptr ");
          LLOpVal(wr, baseV);
          Wr.PutText(wr, ", " & ap & " " & Fmt.Int(idx) & "\n");
        END;

    | MSIR.Op.GepByte =>
        (* getelementptr inbounds i8, ptr %base, i{AP} %offset — dynamic byte-offset ptr arith *)
        VAR
          baseV   := MSIR.InsnOperand(i, 0);
          offsetV := MSIR.InsnOperand(i, 1);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = getelementptr inbounds i8, ptr ");
          LLOpVal(wr, baseV);
          Wr.PutText(wr, ", ");
          LLTypedVal(wr, offsetV);
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.CallIndirect =>
        (* ops[0]=fn, ops[1..n-1]=args; targetType=return type *)
        VAR
          fnV   := MSIR.InsnOperand(i, 0);
          rtype := MSIR.InsnTargetType(i);
        BEGIN
          Wr.PutText(wr, "  ");
          IF res # NIL THEN Wr.PutText(wr, MSIR.ValueName(res) & " = ") END;
          Wr.PutText(wr, "call ");
          IF rtype # NIL THEN LLType(wr, rtype) ELSE Wr.PutText(wr, "void") END;
          Wr.PutText(wr, " ");
          LLOpVal(wr, fnV);
          Wr.PutText(wr, "(");
          FOR k := 1 TO nOps - 1 DO
            IF k > 1 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.InsnOperand(i, k));
          END;
          IF locIdx >= 0
            THEN Wr.PutText(wr, "), !dbg !" & Fmt.Int(locIdx) & "\n");
            ELSE Wr.PutText(wr, ")\n");
          END;
        END;

    | MSIR.Op.InvokeIndirect =>
        VAR
          fnV     := MSIR.InsnOperand(i, 0);
          rtype   := MSIR.InsnTargetType(i);
          normalB := MSIR.InsnBrTarget(i, 0);
          unwindB := MSIR.InsnBrTarget(i, 1);
        BEGIN
          Wr.PutText(wr, "  ");
          IF res # NIL THEN Wr.PutText(wr, MSIR.ValueName(res) & " = ") END;
          Wr.PutText(wr, "invoke ");
          IF rtype # NIL THEN LLType(wr, rtype) ELSE Wr.PutText(wr, "void") END;
          Wr.PutText(wr, " ");
          LLOpVal(wr, fnV);
          Wr.PutText(wr, "(");
          FOR k := 1 TO nOps - 1 DO
            IF k > 1 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.InsnOperand(i, k));
          END;
          Wr.PutText(wr, ")\n");
          Wr.PutText(wr, "          to label %");
          Wr.PutText(wr, MSIR.BlockLabel(normalB));
          Wr.PutText(wr, " unwind label %");
          Wr.PutText(wr, MSIR.BlockLabel(unwindB));
          IF locIdx >= 0
            THEN Wr.PutText(wr, ", !dbg !" & Fmt.Int(locIdx) & "\n");
            ELSE Wr.PutText(wr, "\n");
          END;
        END;

    | MSIR.Op.LandingPad =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = landingpad ");
        LLType(wr, MSIR.ValueType(res));
        Wr.PutText(wr, "\n");
        IF MSIR.InsnIsCleanup(i) THEN
          Wr.PutText(wr, "          cleanup\n");
        ELSE
          Wr.PutText(wr, "          catch ptr @_ZTI6_M3Exc\n");
        END;

    | MSIR.Op.ExtractValue =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = extractvalue ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", " & Fmt.Int(MSIR.InsnExtractIdx(i)) & "\n");

    | MSIR.Op.Resume =>
        Wr.PutText(wr, "  resume ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.FieldAddr =>
        VAR
          objV    := MSIR.InsnOperand(i, 0);
          objT    := MSIR.ValueType(objV);
          fname   := MSIR.InsnSelector(i);
          structT : MSIR.T;
          idx     : INTEGER;
        BEGIN
          IF MSIR.Kind(objT) = MSIR.TypeKind.Ptr  OR
             MSIR.Kind(objT) = MSIR.TypeKind.GcRef THEN
            structT := MSIR.EltType(objT);
          ELSE
            structT := objT;
          END;
          idx := FieldIndex(structT, fname);
          Wr.PutText(wr, "  " & MSIR.ValueName(res));
          Wr.PutText(wr, " = getelementptr inbounds ");
          LLType(wr, structT);
          Wr.PutText(wr, ", ptr ");
          LLOpVal(wr, objV);
          Wr.PutText(wr, ", i32 0, i32 " & Fmt.Int(idx) & "\n");
        END;

    | MSIR.Op.ArrayElemAddr =>
        VAR
          arrV      := MSIR.InsnOperand(i, 0);
          idxV      := MSIR.InsnOperand(i, 1);
          arrT      := MSIR.ValueType(arrV);
          fixedArrT : MSIR.T;
        BEGIN
          IF MSIR.Kind(arrT) = MSIR.TypeKind.Ptr THEN
            fixedArrT := MSIR.EltType(arrT);
          ELSE
            fixedArrT := arrT;
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res));
          Wr.PutText(wr, " = getelementptr inbounds ");
          LLType(wr, fixedArrT);
          Wr.PutText(wr, ", ptr ");
          LLOpVal(wr, arrV);
          Wr.PutText(wr, ", " & ap & " 0, ");
          LLTypedVal(wr, idxV);
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.OpenArraySize =>
        VAR
          oaV  := MSIR.InsnOperand(i, 0);
          dimV := MSIR.InsnOperand(i, 1);
          dim  := MSIR.GetIntVal(dimV);
          oaT  := MSIR.ValueType(oaV);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res));
          Wr.PutText(wr, " = extractvalue ");
          LLType(wr, oaT); Wr.PutText(wr, " "); LLOpVal(wr, oaV);
          Wr.PutText(wr, ", " & Fmt.Int(dim + 1) & "\n");
        END;

    | MSIR.Op.Convert =>
        VAR
          src     := MSIR.InsnOperand(i, 0);
          srcT    := MSIR.ValueType(src);
          dstT    := MSIR.InsnTargetType(i);
          srcBits := MSIR.BitWidth(srcT);
          dstBits := MSIR.BitWidth(dstT);
          convOp  : TEXT;
        BEGIN
          IF dstT = NIL THEN dstT := MSIR.ValueType(res) END;
          VAR
            srcIsPtr   := MSIR.Kind(srcT) = MSIR.TypeKind.Ptr
                       OR MSIR.Kind(srcT) = MSIR.TypeKind.GcRef
                       OR MSIR.Kind(srcT) = MSIR.TypeKind.GcSlot;
            dstIsPtr   := MSIR.Kind(dstT) = MSIR.TypeKind.Ptr
                       OR MSIR.Kind(dstT) = MSIR.TypeKind.GcRef
                       OR MSIR.Kind(dstT) = MSIR.TypeKind.GcSlot;
            srcIsFloat := MSIR.Kind(srcT) = MSIR.TypeKind.F32
                       OR MSIR.Kind(srcT) = MSIR.TypeKind.F64
                       OR MSIR.Kind(srcT) = MSIR.TypeKind.F128;
            dstIsFloat := MSIR.Kind(dstT) = MSIR.TypeKind.F32
                       OR MSIR.Kind(dstT) = MSIR.TypeKind.F64
                       OR MSIR.Kind(dstT) = MSIR.TypeKind.F128;
          BEGIN
            IF srcIsPtr AND dstBits > 0 THEN
              IF dstIsFloat THEN
                (* ptr → float LOOPHOLE: ptrtoint ptr to i<dstBits>, bitcast to float *)
                VAR tmpName := MSIR.ValueName(res) & ".pi";
                    intT    := MSIR.TI(dstBits);
                BEGIN
                  Wr.PutText(wr, "  " & tmpName & " = ptrtoint ");
                  LLTypedVal(wr, src);
                  Wr.PutText(wr, " to "); LLType(wr, intT); Wr.PutText(wr, "\n");
                  Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = bitcast ");
                  LLType(wr, intT); Wr.PutText(wr, " " & tmpName);
                  Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
                END;
              ELSE
                Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = ptrtoint ");
                LLTypedVal(wr, src);
                Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
              END;
            ELSIF srcBits > 0 AND dstIsPtr THEN
              IF srcIsFloat THEN
                (* float → ptr LOOPHOLE: bitcast float to i<srcBits>, inttoptr to ptr *)
                VAR tmpName := MSIR.ValueName(res) & ".fp";
                    intT    := MSIR.TI(srcBits);
                BEGIN
                  Wr.PutText(wr, "  " & tmpName & " = bitcast ");
                  LLTypedVal(wr, src);
                  Wr.PutText(wr, " to "); LLType(wr, intT); Wr.PutText(wr, "\n");
                  Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = inttoptr ");
                  LLType(wr, intT); Wr.PutText(wr, " " & tmpName);
                  Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
                END;
              ELSE
                Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = inttoptr ");
                LLTypedVal(wr, src);
                Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
              END;
            ELSIF srcBits > 0 AND dstBits > 0
                  AND NOT srcIsFloat AND dstIsFloat AND srcBits # dstBits THEN
              (* Int → Float LOOPHOLE with size mismatch:
                 resize integer to match float bit width, then bitcast. *)
              VAR
                tmpName  := MSIR.ValueName(res) & ".ri";
                resizeOp : TEXT;
                intT     := MSIR.TI(dstBits);
              BEGIN
                IF srcBits > dstBits THEN resizeOp := "trunc"
                ELSE                      resizeOp := "zext"
                END;
                Wr.PutText(wr, "  " & tmpName & " = " & resizeOp & " ");
                LLTypedVal(wr, src);
                Wr.PutText(wr, " to "); LLType(wr, intT); Wr.PutText(wr, "\n");
                Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = bitcast ");
                LLType(wr, intT); Wr.PutText(wr, " " & tmpName);
                Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
              END;
            ELSIF srcBits > 0 AND dstBits > 0
                  AND srcIsFloat AND NOT dstIsFloat AND srcBits # dstBits THEN
              (* Float → Int LOOPHOLE with size mismatch:
                 bitcast float to same-size integer, then resize. *)
              VAR
                tmpName  := MSIR.ValueName(res) & ".ri";
                resizeOp : TEXT;
                intT     := MSIR.TI(srcBits);
              BEGIN
                IF srcBits > dstBits THEN resizeOp := "trunc"
                ELSE                      resizeOp := "zext"
                END;
                Wr.PutText(wr, "  " & tmpName & " = bitcast ");
                LLTypedVal(wr, src);
                Wr.PutText(wr, " to "); LLType(wr, intT); Wr.PutText(wr, "\n");
                Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & resizeOp & " ");
                LLType(wr, intT); Wr.PutText(wr, " " & tmpName);
                Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
              END;
            ELSE
              (* Int ↔ Int or same-size float ↔ int: single cast. *)
              IF srcBits > 0 AND dstBits > 0 THEN
                IF    dstBits > srcBits THEN convOp := "sext";
                ELSIF dstBits < srcBits THEN convOp := "trunc";
                ELSE                        convOp := "bitcast";
                END;
              ELSE
                convOp := "bitcast";
              END;
              Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & convOp & " ");
              LLTypedVal(wr, src);
              Wr.PutText(wr, " to "); LLType(wr, dstT); Wr.PutText(wr, "\n");
            END;
          END;
        END;

    | MSIR.Op.SIToFP, MSIR.Op.FPToSI,
      MSIR.Op.FPExt,  MSIR.Op.FPTrunc,
      MSIR.Op.ZExt,   MSIR.Op.SExt, MSIR.Op.Trunc =>
        VAR
          src    := MSIR.InsnOperand(i, 0);
          dstT   := MSIR.ValueType(res);
          llop   : TEXT;
        BEGIN
          CASE MSIR.InsnOp(i) OF
          | MSIR.Op.SIToFP  => llop := "sitofp";
          | MSIR.Op.FPToSI  => llop := "fptosi";
          | MSIR.Op.FPExt   => llop := "fpext";
          | MSIR.Op.FPTrunc => llop := "fptrunc";
          | MSIR.Op.ZExt    => llop := "zext";
          | MSIR.Op.SExt    => llop := "sext";
          | MSIR.Op.Trunc   => llop := "trunc";
          ELSE                  llop := "bitcast";
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & llop & " ");
          LLTypedVal(wr, src);
          Wr.PutText(wr, " to ");
          LLType(wr, dstT);
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.Typecase =>
        (* Emit the RTHooks__ScanTypecase call and a switch on the result.
           The type table (@tc.table.N) is a mutable global (ScanTypecase
           lazily fills the defn pointer) collected in pendingTC and emitted
           after all function definitions. *)
        VAR
          nClauses := MSIR.InsnTypecaseClauseCount(i);
          refV     := MSIR.InsnOperand(i, 0);
          tblN     : TEXT;
          tblName  : TEXT;
          idxName  : TEXT;
          uids     : REF ARRAY OF INTEGER;
        BEGIN
          INC(tcN);
          tblN    := Fmt.Int(tcN);
          tblName := "@tc.table." & tblN;
          idxName := "%__tc_idx." & tblN;

          (* Collect UIDs for the pending type table. *)
          uids := NEW(REF ARRAY OF INTEGER, nClauses);
          FOR k := 0 TO nClauses - 1 DO
            uids[k] := MSIR.InsnTypecaseClause(i, k).uid;
          END;
          IF pendingTC = NIL THEN pendingTC := NEW(RefSeq.T).init() END;
          VAR ent := NEW(TCEntry);
          BEGIN
            ent.name := tblName;
            ent.uids := uids;
            pendingTC.addhi(ent);
          END;

          (* Call ScanTypecase(ref, table) → index via registered hook proc. *)
          Wr.PutText(wr, "  " & idxName & " = call i64 @"
                         & LLHookName(MSIR.ModuleScanTypecase(curEmitModule),
                                       "RTHooks__ScanTypecase")
                         & "(ptr ");
          LLOpVal(wr, refV);
          Wr.PutText(wr, ", ptr " & tblName & ")\n");

          (* Switch on index: ELSE clause is the default (last clause). *)
          VAR elseClause := MSIR.InsnTypecaseClause(i, nClauses - 1);
          BEGIN
            Wr.PutText(wr, "  switch i64 " & idxName & ", label %");
            Wr.PutText(wr, MSIR.BlockLabel(elseClause.block) & " [\n");
            FOR k := 0 TO nClauses - 2 DO  (* non-ELSE clauses *)
              VAR cl := MSIR.InsnTypecaseClause(i, k);
              BEGIN
                Wr.PutText(wr, "    i64 " & Fmt.Int(k) & ", label %");
                Wr.PutText(wr, MSIR.BlockLabel(cl.block) & "\n");
              END;
            END;
            Wr.PutText(wr, "  ]\n");
          END;
        END;

    ELSE
        Wr.PutText(wr, "  ; unhandled op\n");
    END;
  END EmitInsn;

(*------------------------------------------------------- block emission *)

PROCEDURE EmitBlock(wr: Wr.T;  b: MSIR.Block) =
  VAR n := MSIR.BlockInsnCount(b);
  BEGIN
    Wr.PutText(wr, MSIR.BlockLabel(b) & ":\n");
    FOR k := 0 TO n - 1 DO
      EmitInsn(wr, MSIR.BlockInsn(b, k));
    END;
    (* Ensure block is terminated (should always be, but guard anyway) *)
    IF n = 0 OR NOT MSIR.BlockIsTerminated(b) THEN
      Wr.PutText(wr, "  unreachable\n");
    END;
  END EmitBlock;

(*---------------------------------------------- extern proc collection *)

PROCEDURE ProcSeen(procs: RefSeq.T;  p: MSIR.Proc): BOOLEAN =
  (* Deduplicate by name — procMap overflow can create multiple distinct stubs
     for the same external function; name equality prevents duplicate declares. *)
  VAR pName := MSIR.ProcName(p);
  BEGIN
    FOR i := 0 TO procs.size() - 1 DO
      VAR q: MSIR.Proc := procs.get(i);
      BEGIN
        IF q = p THEN RETURN TRUE END;
        VAR qName := MSIR.ProcName(q); BEGIN
          IF pName # NIL AND qName # NIL AND Text.Equal(qName, pName) THEN
            RETURN TRUE
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END ProcSeen;

PROCEDURE IsModuleProc(m: MSIR.Module;  p: MSIR.Proc): BOOLEAN =
  (* TRUE if p is defined in this module (appears in module's proc list). *)
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF MSIR.ModuleProc(m, i) = p THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END IsModuleProc;

PROCEDURE IsModuleProcByName(m: MSIR.Module;  name: TEXT): BOOLEAN =
  (* TRUE if any module proc has this exact unqualified name.
     Used for extern stubs that refer to module-internal procs: when the
     scope name of the calling module is empty the stub gets the bare
     local name ("Abs") rather than the qualified form ("Main.Abs"). *)
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR pn := MSIR.ProcName(MSIR.ModuleProc(m, i)); BEGIN
        IF pn # NIL AND name # NIL AND Text.Equal(pn, name) THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END IsModuleProcByName;

PROCEDURE MaybeAddExtern(m: MSIR.Module;  externs: RefSeq.T;  p: MSIR.Proc) =
  (* LLVM 22+ disallows 'declare' + 'define' for the same symbol in one module.
     Use LLSymbol comparison (not raw ProcName) because a forward-reference stub
     may carry the fully-qualified name (e.g. "RTAllocator__GetTraced") while the
     module proc has the short name ("GetTraced"); raw name comparison misses this.
     Also skip the module's own _I3 binder: EmitModuleInfo always define[weak]s it,
     so a preceding 'declare' would be an LLVM 22 redefinition error. *)
  VAR sym: TEXT;
  BEGIN
    IF p = NIL OR MSIR.ProcName(p) = NIL OR IsModuleProc(m, p) OR ProcSeen(externs, p) THEN RETURN END;
    sym := LLSymbol(p);
    IF Text.Equal(sym, MSIR.ModuleName(m) & "_I3") THEN RETURN END;
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF Text.Equal(LLSymbol(MSIR.ModuleProc(m, i)), sym) THEN RETURN END;
    END;
    externs.addhi(p);
  END MaybeAddExtern;

PROCEDURE CollectExterns(m: MSIR.Module;  externs: RefSeq.T) =
  (* Walk all insns in all internal procs.  Collect:
     (a) external direct callees (Call/Invoke),
     (b) external procs used as ConstProc values (function-pointer args). *)
  VAR
    np  := MSIR.ModuleProcCount(m);
    p   : MSIR.Proc;
    nb  : INTEGER;
    b   : MSIR.Block;
    ni  : INTEGER;
    ins : MSIR.Insn;
    nop : INTEGER;
    v   : MSIR.Value;
  BEGIN
    FOR pi := 0 TO np - 1 DO
      p  := MSIR.ModuleProc(m, pi);
      nb := MSIR.ProcBlockCount(p);
      FOR bi := 0 TO nb - 1 DO
        b  := MSIR.ProcBlock(p, bi);
        ni := MSIR.BlockInsnCount(b);
        FOR ii := 0 TO ni - 1 DO
          ins := MSIR.BlockInsn(b, ii);
          (* (a) Direct callees. *)
          IF MSIR.InsnOp(ins) = MSIR.Op.Call OR
             MSIR.InsnOp(ins) = MSIR.Op.Invoke THEN
            MaybeAddExtern(m, externs, MSIR.InsnCallee(ins));
          END;
          (* (b) ConstProc operands — external procs used as function-pointer values. *)
          nop := MSIR.InsnOperandCount(ins);
          FOR oi := 0 TO nop - 1 DO
            v := MSIR.InsnOperand(ins, oi);
            IF v # NIL AND MSIR.GetValueKind(v) = MSIR.ValueKind.ConstProc THEN
              MaybeAddExtern(m, externs, MSIR.GetConstProc(v));
            END;
          END;
        END;
      END;
    END;
  END CollectExterns;

(*-------------------------------------------------- proc signature emit *)

PROCEDURE EmitParamList(wr: Wr.T;  p: MSIR.Proc) =
  VAR n := MSIR.ProcParamCount(p);
  BEGIN
    Wr.PutText(wr, "(");
    FOR i := 0 TO n - 1 DO
      IF i > 0 THEN Wr.PutText(wr, ", ") END;
      LLType(wr, MSIR.ValueType(MSIR.ProcParam(p, i)));
      Wr.PutText(wr, " ");
      LLOpVal(wr, MSIR.ProcParam(p, i));
    END;
    Wr.PutText(wr, ")");
  END EmitParamList;

PROCEDURE EmitParamTypeList(wr: Wr.T;  p: MSIR.Proc) =
  (* Just types, no names — for declare statements *)
  VAR n := MSIR.ProcParamCount(p);
  BEGIN
    Wr.PutText(wr, "(");
    FOR i := 0 TO n - 1 DO
      IF i > 0 THEN Wr.PutText(wr, ", ") END;
      LLType(wr, MSIR.ValueType(MSIR.ProcParam(p, i)));
    END;
    Wr.PutText(wr, ")");
  END EmitParamTypeList;

(*------------------------------------------------------- debug info *)

(* Split an absolute path into directory and basename.
   "/a/b/Main.m3" → dir="/a/b", base="Main.m3".
   A path with no slash → dir=".", base=path. *)
(* Map an MSIR TypeKind to a 0..NBT-1 basic-type slot, or -1 if unsupported.
   Ptr / GcRef / GcSlot map to the ADDRESS slot (slot 8); all other
   composite kinds return -1 so no type annotation is emitted.
   IMPORTANT: a DILocalVariable with no type combined with @llvm.dbg.declare
   crashes LLVM 22's DWARF emitter — every var that gets a declare call must
   have a type. *)
PROCEDURE BTypeIdx(kind: MSIR.TypeKind): INTEGER =
  BEGIN
    CASE kind OF
    | MSIR.TypeKind.I64                             => RETURN 0;   (* INTEGER *)
    | MSIR.TypeKind.W64                             => RETURN 1;   (* CARDINAL *)
    | MSIR.TypeKind.I32                             => RETURN 2;   (* INTEGER32 *)
    | MSIR.TypeKind.W32                             => RETURN 3;   (* CARDINAL32 *)
    | MSIR.TypeKind.I1                              => RETURN 4;   (* BOOLEAN *)
    | MSIR.TypeKind.F32                             => RETURN 5;   (* REAL *)
    | MSIR.TypeKind.F64                             => RETURN 6;   (* LONGREAL *)
    | MSIR.TypeKind.W8                              => RETURN 7;   (* CHAR *)
    | MSIR.TypeKind.Ptr,
      MSIR.TypeKind.GcRef,
      MSIR.TypeKind.GcSlot                          => RETURN 8;   (* ADDRESS *)
    ELSE                                               RETURN -1;
    END;
  END BTypeIdx;

(* Strip the leading '%' from an SSA name and the trailing '.slot' suffix
   used for spilled VALUE formals, yielding the M3 display name. *)
PROCEDURE StripVarName(ssaName: TEXT): TEXT =
  VAR n := ssaName;  len: INTEGER;
  BEGIN
    IF Text.Length(n) > 0 AND Text.GetChar(n, 0) = '%' THEN
      n := Text.Sub(n, 1);
    END;
    len := Text.Length(n);
    IF len > 5 AND Text.Equal(Text.Sub(n, len - 5), ".slot") THEN
      n := Text.Sub(n, 0, len - 5);
    END;
    (* Strip trailing .<digits> de-duplication suffix added by UniqueLocalName. *)
    len := Text.Length(n);
    IF len >= 2 THEN
      VAR i := len - 1;
      BEGIN
        WHILE i > 0 AND Text.GetChar(n, i) >= '0' AND Text.GetChar(n, i) <= '9' DO
          DEC(i);
        END;
        IF i > 0 AND i < len - 1 AND Text.GetChar(n, i) = '.' THEN
          n := Text.Sub(n, 0, i);
        END;
      END;
    END;
    RETURN n;
  END StripVarName;

(* Return TRUE for SSA names that are internal compiler temporaries and
   should not get DILocalVariable entries:
   - %__xxx  — double-underscore helpers (%__env, %__ll3, %__result_ptr …)
   - %t<digits>  — MSIR freshName-generated names (%t1, %t5 …) used for
                   unnamed allocas (closure env arrays, result-dispatch slots) *)
PROCEDURE IsInternalVarName(ssaName: TEXT): BOOLEAN =
  VAR len := Text.Length(ssaName);
  BEGIN
    IF len < 2 OR Text.GetChar(ssaName, 0) # '%' THEN RETURN FALSE END;
    (* %__xxx pattern *)
    IF len >= 3
       AND Text.GetChar(ssaName, 1) = '_'
       AND Text.GetChar(ssaName, 2) = '_' THEN
      RETURN TRUE;
    END;
    (* %t<digits> pattern — freshName temporaries *)
    IF Text.GetChar(ssaName, 1) = 't' AND len >= 3 THEN
      FOR k := 2 TO len - 1 DO
        IF Text.GetChar(ssaName, k) < '0' OR Text.GetChar(ssaName, k) > '9' THEN
          RETURN FALSE;
        END;
      END;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END IsInternalVarName;

(* Return the DILocalVariable metadata index for the alloca result value,
   or -1 if not recorded. *)
PROCEDURE GetDbgVarMetaIdx(allocaVal: MSIR.Value): INTEGER =
  BEGIN
    FOR j := 0 TO dbgVarN - 1 DO
      IF dbgVars[j].allocaVal = allocaVal THEN
        RETURN dbgVars[j].metaIdx;
      END;
    END;
    RETURN -1;
  END GetDbgVarMetaIdx;

(* Return an existing DbgLocEntry index for (spIdx, line), or allocate a new
   one at metaN (incrementing metaN) and return it.  Returns -1 if the table
   is full. *)
PROCEDURE GetOrAddDbgLoc(spIdx, line: INTEGER;  VAR metaN: INTEGER): INTEGER =
  BEGIN
    FOR k := 0 TO dbgLocN - 1 DO
      IF dbgLocs[k].spIdx = spIdx AND dbgLocs[k].line = line THEN
        RETURN dbgLocs[k].metaIdx;
      END;
    END;
    IF dbgLocN >= MaxDbgLocs THEN RETURN -1 END;
    VAR idx := metaN;
    BEGIN
      dbgLocs[dbgLocN].spIdx   := spIdx;
      dbgLocs[dbgLocN].line    := line;
      dbgLocs[dbgLocN].metaIdx := idx;
      INC(dbgLocN);
      INC(metaN);
      RETURN idx;
    END;
  END GetOrAddDbgLoc;

(* Return the pre-built DILocation metadata index for (proc, line),
   or -1 if no entry exists (i.e. BuildDebugInfo never saw that line). *)
PROCEDURE GetDbgLocMetaIdx(p: MSIR.Proc;  line: INTEGER): INTEGER =
  VAR spIdx := GetProcMetaIdx(p);
  BEGIN
    IF spIdx < 0 OR line <= 0 THEN RETURN -1 END;
    FOR k := 0 TO dbgLocN - 1 DO
      IF dbgLocs[k].spIdx = spIdx AND dbgLocs[k].line = line THEN
        RETURN dbgLocs[k].metaIdx;
      END;
    END;
    RETURN -1;
  END GetDbgLocMetaIdx;

(* Return the best !dbg metadata index for instruction i:
   - per-line DILocation when the instruction has a known srcLine, else
   - curDbgLocIdx (the line:0 fallback for the current proc). *)
PROCEDURE InsnDbgLocIdx(i: MSIR.Insn): INTEGER =
  VAR sl := MSIR.InsnSrcLine(i);
  BEGIN
    IF sl > 0 AND dbgEnabled AND curEmitProc # NIL THEN
      VAR idx := GetDbgLocMetaIdx(curEmitProc, sl);
      BEGIN
        IF idx >= 0 THEN RETURN idx END;
      END;
    END;
    RETURN curDbgLocIdx;
  END InsnDbgLocIdx;

(* Total bit size of an MSIR type for DWARF size: and member offset computation. *)
PROCEDURE TotalBitsOf(t: MSIR.T): INTEGER =
  VAR bw := MSIR.BitWidth(t);
  BEGIN
    IF t = NIL THEN RETURN Target.AddressSize() END;
    IF bw > 0 THEN RETURN bw END;
    CASE MSIR.Kind(t) OF
    | MSIR.TypeKind.I1 => RETURN 1;
    | MSIR.TypeKind.Ptr, MSIR.TypeKind.GcRef, MSIR.TypeKind.GcSlot =>
        RETURN Target.AddressSize();
    | MSIR.TypeKind.Struct =>
        VAR n := MSIR.StructFieldCount(t);  maxEnd := 0;
        BEGIN
          FOR i := 0 TO n - 1 DO
            VAR f  := MSIR.StructField(t, i);
                fe := f.offset + TotalBitsOf(f.type);
            BEGIN
              IF fe > maxEnd THEN maxEnd := fe END;
            END;
          END;
          RETURN maxEnd;
        END;
    | MSIR.TypeKind.FixedArray =>
        RETURN MSIR.FixedArrayLen(t) * TotalBitsOf(MSIR.FixedArrayElt(t));
    | MSIR.TypeKind.Object =>
        (* Walk the full super chain + own fields to find the last field end. *)
        VAR maxEnd := Target.AddressSize();  (* vtable ptr at offset 0 *)
            cur    := t;
        BEGIN
          WHILE cur # NIL DO
            FOR i := 0 TO MSIR.ObjectFieldCount(cur) - 1 DO
              VAR f  := MSIR.ObjectField(cur, i);
                  fe := f.offset + TotalBitsOf(f.type);
              BEGIN
                IF fe > maxEnd THEN maxEnd := fe END;
              END;
            END;
            cur := MSIR.ObjectSuper(cur);
          END;
          RETURN maxEnd;
        END;
    ELSE RETURN Target.AddressSize();
    END;
  END TotalBitsOf;

PROCEDURE GetOrBuildOpenArrayDvType(rank: INTEGER; VAR metaN: INTEGER): INTEGER =
  (* Build a DICompositeType(DW_TAG_structure_type) representing the open-array
     dope vector { ptr data, i64 count0, i64 count1, ... }.
     Deduplication is by rank since the dope layout depends only on rank. *)
  VAR eidx     : INTEGER;
      AP       := Target.AddressSize();
      totalBits := (1 + rank) * AP;
      addrRef  := dbgBtBase + 8;   (* ADDRESS *)
      intRef   := dbgBtBase + 0;   (* INTEGER *)
      nEmitted : INTEGER;
      cname    : TEXT;
  BEGIN
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 3 AND dbgTypes[k].totalBits = totalBits THEN
        RETURN dbgTypes[k].metaIdx;
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN addrRef END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := NIL;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := metaN;   INC(metaN);
    dbgTypes[eidx].baseTypeRef   := -1;
    dbgTypes[eidx].kind          := 3;   (* OpenArray dope vector *)
    dbgTypes[eidx].childBase     := dbgChildN;
    dbgTypes[eidx].totalBits     := totalBits;
    nEmitted := 0;
    (* Field 0: data pointer *)
    IF dbgChildN < MaxDbgChildren THEN
      dbgChildren[dbgChildN].kind    := 0;   (* member *)
      dbgChildren[dbgChildN].name    := "data";
      dbgChildren[dbgChildN].typeRef := addrRef;
      dbgChildren[dbgChildN].size    := AP;
      dbgChildren[dbgChildN].offset  := 0;
      dbgChildren[dbgChildN].metaIdx := metaN;
      INC(dbgChildN);  INC(metaN);  INC(nEmitted);
    END;
    (* Fields 1..rank: element counts *)
    FOR k := 0 TO rank - 1 DO
      IF dbgChildN < MaxDbgChildren THEN
        IF rank = 1 THEN cname := "count"
        ELSE cname := "count" & Fmt.Int(k)
        END;
        dbgChildren[dbgChildN].kind    := 0;
        dbgChildren[dbgChildN].name    := cname;
        dbgChildren[dbgChildN].typeRef := intRef;
        dbgChildren[dbgChildN].size    := AP;
        dbgChildren[dbgChildN].offset  := AP * (k + 1);
        dbgChildren[dbgChildN].metaIdx := metaN;
        INC(dbgChildN);  INC(metaN);  INC(nEmitted);
      END;
    END;
    dbgTypes[eidx].childCount := nEmitted;
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildOpenArrayDvType;

PROCEDURE GetOrBuildObjectStructType(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  (* Build a DW_TAG_structure_type for an OBJECT type body.
     Flattens the full super chain: __vtable at 0, then all inherited and own fields.
     Returns the metadata index of the DICompositeType node (kind=4).
     Deduplicates by MSIR.T pointer identity. *)
  VAR eidx     : INTEGER;
      AP       := Target.AddressSize();
      addrRef  := dbgBtBase + 8;  (* ADDRESS *)
      nEmitted : INTEGER;
      cur      : MSIR.T;
  BEGIN
    (* Dedup: check all existing kind=4 entries. *)
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 4 AND dbgTypes[k].msirType = t THEN
        RETURN dbgTypes[k].metaIdx;
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN addrRef END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := t;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := metaN;   INC(metaN);
    dbgTypes[eidx].baseTypeRef   := -1;
    dbgTypes[eidx].kind          := 4;  (* Object struct *)
    dbgTypes[eidx].totalBits     := TotalBitsOf(t);
    nEmitted := 0;
    (* Two-pass: pre-register all field types so their sub-children land before this
       object type's childBase in dbgChildren[], preventing metadata ID collisions. *)
    CONST MaxDepth = 32;
    CONST MaxF     = 256;
    VAR chain  : ARRAY [0..MaxDepth-1] OF MSIR.T;
        depth  : INTEGER := 0;
        trefs  : ARRAY [0..MaxF-1]    OF INTEGER;
        fi     : INTEGER := 0;
    BEGIN
      (* Build inheritance chain (leaf → root). *)
      cur := t;
      WHILE cur # NIL AND depth < MaxDepth DO
        chain[depth] := cur;  INC(depth);
        cur := MSIR.ObjectSuper(cur);
      END;
      (* Pass 1: pre-register all field types (root-first order). *)
      FOR lvl := depth - 1 TO 0 BY -1 DO
        cur := chain[lvl];
        FOR i := 0 TO MSIR.ObjectFieldCount(cur) - 1 DO
          IF fi < MaxF THEN
            VAR f := MSIR.ObjectField(cur, i);
            BEGIN
              trefs[fi] := GetDbgTypeRef(f.type, metaN);
              INC(fi);
            END;
          END;
        END;
      END;
      (* Pass 2: set childBase after nested types, then add vtable + field members. *)
      dbgTypes[eidx].childBase := dbgChildN;
      IF dbgChildN < MaxDbgChildren THEN
        dbgChildren[dbgChildN].kind    := 0;   (* member *)
        dbgChildren[dbgChildN].name    := "__vtable";
        dbgChildren[dbgChildN].typeRef := addrRef;
        dbgChildren[dbgChildN].size    := AP;
        dbgChildren[dbgChildN].offset  := 0;
        dbgChildren[dbgChildN].metaIdx := metaN;
        INC(dbgChildN);  INC(metaN);  INC(nEmitted);
      END;
      fi := 0;
      FOR lvl := depth - 1 TO 0 BY -1 DO
        cur := chain[lvl];
        FOR i := 0 TO MSIR.ObjectFieldCount(cur) - 1 DO
          VAR f := MSIR.ObjectField(cur, i);
          BEGIN
            IF fi < MaxF AND dbgChildN < MaxDbgChildren THEN
              dbgChildren[dbgChildN].kind    := 0;
              dbgChildren[dbgChildN].name    := f.name;
              dbgChildren[dbgChildN].typeRef := trefs[fi];
              dbgChildren[dbgChildN].size    := TotalBitsOf(f.type);
              dbgChildren[dbgChildN].offset  := f.offset;
              dbgChildren[dbgChildN].metaIdx := metaN;
              INC(dbgChildN);  INC(metaN);  INC(nEmitted);
            END;
            INC(fi);
          END;
        END;
      END;
    END;
    dbgTypes[eidx].childCount := nEmitted;
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildObjectStructType;

PROCEDURE GetOrBuildObjectPtrType(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  (* Build a DW_TAG_pointer_type → object struct for GcRef(Object(...)).
     Returns the metadata index of the DIDerivedType(pointer) node (kind=5).
     Deduplicates by the struct metaIdx it points to. *)
  VAR structIdx : INTEGER;
      eidx      : INTEGER;
  BEGIN
    structIdx := GetOrBuildObjectStructType(t, metaN);
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 5 AND dbgTypes[k].baseTypeRef = structIdx THEN
        RETURN dbgTypes[k].metaIdx;
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN dbgBtBase + 8 END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := t;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := -1;
    dbgTypes[eidx].baseTypeRef   := structIdx;
    dbgTypes[eidx].kind          := 5;  (* Object pointer *)
    dbgTypes[eidx].childBase     := 0;
    dbgTypes[eidx].childCount    := 0;
    dbgTypes[eidx].totalBits     := Target.AddressSize();
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildObjectPtrType;

PROCEDURE GetDbgTypeRef(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  (* Return the metadata index of the DWARF type for t.
     For basic scalar/pointer types returns a DIBasicType index.
     For Struct/FixedArray/Enum/OpenArray/Object builds and returns a composite.
     For unknown kinds returns ADDRESS as fallback (index 8 in basic table). *)
  VAR btI: INTEGER;
  BEGIN
    IF t = NIL OR dbgBtBase < 0 THEN RETURN -1 END;
    (* GcRef to Object: DW_TAG_pointer_type → object body struct. Must check
       before BTypeIdx so we don't short-circuit to ADDRESS. *)
    IF MSIR.Kind(t) = MSIR.TypeKind.GcRef
       AND MSIR.Kind(MSIR.EltType(t)) = MSIR.TypeKind.Object
    THEN
      RETURN GetOrBuildObjectPtrType(MSIR.EltType(t), metaN);
    END;
    btI := BTypeIdx(MSIR.Kind(t));
    IF btI >= 0 THEN RETURN dbgBtBase + btI END;
    CASE MSIR.Kind(t) OF
    | MSIR.TypeKind.Struct     => RETURN GetOrBuildStructType(t, metaN);
    | MSIR.TypeKind.FixedArray => RETURN GetOrBuildFixedArrayType(t, metaN);
    | MSIR.TypeKind.Enum       => RETURN GetOrBuildEnumType(t, metaN);
    | MSIR.TypeKind.OpenArray  => RETURN GetOrBuildOpenArrayDvType(MSIR.OpenArrayRank(t), metaN);
    ELSE RETURN dbgBtBase + 8;  (* ADDRESS fallback *)
    END;
  END GetDbgTypeRef;

PROCEDURE SameStructType(a, b: MSIR.T): BOOLEAN =
  VAR na := MSIR.StructName(a);  nb := MSIR.StructName(b);
  BEGIN
    IF a = b THEN RETURN TRUE END;   (* identical object *)
    IF MSIR.TypeUID(a) # 0 AND MSIR.TypeUID(a) = MSIR.TypeUID(b) THEN
      RETURN TRUE
    END;
    (* Name + field-count match: reliable for named M3 record types. *)
    IF na = NIL OR nb = NIL THEN RETURN FALSE END;
    RETURN Text.Equal(na, nb)
        AND MSIR.StructFieldCount(a) = MSIR.StructFieldCount(b);
  END SameStructType;

PROCEDURE GetOrBuildStructType(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  VAR eidx: INTEGER;
  BEGIN
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 0 AND SameStructType(dbgTypes[k].msirType, t) THEN
        RETURN dbgTypes[k].metaIdx
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN dbgBtBase + 8 END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := t;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := metaN;   INC(metaN);
    dbgTypes[eidx].baseTypeRef   := -1;
    dbgTypes[eidx].kind          := 0;  (* Struct *)
    (* Two-pass: pre-register all field types so their sub-children land before this
       struct's childBase in dbgChildren[], preventing metadata ID collisions when
       nested composite types (FixedArray, Enum) add their own children. *)
    CONST MaxF = 256;
    VAR n        := MSIR.StructFieldCount(t);
        nf       := MIN(n, MaxF);
        trefs    : ARRAY [0..MaxF-1] OF INTEGER;
        nEmitted : INTEGER := 0;
    BEGIN
      (* Pass 1: register all field types (may add their sub-children to dbgChildren). *)
      FOR i := 0 TO nf - 1 DO
        VAR f := MSIR.StructField(t, i);
        BEGIN
          trefs[i] := GetDbgTypeRef(f.type, metaN);
        END;
      END;
      (* Pass 2: set childBase after nested types, then add DIDerivedType members. *)
      dbgTypes[eidx].childBase := dbgChildN;
      FOR i := 0 TO nf - 1 DO
        IF dbgChildN < MaxDbgChildren THEN
          VAR f := MSIR.StructField(t, i);
          BEGIN
            dbgChildren[dbgChildN].kind    := 0;   (* member *)
            dbgChildren[dbgChildN].name    := f.name;
            dbgChildren[dbgChildN].typeRef := trefs[i];
            dbgChildren[dbgChildN].size    := TotalBitsOf(f.type);
            dbgChildren[dbgChildN].offset  := f.offset;
            dbgChildren[dbgChildN].metaIdx := metaN;
            INC(dbgChildN);  INC(metaN);  INC(nEmitted);
          END;
        END;
      END;
      dbgTypes[eidx].childCount := nEmitted;
      dbgTypes[eidx].totalBits  := TotalBitsOf(t);
    END;
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildStructType;

PROCEDURE GetOrBuildFixedArrayType(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  VAR eidx  : INTEGER;
      elt   := MSIR.FixedArrayElt(t);
      len   := MSIR.FixedArrayLen(t);
      lo    := MSIR.FixedArrayLo(t);
      eltRef: INTEGER;
      uid   := MSIR.TypeUID(t);
  BEGIN
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 1 THEN
        VAR kt := dbgTypes[k].msirType;
        BEGIN
          IF kt = t THEN RETURN dbgTypes[k].metaIdx END;
          IF uid # 0 AND MSIR.TypeUID(kt) = uid THEN
            RETURN dbgTypes[k].metaIdx
          END;
          IF MSIR.FixedArrayLen(kt) = len
             AND MSIR.FixedArrayLo(kt) = lo
             AND MSIR.FixedArrayElt(kt) = elt THEN
            RETURN dbgTypes[k].metaIdx
          END;
        END;
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN dbgBtBase + 8 END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := t;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := metaN;   INC(metaN);
    dbgTypes[eidx].kind          := 1;  (* FixedArray *)
    eltRef := GetDbgTypeRef(elt, metaN);   (* BEFORE childBase: element-type sub-children *)
    dbgTypes[eidx].baseTypeRef   := eltRef;  (* must not land in our [childBase..] range *)
    dbgTypes[eidx].childBase     := dbgChildN;
    (* One DISubrange child: count = len, lowerBound in offset field. *)
    IF dbgChildN < MaxDbgChildren THEN
      dbgChildren[dbgChildN].kind    := 1;   (* subrange *)
      dbgChildren[dbgChildN].count   := len;
      dbgChildren[dbgChildN].offset  := lo;  (* lower bound of index type *)
      dbgChildren[dbgChildN].metaIdx := metaN;
      INC(dbgChildN);  INC(metaN);
      dbgTypes[eidx].childCount := 1;
    ELSE
      dbgTypes[eidx].childCount := 0;
    END;
    dbgTypes[eidx].totalBits := TotalBitsOf(t);
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildFixedArrayType;

PROCEDURE GetOrBuildEnumType(t: MSIR.T; VAR metaN: INTEGER): INTEGER =
  VAR eidx  : INTEGER;
      n     := MSIR.EnumLabelCount(t);
      uid   := MSIR.TypeUID(t);
      nm    := MSIR.StructName(t);
  BEGIN
    FOR k := 0 TO dbgTypeN - 1 DO
      IF dbgTypes[k].kind = 2 THEN
        VAR kt := dbgTypes[k].msirType;
        BEGIN
          IF kt = t THEN RETURN dbgTypes[k].metaIdx END;
          IF uid # 0 AND MSIR.TypeUID(kt) = uid THEN RETURN dbgTypes[k].metaIdx END;
          IF nm # NIL AND Text.Equal(MSIR.StructName(kt), nm)
             AND MSIR.EnumLabelCount(kt) = n THEN
            RETURN dbgTypes[k].metaIdx
          END;
        END;
      END;
    END;
    IF dbgTypeN >= MaxDbgTypes THEN RETURN dbgBtBase + 8 END;
    eidx := dbgTypeN;  INC(dbgTypeN);
    dbgTypes[eidx].msirType      := t;
    dbgTypes[eidx].metaIdx       := metaN;   INC(metaN);
    dbgTypes[eidx].elemsTupleIdx := metaN;   INC(metaN);
    dbgTypes[eidx].baseTypeRef   := -1;
    dbgTypes[eidx].kind          := 2;  (* Enum *)
    dbgTypes[eidx].childBase     := dbgChildN;
    VAR nEmitted := 0;
    BEGIN
      FOR i := 0 TO n - 1 DO
        IF dbgChildN < MaxDbgChildren THEN
          dbgChildren[dbgChildN].kind    := 2;   (* enumerator *)
          dbgChildren[dbgChildN].name    := MSIR.EnumLabel(t, i);
          dbgChildren[dbgChildN].value   := i;   (* ordinal = position *)
          dbgChildren[dbgChildN].metaIdx := metaN;
          INC(dbgChildN);  INC(metaN);  INC(nEmitted);
        END;
      END;
      dbgTypes[eidx].childCount := nEmitted;
      dbgTypes[eidx].totalBits  := MSIR.BitWidth(t);
    END;
    RETURN dbgTypes[eidx].metaIdx;
  END GetOrBuildEnumType;

PROCEDURE SplitPath(path: TEXT;  VAR dir: TEXT;  VAR base: TEXT) =
  VAR last := -1;
      n    := Text.Length(path);
  BEGIN
    FOR i := 0 TO n - 1 DO
      IF Text.GetChar(path, i) = '/' THEN last := i END;
    END;
    IF last < 0 THEN
      dir  := ".";
      base := path;
    ELSE
      dir  := Text.Sub(path, 0, last);
      base := Text.Sub(path, last + 1);
    END;
  END SplitPath;

(* Return the metadata index for 'file', adding it if not yet seen.
   nextMetaN is incremented only when a new file is registered. *)
PROCEDURE GetOrAddDbgFile(file: TEXT;  VAR nextMetaN: INTEGER): INTEGER =
  BEGIN
    FOR i := 0 TO dbgFileN - 1 DO
      IF Text.Equal(dbgFiles[i].name, file) THEN
        RETURN dbgFiles[i].metaIdx;
      END;
    END;
    IF dbgFileN >= MaxDbgFiles THEN RETURN -1 END;
    VAR idx := nextMetaN;
    BEGIN
      dbgFiles[dbgFileN].name    := file;
      dbgFiles[dbgFileN].metaIdx := idx;
      INC(dbgFileN);
      INC(nextMetaN);
      RETURN idx;
    END;
  END GetOrAddDbgFile;

(* Return the DIFile metadata index for the given proc's srcFile,
   or dbgFiles[0].metaIdx as a fallback. *)
PROCEDURE GetProcFileIdx(p: MSIR.Proc): INTEGER =
  VAR f := MSIR.ProcSrcFile(p);
  BEGIN
    IF f # NIL THEN
      FOR i := 0 TO dbgFileN - 1 DO
        IF Text.Equal(dbgFiles[i].name, f) THEN RETURN dbgFiles[i].metaIdx END;
      END;
    END;
    IF dbgFileN > 0 THEN RETURN dbgFiles[0].metaIdx END;
    RETURN -1;
  END GetProcFileIdx;

(* Return the DISubprogram metadata index for proc p, or -1 if not recorded. *)
PROCEDURE GetProcMetaIdx(p: MSIR.Proc): INTEGER =
  BEGIN
    FOR i := 0 TO dbgEntryN - 1 DO
      IF dbgEntries[i].proc = p THEN RETURN dbgEntries[i].metaIdx END;
    END;
    RETURN -1;
  END GetProcMetaIdx;

(* Return the DILocation(line:0) index for proc p, or -1 if not recorded. *)
PROCEDURE GetProcLocIdx(p: MSIR.Proc): INTEGER =
  BEGIN
    FOR i := 0 TO dbgEntryN - 1 DO
      IF dbgEntries[i].proc = p THEN RETURN dbgEntries[i].locIdx END;
    END;
    RETURN -1;
  END GetProcLocIdx;

(* Pre-pass: assign metadata indices to all procs that have source locations,
   and scan alloca instructions to build DILocalVariable entries.

   Index layout (all module-global):
     0,1  = module flags (Dwarf version, Debug Info Version)
     2    = distinct !DICompileUnit
     3..3+F-1          = F !DIFile nodes
     3+F               = !DINamespace (one per module)
     3+F+1             = !{null}
     3+F+2             = !DISubroutineType(types: !{null})
     3+F+3..3+F+3+NBT-1 = NBT !DIBasicType nodes
     spBase=3+F+3+NBT  = distinct !DISubprogram[0]
     spBase+1          = !DILocation[0]
     spBase+2k, spBase+2k+1 = subprogram/location for k-th proc
     varBase = spBase+2*P  = !DILocalVariable[0..V-1]  *)
PROCEDURE BuildDebugInfo(m: MSIR.Module) =
  VAR metaN: INTEGER := 3;   (* 0,1,2 reserved for flags + CU *)
  BEGIN
    dbgEnabled  := FALSE;
    dbgEntryN   := 0;
    dbgFileN    := 0;
    dbgVarN     := 0;
    dbgLocN     := 0;
    dbgTypeN    := 0;
    dbgChildN   := 0;
    dbgNsIdx    := -1;
    dbgNlIdx    := -1;
    dbgStIdx    := -1;
    dbgBtBase   := -1;

    (* First pass: collect unique source files. *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR p := MSIR.ModuleProc(m, i);
          f := MSIR.ProcSrcFile(p);
          l := MSIR.ProcSrcLine(p);
      BEGIN
        IF f # NIL AND l > 0 THEN
          dbgEnabled := TRUE;
          EVAL GetOrAddDbgFile(f, metaN);
        END;
      END;
    END;
    IF NOT dbgEnabled THEN RETURN END;

    (* Reserve namespace, null-list, subroutine-type, and basic-type slots. *)
    dbgNsIdx  := metaN;  INC(metaN);
    dbgNlIdx  := metaN;  INC(metaN);
    dbgStIdx  := metaN;  INC(metaN);
    dbgBtBase := metaN;  INC(metaN, NBT);

    (* Second pass: assign per-proc DISubprogram + DILocation indices. *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR p := MSIR.ModuleProc(m, i);
          f := MSIR.ProcSrcFile(p);
          l := MSIR.ProcSrcLine(p);
      BEGIN
        IF f # NIL AND l > 0 AND dbgEntryN < MaxDbgEntries THEN
          dbgEntries[dbgEntryN].proc    := p;
          dbgEntries[dbgEntryN].metaIdx := metaN;  INC(metaN);
          dbgEntries[dbgEntryN].locIdx  := metaN;  INC(metaN);
          INC(dbgEntryN);
        END;
      END;
    END;

    (* Third pass: collect alloca instructions for DILocalVariable entries.
       GetDbgTypeRef builds composite DWARF types on demand, allocating metaN
       indices for Struct/FixedArray types and their child nodes.
       Only allocas whose element type maps to a DWARF type are tracked. *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR p       := MSIR.ModuleProc(m, i);
          spIdx   := GetProcMetaIdx(p);
          fileIdx := GetProcFileIdx(p);
          pline   := MSIR.ProcSrcLine(p);
      BEGIN
        IF spIdx >= 0 THEN
          FOR bi := 0 TO MSIR.ProcBlockCount(p) - 1 DO
            VAR b := MSIR.ProcBlock(p, bi);
            BEGIN
              FOR ii := 0 TO MSIR.BlockInsnCount(b) - 1 DO
                VAR insn := MSIR.BlockInsn(b, ii);
                BEGIN
                  IF MSIR.InsnOp(insn) = MSIR.Op.Alloca THEN
                    VAR res         := MSIR.InsnResult(insn);
                        nm          := MSIR.ValueName(res);
                        elt         := MSIR.InsnTargetType(insn);
                        typeMetaIdx := GetDbgTypeRef(elt, metaN);
                    BEGIN
                      (* Only track allocas with a known DWARF type.
                         A DILocalVariable without type: crashes LLVM 22's
                         DWARF emitter when paired with @llvm.dbg.declare. *)
                      IF NOT IsInternalVarName(nm)
                         AND typeMetaIdx >= 0
                         AND dbgVarN < MaxDbgVars
                      THEN
                        dbgVars[dbgVarN].allocaVal   := res;
                        dbgVars[dbgVarN].varName     := StripVarName(nm);
                        dbgVars[dbgVarN].spIdx       := spIdx;
                        dbgVars[dbgVarN].fileIdx     := fileIdx;
                        dbgVars[dbgVarN].line        := pline;
                        dbgVars[dbgVarN].typeMetaIdx := typeMetaIdx;
                        dbgVars[dbgVarN].metaIdx     := metaN;
                        dbgVars[dbgVarN].isParam     := FALSE;
                        INC(dbgVarN);
                        INC(metaN);
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    END;

    (* Third-pass addendum: track READONLY/VAR open-array formal params.
       These are ptr-to-dope-vector params; llvm.dbg.declare on the ptr value
       itself tells the debugger where the dope vector lives. *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR p       := MSIR.ModuleProc(m, i);
          spIdx   := GetProcMetaIdx(p);
          fileIdx := GetProcFileIdx(p);
          pline   := MSIR.ProcSrcLine(p);
      BEGIN
        IF spIdx >= 0 THEN
          FOR pi := 0 TO MSIR.ProcParamCount(p) - 1 DO
            VAR pval  := MSIR.ProcParam(p, pi);
                nm    := MSIR.ProcParamName(p, pi);
                ptype := MSIR.ValueType(pval);
                pmode := MSIR.ProcParamMode(p, pi);
                dvT   : MSIR.T;
                typeMetaIdx: INTEGER;
            BEGIN
              IF NOT IsInternalVarName("%" & nm)
                 AND (pmode = MSIR.ParamMode.Readonly OR pmode = MSIR.ParamMode.Var)
                 AND MSIR.Kind(ptype) = MSIR.TypeKind.Ptr
                 AND MSIR.Kind(MSIR.EltType(ptype)) = MSIR.TypeKind.OpenArray
                 AND dbgVarN < MaxDbgVars
              THEN
                dvT         := MSIR.EltType(ptype);
                typeMetaIdx := GetOrBuildOpenArrayDvType(MSIR.OpenArrayRank(dvT), metaN);
                dbgVars[dbgVarN].allocaVal   := pval;
                dbgVars[dbgVarN].varName     := nm;
                dbgVars[dbgVarN].spIdx       := spIdx;
                dbgVars[dbgVarN].fileIdx     := fileIdx;
                dbgVars[dbgVarN].line        := pline;
                dbgVars[dbgVarN].typeMetaIdx := typeMetaIdx;
                dbgVars[dbgVarN].metaIdx     := metaN;
                dbgVars[dbgVarN].isParam     := TRUE;
                INC(dbgVarN);  INC(metaN);
              END;
            END;
          END;
        END;
      END;
    END;

    (* Fourth pass: collect unique (spIdx, srcLine) pairs from all instructions.
       These become per-line DILocation nodes used as !dbg annotations. *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR p     := MSIR.ModuleProc(m, i);
          spIdx := GetProcMetaIdx(p);
      BEGIN
        IF spIdx >= 0 THEN
          FOR bi := 0 TO MSIR.ProcBlockCount(p) - 1 DO
            VAR b := MSIR.ProcBlock(p, bi);
            BEGIN
              FOR ii := 0 TO MSIR.BlockInsnCount(b) - 1 DO
                VAR insn := MSIR.BlockInsn(b, ii);
                    sl   := MSIR.InsnSrcLine(insn);
                BEGIN
                  IF sl > 0 THEN
                    EVAL GetOrAddDbgLoc(spIdx, sl, metaN);
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END BuildDebugInfo;

(* Emit all DWARF metadata nodes at the end of the module .ll output. *)
PROCEDURE EmitDebugMetadata(wr: Wr.T) =
  CONST
    (* name / DW_ATE encoding / bit size for the NBT fixed basic types.
       Indices must match BTypeIdx().
       Slot 8 = ADDRESS: used for Ptr/GcRef/GcSlot allocas — ensures every
       DILocalVariable that gets an @llvm.dbg.declare call has a type,
       which is required by LLVM 22's DWARF emitter. *)
    BName = ARRAY [0..NBT-1] OF TEXT {
      "INTEGER", "CARDINAL", "INTEGER32", "CARDINAL32",
      "BOOLEAN", "REAL",     "LONGREAL",  "CHAR",    "ADDRESS" };
    BAte  = ARRAY [0..NBT-1] OF TEXT {
      "DW_ATE_signed",      "DW_ATE_unsigned",     "DW_ATE_signed",       "DW_ATE_unsigned",
      "DW_ATE_boolean",     "DW_ATE_float",        "DW_ATE_float",        "DW_ATE_unsigned_char",
      "DW_ATE_address" };
    BBits = ARRAY [0..NBT-1] OF INTEGER { 64, 64, 32, 32, 1, 32, 64, 8, 64 };
  VAR cuFileIdx: INTEGER;
      dir, base: TEXT;
  BEGIN
    IF dbgFileN = 0 OR dbgEntryN = 0 THEN RETURN END;
    cuFileIdx := dbgFiles[0].metaIdx;
    Wr.PutText(wr, "\n; DWARF debug metadata\n");

    (* DISubprogram nodes: name = short display name, linkageName = mangled linker name,
       scope = DINamespace so LLDB exposes Module::Proc lookup. *)
    FOR k := 0 TO dbgEntryN - 1 DO
      VAR p       := dbgEntries[k].proc;
          spIdx   := dbgEntries[k].metaIdx;
          dispName := MSIR.ProcName(p);
          mangName := LLSymbol(p);
          fileIdx := GetProcFileIdx(p);
          line    := MSIR.ProcSrcLine(p);
      BEGIN
        Wr.PutText(wr, "!" & Fmt.Int(spIdx)
          & " = distinct !DISubprogram(name: \""     & dispName
          & "\", linkageName: \""                    & mangName
          & "\", scope: !"                           & Fmt.Int(dbgNsIdx)
          & ", file: !"                              & Fmt.Int(fileIdx)
          & ", line: "                               & Fmt.Int(line)
          & ", type: !"                              & Fmt.Int(dbgStIdx)
          & ", scopeLine: "                          & Fmt.Int(line)
          & ", unit: !2, spFlags: DISPFlagDefinition)\n");
      END;
    END;

    (* DILocation(line:0) nodes — one per proc, used as !dbg on call/invoke
       to satisfy LLVM's "inlinable call must have !dbg" check. *)
    FOR k := 0 TO dbgEntryN - 1 DO
      Wr.PutText(wr, "!" & Fmt.Int(dbgEntries[k].locIdx)
        & " = !DILocation(line: 0, column: 0, scope: !"
        & Fmt.Int(dbgEntries[k].metaIdx) & ")\n");
    END;

    (* DILocalVariable nodes — one per tracked alloca. *)
    FOR j := 0 TO dbgVarN - 1 DO
      VAR e := dbgVars[j];
      BEGIN
        Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
          & " = !DILocalVariable(name: \""   & e.varName
          & "\", scope: !"                   & Fmt.Int(e.spIdx)
          & ", file: !"                      & Fmt.Int(e.fileIdx)
          & ", line: "                       & Fmt.Int(e.line)
          & ", type: !"                      & Fmt.Int(e.typeMetaIdx)
          & ")\n");
      END;
    END;

    (* Phase 4: composite type nodes — !DICompositeType + elements tuple + children. *)
    FOR k := 0 TO dbgTypeN - 1 DO
      VAR e  := dbgTypes[k];
          nm : TEXT := NIL;
      BEGIN
        IF dbgTypes[k].msirType # NIL THEN
          nm := MSIR.StructName(dbgTypes[k].msirType);
        END;
        IF nm = NIL THEN nm := "" END;
        IF e.kind = 0 THEN
          (* Struct: DW_TAG_structure_type with per-field DIDerivedType children *)
          Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
            & " = !DICompositeType(tag: DW_TAG_structure_type"
            & ", name: \""  & nm & "\""
            & ", size: "    & Fmt.Int(e.totalBits)
            & ", elements: !" & Fmt.Int(e.elemsTupleIdx) & ")\n");
          (* elements tuple *)
          Wr.PutText(wr, "!" & Fmt.Int(e.elemsTupleIdx) & " = !{");
          FOR c := 0 TO e.childCount - 1 DO
            IF c > 0 THEN Wr.PutText(wr, ", ") END;
            Wr.PutText(wr, "!" & Fmt.Int(dbgChildren[e.childBase + c].metaIdx));
          END;
          Wr.PutText(wr, "}\n");
          (* member nodes *)
          FOR c := 0 TO e.childCount - 1 DO
            VAR ch := dbgChildren[e.childBase + c];
                chName := ch.name;
            BEGIN
              IF chName = NIL THEN chName := "" END;
              Wr.PutText(wr, "!" & Fmt.Int(ch.metaIdx)
                & " = !DIDerivedType(tag: DW_TAG_member"
                & ", name: \""  & chName & "\"");
              IF ch.typeRef >= 0 THEN
                Wr.PutText(wr, ", baseType: !" & Fmt.Int(ch.typeRef));
              END;
              Wr.PutText(wr, ", size: "   & Fmt.Int(ch.size)
                & ", offset: " & Fmt.Int(ch.offset) & ")\n");
            END;
          END;
        ELSIF e.kind = 1 THEN
          (* FixedArray: DW_TAG_array_type with one DISubrange child *)
          Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
            & " = !DICompositeType(tag: DW_TAG_array_type");
          IF e.baseTypeRef >= 0 THEN
            Wr.PutText(wr, ", baseType: !" & Fmt.Int(e.baseTypeRef));
          END;
          Wr.PutText(wr, ", size: "    & Fmt.Int(e.totalBits)
            & ", elements: !" & Fmt.Int(e.elemsTupleIdx) & ")\n");
          (* elements tuple *)
          Wr.PutText(wr, "!" & Fmt.Int(e.elemsTupleIdx) & " = !{");
          IF e.childCount > 0 THEN
            Wr.PutText(wr, "!" & Fmt.Int(dbgChildren[e.childBase].metaIdx));
          END;
          Wr.PutText(wr, "}\n");
          (* single DISubrange child *)
          IF e.childCount > 0 THEN
            VAR ch := dbgChildren[e.childBase];
            BEGIN
              Wr.PutText(wr, "!" & Fmt.Int(ch.metaIdx)
                & " = !DISubrange(count: " & Fmt.Int(ch.count));
              IF ch.offset # 0 THEN
                Wr.PutText(wr, ", lowerBound: " & Fmt.Int(ch.offset));
              END;
              Wr.PutText(wr, ")\n");
            END;
          END;
        ELSIF e.kind = 2 THEN
          (* Enum: DW_TAG_enumeration_type with DIEnumerator children *)
          Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
            & " = !DICompositeType(tag: DW_TAG_enumeration_type"
            & ", name: \""    & nm & "\""
            & ", size: "      & Fmt.Int(e.totalBits)
            & ", elements: !" & Fmt.Int(e.elemsTupleIdx) & ")\n");
          Wr.PutText(wr, "!" & Fmt.Int(e.elemsTupleIdx) & " = !{");
          FOR c := 0 TO e.childCount - 1 DO
            IF c > 0 THEN Wr.PutText(wr, ", ") END;
            Wr.PutText(wr, "!" & Fmt.Int(dbgChildren[e.childBase + c].metaIdx));
          END;
          Wr.PutText(wr, "}\n");
          FOR c := 0 TO e.childCount - 1 DO
            VAR ch := dbgChildren[e.childBase + c];
            BEGIN
              Wr.PutText(wr, "!" & Fmt.Int(ch.metaIdx)
                & " = !DIEnumerator(name: \"" & ch.name & "\""
                & ", value: " & Fmt.Int(ch.value) & ")\n");
            END;
          END;
        ELSIF e.kind = 3 THEN
          (* kind = 3: OpenArray dope-vector — DW_TAG_structure_type with
             {data: ADDRESS, count: INTEGER} or {data, count0, count1, ...} fields *)
          VAR rank := e.totalBits DIV Target.AddressSize() - 1;
          BEGIN
            Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
              & " = !DICompositeType(tag: DW_TAG_structure_type"
              & ", name: \"__dope_" & Fmt.Int(rank) & "\""
              & ", size: "          & Fmt.Int(e.totalBits)
              & ", elements: !"     & Fmt.Int(e.elemsTupleIdx) & ")\n");
            Wr.PutText(wr, "!" & Fmt.Int(e.elemsTupleIdx) & " = !{");
            FOR c := 0 TO e.childCount - 1 DO
              IF c > 0 THEN Wr.PutText(wr, ", ") END;
              Wr.PutText(wr, "!" & Fmt.Int(dbgChildren[e.childBase + c].metaIdx));
            END;
            Wr.PutText(wr, "}\n");
            FOR c := 0 TO e.childCount - 1 DO
              VAR ch := dbgChildren[e.childBase + c];
              BEGIN
                Wr.PutText(wr, "!" & Fmt.Int(ch.metaIdx)
                  & " = !DIDerivedType(tag: DW_TAG_member"
                  & ", name: \""  & ch.name & "\""
                  & ", baseType: !" & Fmt.Int(ch.typeRef)
                  & ", size: "    & Fmt.Int(ch.size)
                  & ", offset: "  & Fmt.Int(ch.offset) & ")\n");
              END;
            END;
          END;
        ELSIF e.kind = 4 THEN
          (* kind = 4: Object body — DW_TAG_structure_type with vtable ptr + all fields. *)
          VAR objName := MSIR.ObjectName(e.msirType);
          BEGIN
            IF objName = NIL THEN objName := "OBJECT" END;
            Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
              & " = !DICompositeType(tag: DW_TAG_structure_type"
              & ", name: \""    & objName & "\""
              & ", size: "      & Fmt.Int(e.totalBits)
              & ", elements: !" & Fmt.Int(e.elemsTupleIdx) & ")\n");
            Wr.PutText(wr, "!" & Fmt.Int(e.elemsTupleIdx) & " = !{");
            FOR c := 0 TO e.childCount - 1 DO
              IF c > 0 THEN Wr.PutText(wr, ", ") END;
              Wr.PutText(wr, "!" & Fmt.Int(dbgChildren[e.childBase + c].metaIdx));
            END;
            Wr.PutText(wr, "}\n");
            FOR c := 0 TO e.childCount - 1 DO
              VAR ch := dbgChildren[e.childBase + c];
              BEGIN
                Wr.PutText(wr, "!" & Fmt.Int(ch.metaIdx)
                  & " = !DIDerivedType(tag: DW_TAG_member"
                  & ", name: \""  & ch.name & "\""
                  & ", baseType: !" & Fmt.Int(ch.typeRef)
                  & ", size: "    & Fmt.Int(ch.size)
                  & ", offset: "  & Fmt.Int(ch.offset) & ")\n");
              END;
            END;
          END;
        ELSIF e.kind = 5 THEN
          (* kind = 5: Object pointer — DW_TAG_pointer_type → object body struct. *)
          Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
            & " = !DIDerivedType(tag: DW_TAG_pointer_type"
            & ", baseType: !" & Fmt.Int(e.baseTypeRef)
            & ", size: "      & Fmt.Int(Target.AddressSize()) & ")\n");
        END;
      END;
    END;

    (* Per-line DILocation nodes (Phase 3 — one per unique (subprogram, line) pair). *)
    FOR k := 0 TO dbgLocN - 1 DO
      VAR e := dbgLocs[k];
      BEGIN
        Wr.PutText(wr, "!" & Fmt.Int(e.metaIdx)
          & " = !DILocation(line: " & Fmt.Int(e.line)
          & ", column: 0, scope: !"  & Fmt.Int(e.spIdx) & ")\n");
      END;
    END;

    (* DIFile nodes. *)
    FOR j := 0 TO dbgFileN - 1 DO
      SplitPath(dbgFiles[j].name, dir, base);
      Wr.PutText(wr, "!" & Fmt.Int(dbgFiles[j].metaIdx)
        & " = !DIFile(filename: \"" & base
        & "\", directory: \""       & dir & "\")\n");
    END;

    (* DINamespace — wraps all procs in the module so LLDB exposes Module::Proc. *)
    Wr.PutText(wr, "!" & Fmt.Int(dbgNsIdx)
      & " = !DINamespace(name: \""  & MSIR.ModuleName(curEmitModule)
      & "\", scope: !2)\n");

    (* Null-list and shared void-return subroutine type. *)
    Wr.PutText(wr, "!" & Fmt.Int(dbgNlIdx) & " = !{null}\n");
    Wr.PutText(wr, "!" & Fmt.Int(dbgStIdx)
      & " = !DISubroutineType(types: !" & Fmt.Int(dbgNlIdx) & ")\n");

    (* Fixed set of DIBasicType nodes (indices BTypeIdx → btBase+k). *)
    FOR k := 0 TO NBT - 1 DO
      Wr.PutText(wr, "!" & Fmt.Int(dbgBtBase + k)
        & " = !DIBasicType(name: \""  & BName[k]
        & "\", size: "                & Fmt.Int(BBits[k])
        & ", encoding: "              & BAte[k]  & ")\n");
    END;

    (* Compile unit and module-level flags. *)
    Wr.PutText(wr, "!2 = distinct !DICompileUnit("
      & "language: DW_LANG_Modula3, file: !" & Fmt.Int(cuFileIdx)
      & ", producer: \"CM3 MSIR\", isOptimized: false"
      & ", runtimeVersion: 0, emissionKind: FullDebug)\n");
    Wr.PutText(wr, "!0 = !{i32 2, !\"Dwarf Version\", i32 4}\n");
    Wr.PutText(wr, "!1 = !{i32 2, !\"Debug Info Version\", i32 3}\n");
    Wr.PutText(wr, "!llvm.module.flags = !{!0, !1}\n");
    Wr.PutText(wr, "!llvm.dbg.cu = !{!2}\n");
  END EmitDebugMetadata;

(*------------------------------------------------------- proc emission *)

PROCEDURE HasInvoke(p: MSIR.Proc): BOOLEAN =
  VAR nb := MSIR.ProcBlockCount(p);
  BEGIN
    FOR bi := 0 TO nb - 1 DO
      VAR
        b  := MSIR.ProcBlock(p, bi);
        ni := MSIR.BlockInsnCount(b);
      BEGIN
        FOR ii := 0 TO ni - 1 DO
          CASE MSIR.InsnOp(MSIR.BlockInsn(b, ii)) OF
          | MSIR.Op.Invoke, MSIR.Op.InvokeIndirect,
            MSIR.Op.LandingPad, MSIR.Op.Resume =>
              RETURN TRUE;
          ELSE
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END HasInvoke;

PROCEDURE EmitProc(wr: Wr.T;  p: MSIR.Proc) =
  VAR
    nb    := MSIR.ProcBlockCount(p);
    rtype := MSIR.ProcResultType(p);
  BEGIN
    auxN := 0;   (* reset per-function auxiliary counter *)

    IF nb = 0 THEN
      (* External declaration — no body *)
      Wr.PutText(wr, "declare ");
      LLType(wr, rtype);
      Wr.PutText(wr, " @");
      Wr.PutText(wr, LLSymbol(p));
      EmitParamTypeList(wr, p);
      Wr.PutText(wr, "\n");
      RETURN;
    END;

    Wr.PutText(wr, "\ndefine ");
    IF MSIR.ProcGetLinkage(p) = MSIR.Linkage.Internal THEN
      Wr.PutText(wr, "internal ");
    END;
    LLType(wr, rtype);
    Wr.PutText(wr, " @");
    Wr.PutText(wr, LLSymbol(p));
    EmitParamList(wr, p);
    (* Always emit personality so C++ exceptions can propagate through MSIR
       frames without triggering terminate() in nounwind functions. *)
    Wr.PutText(wr, " personality ptr @__gxx_personality_v0");
    VAR dbgIdx := GetProcMetaIdx(p); BEGIN
      IF dbgIdx >= 0 THEN
        Wr.PutText(wr, " !dbg !" & Fmt.Int(dbgIdx));
      END;
    END;
    Wr.PutText(wr, " {\n");

    curDbgLocIdx := GetProcLocIdx(p);
    curEmitProc  := p;

    (* Inline the entry block (block 0) so param declares land inside it.
       @llvm.dbg.declare instructions must be inside a basic block, not before
       the first block label. *)
    VAR b0 := MSIR.ProcBlock(p, 0);
        n0 := MSIR.BlockInsnCount(b0);
    BEGIN
      Wr.PutText(wr, MSIR.BlockLabel(b0) & ":\n");
      (* Emit @llvm.dbg.declare for open-array param formals (ptr-typed params,
         no alloca — declare points directly at the param value). *)
      IF dbgEnabled THEN
        VAR procSpIdx  := GetProcMetaIdx(p);
            procLocIdx := curDbgLocIdx;
        BEGIN
          IF procSpIdx >= 0 AND procLocIdx >= 0 THEN
            FOR j := 0 TO dbgVarN - 1 DO
              IF dbgVars[j].isParam AND dbgVars[j].spIdx = procSpIdx THEN
                Wr.PutText(wr, "  call void @llvm.dbg.declare(metadata ptr ");
                LLOpVal(wr, dbgVars[j].allocaVal);
                Wr.PutText(wr, ", metadata !" & Fmt.Int(dbgVars[j].metaIdx)
                  & ", metadata !DIExpression()), !dbg !"
                  & Fmt.Int(procLocIdx) & "\n");
              END;
            END;
          END;
        END;
      END;
      FOR k := 0 TO n0 - 1 DO
        EmitInsn(wr, MSIR.BlockInsn(b0, k));
      END;
      IF n0 = 0 OR NOT MSIR.BlockIsTerminated(b0) THEN
        Wr.PutText(wr, "  unreachable\n");
      END;
    END;

    FOR bi := 1 TO nb - 1 DO
      EmitBlock(wr, MSIR.ProcBlock(p, bi));
    END;
    curDbgLocIdx := -1;
    curEmitProc  := NIL;

    Wr.PutText(wr, "}\n");
  END EmitProc;

(*----------------------------------------------------- global emission *)

PROCEDURE EmitGlobal(wr: Wr.T;  g: MSIR.Global;  m: MSIR.Module) =
  VAR t := MSIR.GlobalType(g);
  BEGIN
    IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN
      (* Struct-embedded: alias is emitted by the typed-alias loop; skip here. *)
      RETURN;
    END;
    Wr.PutText(wr, "@");
    IF MSIR.GlobalIsExternal(g) THEN
      Wr.PutText(wr, MSIR.GlobalName(g));
      Wr.PutText(wr, " = external global ");
      IF MSIR.GlobalIsTraced(g) OR t = NIL OR MSIR.Kind(t) = MSIR.TypeKind.Void THEN
        Wr.PutText(wr, "ptr");
      ELSE
        LLType(wr, t);
      END;
    ELSE
      Wr.PutText(wr, MSIR.GlobalName(g));
      (* An interface variable's backing global is emitted by BOTH the interface
         unit and its same-name implementation module (MODULE Z EXPORTS Z) — two
         strong defs are a link-time duplicate (e.g. RTHeapRep.align in m3core).
         Such globals are flagged weak by the front end (GlobalSetWeak) so the
         interface's strong def wins and the module's is a safe BSS fallback.
         Module-PRIVATE globals are NOT flagged, so they stay strong and remain
         externally referenceable (e.g. a C test harness referencing Main.gCounter). *)
      IF MSIR.GlobalIsWeak(g)
        THEN Wr.PutText(wr, " = weak global ");
        ELSE Wr.PutText(wr, " = global ");
      END;
      VAR nb := MSIR.GlobalBackingBytes(g);
      BEGIN
        IF nb > 0 THEN
          (* Indirect-global backing storage: emit as a byte array of the
             declared size (zero-initialized, matching the CG's BSS bss_var). *)
          Wr.PutText(wr, "[" & Fmt.Int(nb) & " x i8] zeroinitializer");
        ELSIF MSIR.GlobalIsTraced(g) THEN
          Wr.PutText(wr, "ptr null");  (* traced ref slot starts as null ptr *)
        ELSIF t = NIL OR MSIR.Kind(t) = MSIR.TypeKind.Void THEN
          Wr.PutText(wr, "ptr null");  (* void-typed untraced slot — treat as ptr *)
        ELSE
          LLType(wr, t);
          Wr.PutText(wr, " zeroinitializer");
        END;
      END;
    END;
    Wr.PutText(wr, "\n");
  END EmitGlobal;

(*------------------------------------------------------- declare emit *)

PROCEDURE EmitDeclare(wr: Wr.T;  p: MSIR.Proc) =
  VAR rtype := MSIR.ProcResultType(p);
  BEGIN
    Wr.PutText(wr, "declare ");
    LLType(wr, rtype);
    Wr.PutText(wr, " @");
    Wr.PutText(wr, LLSymbol(p));
    EmitParamTypeList(wr, p);
    Wr.PutText(wr, "\n");
  END EmitDeclare;

PROCEDURE EmitDeclareFromSeq(wr: Wr.T;  seq: RefSeq.T;  i: INTEGER) =
  (* Wrapper that isolates the implicit NARROW (REFANY → MSIR.Proc) from RefSeq.get
     into its own procedure.  Two NARROWs in the same LLVM function produce duplicate
     %narrow.chk SSA names; moving one to a callee avoids the collision until the
     permanent fix (unique names in Narrow.m3) is compiled into cm3. *)
  VAR p: MSIR.Proc := seq.get(i);
  BEGIN
    EmitDeclare(wr, p);
  END EmitDeclareFromSeq;

(*----------------------------------------------- TypeCell / ObjectTypeCell emission *)

PROCEDURE EmitTextLiterals(wr: Wr.T;  m: MSIR.Module) =
  (* Emit TextLiteral.T globals for every string literal in the module.
     Layout of each @textlit_N:
       { i{AP} gc_header, ptr method_list, i{IP} cnt, [len+1 x i8] chars }
     Literal data comes from TextExpr.LiteralCount/Chars/Cnt — the same
     per-module registry the CG path uses (SetUID tracking). *)
  VAR
    GcHeader := Word.Shift(TEXT_typecode, RH_typecode_offset);
    ip       := "i" & Fmt.Int(Target.IntegerSize());
    ap       := "i" & Fmt.Int(Target.AddressSize());
  VAR n := MSIR.ModuleTextLitCount(m);
  BEGIN
    IF n = 0 THEN RETURN END;
    Wr.PutText(wr, "\n; TEXT literal globals\n");

    (* declare lines for the 5 TextLiteral vtable methods.
       Names come from the MSIR.Proc stubs set by MSIREmit via RunTyme.LookUpProc;
       the stubs are also added to the externs list in CollectExterns so
       EmitDeclare emits their full signatures. *)

    Wr.PutText(wr, "@textlit_methods = internal constant [5 x ptr] [\n");
    FOR i := 0 TO 4 DO
      VAR p    := MSIR.ModuleGetTextLitHook(m, i);
          name : TEXT;
      BEGIN
        IF p = NIL
          THEN name := "<nil-textlit-hook-" & Fmt.Int(i) & ">";
          ELSE name := LLSymbol(p);
        END;
        Wr.PutText(wr, "  ptr @" & name);
      END;
      IF i < 4 THEN Wr.PutText(wr, ",") END;
      Wr.PutText(wr, "\n");
    END;
    Wr.PutText(wr, "]\n");

    FOR uid := 0 TO n - 1 DO
      VAR
        chars      := MSIR.ModuleTextLitChars(m, uid);
        cnt        := MSIR.ModuleTextLitCnt(m, uid);
        len        := ABS(cnt);
        wide       := cnt < 0;
        wcharBytes : INTEGER;
        byteCount  : INTEGER;
      BEGIN
        IF wide
          THEN wcharBytes := Target.WideCharSize() DIV Target.Char.size;
          ELSE wcharBytes := 1;
        END;
        byteCount := len * wcharBytes + wcharBytes;
        Wr.PutText(wr, "@textlit_" & Fmt.Int(uid) & " = internal constant { " & ap & ", ptr, " & ip & ", ["
                       & Fmt.Int(byteCount) & " x i8] } { " & ap & " "
                       & Fmt.Int(GcHeader)
                       & ", ptr @textlit_methods, " & ip & " " & Fmt.Int(cnt) & ", ["
                       & Fmt.Int(byteCount) & " x i8] c\"");
        (* Emit body bytes: for 8-bit, 1 byte/char; for wide, wcharBytes bytes/char *)
        FOR j := 0 TO len * wcharBytes - 1 DO
          VAR c := ORD(Text.GetChar(chars, j));
          BEGIN
            IF c >= 32 AND c < 127 AND c # ORD('"') AND c # ORD('\\') THEN
              Wr.PutChar(wr, VAL(c, CHAR));
            ELSE
              Wr.PutText(wr, "\\");
              Wr.PutText(wr, Fmt.Pad(Fmt.Unsigned(c, 16), 2, '0'));
            END;
          END;
        END;
        (* Null terminator: wcharBytes zero bytes *)
        FOR k := 0 TO wcharBytes - 1 DO Wr.PutText(wr, "\\00") END;
        Wr.PutText(wr, "\" }\n");
      END;
    END;
  END EmitTextLiterals;

PROCEDURE EmitConstArrays(wr: Wr.T;  m: MSIR.Module) =
  VAR n := MSIR.ModuleConstArrayCount(m);
  BEGIN
    IF n = 0 THEN RETURN END;
    Wr.PutText(wr, "\n; CONST array globals\n");
    FOR i := 0 TO n - 1 DO
      VAR
        ca  := MSIR.ModuleConstArray(m, i);
        cnt := MSIR.ConstArrayEltCount(ca);
      BEGIN
        Wr.PutText(wr, "@" & MSIR.ConstArrayName(ca)
                       & " = private constant [" & Fmt.Int(cnt) & " x ");
        LLType(wr, MSIR.ConstArrayEltType(ca));
        Wr.PutText(wr, "] [");
        IF cnt = 0 THEN
          Wr.PutText(wr, "]\n");
        ELSE
          Wr.PutText(wr, "\n");
          FOR j := 0 TO cnt - 1 DO
            Wr.PutText(wr, "  ");
            LLTypedVal(wr, MSIR.ConstArrayElt(ca, j));
            IF j < cnt - 1 THEN Wr.PutText(wr, ",") END;
            Wr.PutText(wr, "\n");
          END;
          Wr.PutText(wr, "]\n");
        END;
      END;
    END;
  END EmitConstArrays;

(* FieldKind: abstract category driving the RT0 struct layout walker.
   Walker converts each kind to its LLVM type and natural-alignment size,
   inserting [N x i8] padding fields where the ABI requires it. *)
TYPE FieldKind = { I8, I64, IP, Ptr };

(* Logical field indices for RT0.Typecell — no slots for padding. *)
CONST
  TC_typecode   = 0;   TC_selfID    = 1;   TC_fp         = 2;
  TC_traced     = 3;   TC_kind      = 4;   TC_link_state = 5;
  TC_dataAlign  = 6;   TC_dataSize  = 7;   TC_type_map   = 8;
  TC_gc_map     = 9;   TC_type_desc = 10;  TC_initProc   = 11;
  TC_brand_ptr  = 12;  TC_name      = 13;  TC_next       = 14;
  TC_nBase      = 15;

(* RT0.ObjectTypecell extension field indices (relative to TC_nBase). *)
CONST
  OTCe_parentID       = 0;
  OTCe_linkProc       = 1;
  OTCe_dataOffset     = 2;
  OTCe_methodOffset   = 3;
  OTCe_methodSize     = 4;
  OTCe_defaultMethods = 5;
  OTCe_parent         = 6;
  OTC_nExt            = 7;

(* RT0.ArrayTypecell extension field indices (relative to TC_nBase). *)
CONST
  ATCe_nDimensions = 0;
  ATCe_elementSize = 1;
  ATC_nExt         = 2;

(* RT0.TypeLink field indices. *)
CONST
  TL_defn     = 0;
  TL_typecode = 1;
  TL_nFields  = 2;

(* Field-kind arrays: one entry per logical field, parallel to field values.
   Walker inserts alignment padding between fields automatically. *)
CONST TCKinds = ARRAY [0 .. TC_nBase - 1] OF FieldKind {
  FieldKind.IP,   (* typecode *)
  FieldKind.IP,   (* selfID *)
  FieldKind.I64,  (* fp *)
  FieldKind.I8,   (* traced *)
  FieldKind.I8,   (* kind *)
  FieldKind.I8,   (* link_state *)
  FieldKind.I8,   (* dataAlignment *)
  FieldKind.IP,   (* dataSize *)
  FieldKind.Ptr,  (* type_map *)
  FieldKind.Ptr,  (* gc_map *)
  FieldKind.Ptr,  (* type_desc *)
  FieldKind.Ptr,  (* initProc *)
  FieldKind.Ptr,  (* brand_ptr *)
  FieldKind.Ptr,  (* name *)
  FieldKind.Ptr   (* next *)
};

CONST OTCExtKinds = ARRAY [0 .. OTC_nExt - 1] OF FieldKind {
  FieldKind.IP,   (* parentID *)
  FieldKind.Ptr,  (* linkProc *)
  FieldKind.IP,   (* dataOffset *)
  FieldKind.IP,   (* methodOffset *)
  FieldKind.IP,   (* methodSize *)
  FieldKind.Ptr,  (* defaultMethods *)
  FieldKind.Ptr   (* parent *)
};

CONST ATCExtKinds = ARRAY [0 .. ATC_nExt - 1] OF FieldKind {
  FieldKind.IP,   (* nDimensions *)
  FieldKind.IP    (* elementSize *)
};

CONST TLKinds = ARRAY [0 .. TL_nFields - 1] OF FieldKind {
  FieldKind.Ptr,  (* defn *)
  FieldKind.IP    (* typecode *)
};

(* Render the 64-bit fingerprint from a TypeDesc as an unsigned hex LLVM i64.
   Bytes are stored little-endian (byte[0]=LSB); emit byte[7..0] MSB-first.
   Uses the LLVM IR u0x prefix for unsigned hex integer constants. *)
PROCEDURE FPHex(d: MSIR.TypeDesc): TEXT =
  VAR s := "u0x";
  BEGIN
    FOR i := 7 TO 0 BY -1 DO
      s := s & Fmt.Pad(Fmt.Int(MSIR.TypeDescFPByte(d, i), 16), 2, '0');
    END;
    RETURN s;
  END FPHex;

PROCEDURE RTFieldSize(fk: FieldKind): INTEGER =
  BEGIN
    CASE fk OF
    | FieldKind.I8  => RETURN 1;
    | FieldKind.I64 => RETURN 8;
    | FieldKind.IP  => RETURN Target.IntegerBytes();
    | FieldKind.Ptr => RETURN Target.AddressBytes();
    END;
  END RTFieldSize;

PROCEDURE RTFieldLLType(fk: FieldKind): TEXT =
  BEGIN
    CASE fk OF
    | FieldKind.I8  => RETURN "i8";
    | FieldKind.I64 => RETURN "i64";
    | FieldKind.IP  => RETURN "i" & Fmt.Int(Target.IntegerSize());
    | FieldKind.Ptr => RETURN "ptr";
    END;
  END RTFieldLLType;

PROCEDURE EmitRTStructType(wr: Wr.T; name: TEXT;
                            READONLY kinds: ARRAY OF FieldKind) =
  BEGIN
    EmitRTStructTypeExt(wr, name, kinds, SUBARRAY(kinds, 0, 0));
  END EmitRTStructType;

(* Emit an LLVM named struct type for base + optional extension fields,
   inserting [N x i8] padding to match the C ABI natural-alignment layout. *)
PROCEDURE EmitRTStructTypeExt(wr: Wr.T; name: TEXT;
                               READONLY base: ARRAY OF FieldKind;
                               READONLY ext:  ARRAY OF FieldKind) =
  VAR off := 0; first := TRUE;
  BEGIN
    Wr.PutText(wr, "%" & name & " = type { ");
    FOR i := 0 TO LAST(base) DO
      VAR fk := base[i]; sz := RTFieldSize(fk); pad := (-off) MOD sz;
      BEGIN
        IF pad > 0 THEN
          IF NOT first THEN Wr.PutText(wr, ", ") END;
          Wr.PutText(wr, "[" & Fmt.Int(pad) & " x i8]");
          first := FALSE; INC(off, pad);
        END;
        IF NOT first THEN Wr.PutText(wr, ", ") END;
        Wr.PutText(wr, RTFieldLLType(fk));
        first := FALSE; INC(off, sz);
      END;
    END;
    FOR i := 0 TO LAST(ext) DO
      VAR fk := ext[i]; sz := RTFieldSize(fk); pad := (-off) MOD sz;
      BEGIN
        IF pad > 0 THEN
          IF NOT first THEN Wr.PutText(wr, ", ") END;
          Wr.PutText(wr, "[" & Fmt.Int(pad) & " x i8]");
          first := FALSE; INC(off, pad);
        END;
        IF NOT first THEN Wr.PutText(wr, ", ") END;
        Wr.PutText(wr, RTFieldLLType(fk));
        first := FALSE; INC(off, sz);
      END;
    END;
    Wr.PutText(wr, " }\n");
  END EmitRTStructTypeExt;

PROCEDURE EmitRTStructFields(wr: Wr.T;
                              READONLY kinds: ARRAY OF FieldKind;
                              READONLY vals:  ARRAY OF TEXT) =
  BEGIN
    EmitRTStructFieldsExt(wr, kinds, vals, SUBARRAY(kinds, 0, 0), SUBARRAY(vals, 0, 0));
  END EmitRTStructFields;

(* Emit the initializer fields for a base + optional extension RT0 struct global,
   inserting zeroinitializer padding to maintain the natural-alignment layout. *)
PROCEDURE EmitRTStructFieldsExt(wr: Wr.T;
                                 READONLY baseKinds: ARRAY OF FieldKind;
                                 READONLY baseVals:  ARRAY OF TEXT;
                                 READONLY extKinds:  ARRAY OF FieldKind;
                                 READONLY extVals:   ARRAY OF TEXT) =
  VAR off := 0; first := TRUE;
  BEGIN
    FOR i := 0 TO LAST(baseKinds) DO
      VAR fk := baseKinds[i]; val := baseVals[i];
          sz := RTFieldSize(fk); pad := (-off) MOD sz;
      BEGIN
        IF pad > 0 THEN
          IF NOT first THEN Wr.PutText(wr, ",\n") END;
          Wr.PutText(wr, "  [" & Fmt.Int(pad) & " x i8] zeroinitializer");
          first := FALSE; INC(off, pad);
        END;
        IF NOT first THEN Wr.PutText(wr, ",\n") END;
        Wr.PutText(wr, "  " & RTFieldLLType(fk) & " " & val);
        first := FALSE; INC(off, sz);
      END;
    END;
    FOR i := 0 TO LAST(extKinds) DO
      VAR fk := extKinds[i]; val := extVals[i];
          sz := RTFieldSize(fk); pad := (-off) MOD sz;
      BEGIN
        IF pad > 0 THEN
          IF NOT first THEN Wr.PutText(wr, ",\n") END;
          Wr.PutText(wr, "  [" & Fmt.Int(pad) & " x i8] zeroinitializer");
          first := FALSE; INC(off, pad);
        END;
        IF NOT first THEN Wr.PutText(wr, ",\n") END;
        Wr.PutText(wr, "  " & RTFieldLLType(fk) & " " & val);
        first := FALSE; INC(off, sz);
      END;
    END;
    Wr.PutText(wr, "\n");
  END EmitRTStructFieldsExt;

PROCEDURE IsMethodProcDefined(m: MSIR.Module;  name: TEXT): BOOLEAN =
  (* Return TRUE if a proc with LLVM symbol equal to 'name' is defined in m. *)
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF Text.Equal(LLSymbol(MSIR.ModuleProc(m, i)), name) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END IsMethodProcDefined;

PROCEDURE EmitTypeCells(wr: Wr.T;  m: MSIR.Module;  externs: RefSeq.T) =
  (* RT0.TypeKind ordinals: { Unknown=0, Ref=1, Obj=2, Array=3 } *)
  CONST TK_Ref = 1;  TK_Obj = 2;  TK_Array = 3;
  VAR n := MSIR.ModuleTypeDescCount(m);
      declaredMethods := NEW(RefSeq.T).init();  (* dedup declare void @m() *)
  BEGIN
    (* Pre-populate declaredMethods with externs already declared by CollectExterns
       so that EmitTypeCells does not re-declare the same symbol with a conflicting
       (void @name()) signature. *)
    IF externs # NIL THEN
      FOR ei := 0 TO externs.size() - 1 DO
        VAR ep: MSIR.Proc := externs.get(ei);  sym: TEXT; BEGIN
          sym := LLSymbol(ep);
          declaredMethods.addhi(sym);
        END;
      END;
    END;
    IF n = 0 THEN RETURN END;

    Wr.PutText(wr, "\n; TypeCell / ObjectTypeCell globals\n");
    EmitRTStructType(wr, "TC_t", TCKinds);
    EmitRTStructTypeExt(wr, "OTC_t", TCKinds, OTCExtKinds);
    EmitRTStructTypeExt(wr, "ATC_t", TCKinds, ATCExtKinds);

    FOR k := 0 TO n - 1 DO
      VAR
        d        := MSIR.ModuleTypeDesc(m, k);
        nm       := MSIR.TypeDescName(d);
        knd      := MSIR.TypeDescKind(d);
        isObj    := knd = TK_Obj;
        isArr    := knd = TK_Array;
        structNm : TEXT;
        nameSym  : TEXT := NIL;
        nextVal  : TEXT;
        tcVals    : ARRAY [0 .. TC_nBase - 1] OF TEXT;
        extVals   : REF ARRAY OF TEXT := NIL;
        gcMapName : TEXT := NIL;
      BEGIN
        IF k < n - 1
          THEN nextVal := "@" & MSIR.TypeDescName(MSIR.ModuleTypeDesc(m, k+1));
          ELSE nextVal := "null";
        END;

        IF isObj THEN
          structNm := "OTC_t";
          extVals  := NEW(REF ARRAY OF TEXT, OTC_nExt);
        ELSIF isArr THEN
          structNm := "ATC_t";
          extVals  := NEW(REF ARRAY OF TEXT, ATC_nExt);
        ELSE
          structNm := "TC_t";
        END;

        (* OTC extension — sets nameSym and emits ancillary globals *)
        IF isObj THEN
          CONST
            (* OTC_defaultMethods byte offset in the ObjectTypecell struct.
               Derived from M3RT.m3: OTC_defaultMethods = TC_SIZE + 5*IP + AP = 96+40+8=...
               Actual = 136 bytes (TC_SIZE=96, OTC extras: parentID+linkProc+3*INTEGER = 40). *)
            OTC_defaultMethods_BYTES = 136;
          VAR
            uName    := MSIR.TypeDescUserName(d);
            nMethods := MSIR.TypeDescMethodCount(d);
            dmv      : TEXT := "null";
            lpv      : TEXT := "null";  (* linkProc value *)
          BEGIN
            IF uName # NIL THEN
              nameSym := "@" & nm & ".tc_name";
              Wr.PutText(wr, nameSym & " = private unnamed_addr constant ["
                & Fmt.Int(Text.Length(uName) + 1) & " x i8] c\""
                & uName & "\\00\"\n");
            END;
            IF nMethods > 0 AND MSIR.TypeDescDynamicMethOff(d) THEN
              (* Dynamic linkProc mode: method names at LOCAL indices (0..nLocal-1),
                 supertype method count unknown at compile time.  Emit a linkProc
                 that reads OTC_methodOffset from the TypeCell at runtime (set by
                 RTLinker from the parent's methodSize) and stores each own method
                 at the absolute slot = OTC_methodOffset/sizeof_ptr + localIndex. *)
              VAR lpName := nm & ".linkproc";
                  AP := Target.Address.bytes;
              BEGIN
                lpv := "@" & lpName;
                FOR j := 0 TO nMethods - 1 DO
                  VAR mname := MSIR.TypeDescMethod(d, j);
                      alreadyDeclared := FALSE;
                  BEGIN
                    IF mname # NIL THEN
                      FOR di := 0 TO declaredMethods.size() - 1 DO
                        IF Text.Equal(NARROW(declaredMethods.get(di), TEXT), mname) THEN
                          alreadyDeclared := TRUE; EXIT
                        END;
                      END;
                      IF NOT IsMethodProcDefined(m, mname) AND NOT alreadyDeclared THEN
                        Wr.PutText(wr, "declare void @" & mname & "()\n");
                        declaredMethods.addhi(mname);
                      END;
                    END;
                  END;
                END;
                Wr.PutText(wr, "define internal void @" & lpName
                               & "(ptr %tp) personality ptr @__gxx_personality_v0 {\n");
                Wr.PutText(wr, "entry:\n");
                (* Read OTC_defaultMethods from the TypeCell *)
                Wr.PutText(wr, "  %dm.ptr = getelementptr inbounds i8, ptr %tp, i64 "
                               & Fmt.Int(OTC_defaultMethods_BYTES) & "\n");
                Wr.PutText(wr, "  %dm = load ptr, ptr %dm.ptr\n");
                (* Read OTC_methodOffset from the TypeCell (set by RTLinker) *)
                Wr.PutText(wr, "  %methoff.ptr = getelementptr inbounds i8, ptr %tp, i64 "
                               & Fmt.Int(M3RT.OTC_methodOffset DIV Target.Char.size) & "\n");
                Wr.PutText(wr, "  %methoff = load i64, ptr %methoff.ptr\n");
                FOR j := 0 TO nMethods - 1 DO
                  VAR mname := MSIR.TypeDescMethod(d, j);
                  BEGIN
                    IF mname # NIL THEN
                      (* absolute slot = methoff / AP + j *)
                      Wr.PutText(wr, "  %slot.abs." & Fmt.Int(j)
                                     & " = sdiv i64 %methoff, " & Fmt.Int(AP) & "\n");
                      IF j > 0 THEN
                        Wr.PutText(wr, "  %slot.off." & Fmt.Int(j)
                                       & " = add i64 %slot.abs." & Fmt.Int(j)
                                       & ", " & Fmt.Int(j) & "\n");
                        Wr.PutText(wr, "  %slot." & Fmt.Int(j)
                                       & " = getelementptr ptr, ptr %dm, i64 %slot.off."
                                       & Fmt.Int(j) & "\n");
                      ELSE
                        Wr.PutText(wr, "  %slot." & Fmt.Int(j)
                                       & " = getelementptr ptr, ptr %dm, i64 %slot.abs."
                                       & Fmt.Int(j) & "\n");
                      END;
                      Wr.PutText(wr, "  store ptr @" & mname & ", ptr %slot."
                                     & Fmt.Int(j) & "\n");
                    END;
                  END;
                END;
                Wr.PutText(wr, "  ret void\n}\n");
              END;
            ELSIF nMethods > 0 THEN
              (* Detect whether any slot is NIL (= inherited slot unknown at
                 compile time).  If so, use linkProc mode: RTLinker allocates
                 the vtable, copies parent methods, then calls linkProc to fill
                 in our own slots.  If all slots are non-NIL (full vtable
                 known), use the original defaultMethods mode. *)
              VAR hasNil := FALSE;
              BEGIN
                FOR j := 0 TO nMethods - 1 DO
                  IF MSIR.TypeDescMethod(d, j) = NIL THEN hasNil := TRUE END;
                END;
                IF hasNil THEN
                  (* linkProc mode: emit declarations first (module level), then function. *)
                  VAR lpName := nm & ".linkproc";
                  BEGIN
                    lpv := "@" & lpName;
                    (* Pass 1: emit external declares for own methods (module level) *)
                    FOR j := 0 TO nMethods - 1 DO
                      VAR mname := MSIR.TypeDescMethod(d, j);
                          alreadyDeclared := FALSE;
                      BEGIN
                        IF mname # NIL THEN
                          FOR di := 0 TO declaredMethods.size() - 1 DO
                            IF Text.Equal(NARROW(declaredMethods.get(di), TEXT), mname) THEN
                              alreadyDeclared := TRUE; EXIT
                            END;
                          END;
                          IF NOT IsMethodProcDefined(m, mname) AND NOT alreadyDeclared THEN
                            Wr.PutText(wr, "declare void @" & mname & "()\n");
                            declaredMethods.addhi(mname);
                          END;
                        END;
                      END;
                    END;
                    (* Pass 2: emit the linkProc function body *)
                    Wr.PutText(wr, "define internal void @" & lpName
                                   & "(ptr %tp) personality ptr @__gxx_personality_v0 {\n");
                    Wr.PutText(wr, "entry:\n");
                    Wr.PutText(wr, "  %dm.ptr = getelementptr inbounds i8, ptr %tp, i64 "
                                   & Fmt.Int(OTC_defaultMethods_BYTES) & "\n");
                    Wr.PutText(wr, "  %dm = load ptr, ptr %dm.ptr\n");
                    FOR j := 0 TO nMethods - 1 DO
                      VAR mname := MSIR.TypeDescMethod(d, j);
                      BEGIN
                        IF mname # NIL THEN
                          Wr.PutText(wr, "  %slot." & Fmt.Int(j)
                                         & " = getelementptr ptr, ptr %dm, i64 "
                                         & Fmt.Int(j) & "\n");
                          Wr.PutText(wr, "  store ptr @" & mname & ", ptr %slot."
                                         & Fmt.Int(j) & "\n");
                        END;
                      END;
                    END;
                    Wr.PutText(wr, "  ret void\n}\n");
                  END;
                  (* defaultMethods stays null; RTLinker allocates and fills via linkProc. *)
                ELSE
                  (* Full vtable known: emit as constant defaultMethods. *)
                  dmv := "@" & nm & ".methods";
                  FOR j := 0 TO nMethods - 1 DO
                    VAR mname := MSIR.TypeDescMethod(d, j);
                        alreadyDeclared := FALSE;
                    BEGIN
                      FOR di := 0 TO declaredMethods.size() - 1 DO
                        IF Text.Equal(NARROW(declaredMethods.get(di), TEXT), mname) THEN
                          alreadyDeclared := TRUE; EXIT
                        END;
                      END;
                      IF NOT IsMethodProcDefined(m, mname) AND NOT alreadyDeclared THEN
                        Wr.PutText(wr, "declare void @" & mname & "()\n");
                        declaredMethods.addhi(mname);
                      END;
                    END;
                  END;
                  Wr.PutText(wr, "@" & nm & ".methods = internal constant ["
                    & Fmt.Int(nMethods) & " x ptr] [");
                  FOR j := 0 TO nMethods - 1 DO
                    IF j > 0 THEN Wr.PutText(wr, ", ") END;
                    Wr.PutText(wr, "ptr @" & MSIR.TypeDescMethod(d, j));
                  END;
                  Wr.PutText(wr, "]\n");
                END;
              END;
            END;
            extVals[OTCe_parentID]       := Fmt.Int(MSIR.TypeDescParentUID(d));
            extVals[OTCe_linkProc]       := lpv;
            extVals[OTCe_dataOffset]     := "0";  (* sentinel: RTLinker fills in from parent *)
            extVals[OTCe_methodOffset]   := "0";  (* sentinel: RTLinker fills in from parent *)
            extVals[OTCe_methodSize]     := Fmt.Int(MSIR.TypeDescMethodBytes(d));
            extVals[OTCe_defaultMethods] := dmv;
            extVals[OTCe_parent]         := "null";
          END;
        END;

        (* ATC extension *)
        IF isArr THEN
          extVals[ATCe_nDimensions] := Fmt.Int(MSIR.TypeDescNDimensions(d));
          extVals[ATCe_elementSize] := Fmt.Int(MSIR.TypeDescElementSize(d));
        END;

        (* Typecell base fields *)
        tcVals[TC_typecode]   := "0";
        tcVals[TC_selfID]     := Fmt.Int(MSIR.TypeDescUID(d));
        tcVals[TC_fp]         := FPHex(d);
        tcVals[TC_traced]     := Fmt.Int(ORD(MSIR.TypeDescTraced(d)));
        tcVals[TC_kind]       := Fmt.Int(knd);
        tcVals[TC_link_state] := "0";
        tcVals[TC_dataAlign]  := Fmt.Int(MSIR.TypeDescAlign(d));
        tcVals[TC_dataSize]   := Fmt.Int(MSIR.TypeDescSize(d));
        tcVals[TC_type_map]   := "null";
        (* Emit gc_maps for traced types that the GC would otherwise treat as
           "pure" (no traced references).  The CM3 GC checks:
             (def.gc_map = NIL) AND (def.kind # ORD(TK.Obj))
           If that condition holds, the GC makes a bitwise copy without pointer
           fixup, leaving stale pointers after collection.

           TK.Ref (1): REF RECORD / REF to opaque type.
             Conservative map: one Op.Ref (byte 4) per pointer-sized slot,
             terminated by Op.Stop (byte 0).  Emitted only when size > 0.

           TK.Array (3): REF ARRAY OF T (open array).
             Safe map only for the common single-traced-pointer-per-element
             case (elementSize = addressBytes, nDimensions <= 255):
               [Op.OpenArray_1 (24), nDimensions, Op.Ref (4), Op.Stop (0)]
             This correctly traces REF ARRAY OF REFANY and similar types.
             More complex element layouts are left with gc_map=null for now. *)
        IF MSIR.TypeDescTraced(d) THEN
          IF knd = TK_Ref THEN
            (* For TK_Ref we don't emit a gc_map.  The conservative
               one-Ref-per-slot map was wrong: it made the GC chase integer
               values in e.g. REF ARRAY OF INTEGER as if they were pointers
               (SIGSEGV at collection time).  A null gc_map is correct for
               referents with no internal traced pointers; for referents WITH
               internal pointers the GC will skip tracing them (possible
               retention but no crash).  We accept this trade-off until a
               proper structural gc_map is generated.  isTraced=1 is still set
               so the GC tracks the REF container itself. *)
            (* gcMapName stays NIL → tc_gc_map = "null" below *)
          ELSIF knd = TK_Array THEN
            VAR nDims := MSIR.TypeDescNDimensions(d);
                eltSz := MSIR.TypeDescElementSize(d);
            BEGIN
              (* Only handle the safe case: 1 pointer per element, <=255 dims *)
              IF nDims > 0 AND nDims <= 255
                 AND eltSz = Target.AddressBytes() THEN
                gcMapName := nm & "_gc_map";
                (* [OpenArray_1 (24), nDims, Ref (4), Stop (0)] *)
                Wr.PutText(wr, "@" & gcMapName & " = internal constant [4 x i8]"
                             & " [i8 24, i8 " & Fmt.Int(nDims) & ", i8 4, i8 0]\n");
              END;
            END;
          END;
        END;
        IF gcMapName # NIL
          THEN tcVals[TC_gc_map] := "@" & gcMapName;
          ELSE tcVals[TC_gc_map] := "null";
        END;
        tcVals[TC_type_desc]  := "null";
        (* Set TC_initProc to the object's field-default init procedure if one
           was registered (for OBJECT types with non-zero field defaults). *)
        IF isObj THEN
          VAR ipName := MSIR.TypeDescInitProcName(d);
          BEGIN
            IF ipName # NIL THEN
              (* Declare the init proc — emitted as a define in the same module. *)
              tcVals[TC_initProc] := "@" & ipName;
            ELSE
              tcVals[TC_initProc] := "null";
            END;
          END;
        ELSE
          tcVals[TC_initProc]   := "null";
        END;
        tcVals[TC_brand_ptr]  := "null";
        IF nameSym # NIL
          THEN tcVals[TC_name] := nameSym;
          ELSE tcVals[TC_name] := "null";
        END;
        tcVals[TC_next] := nextVal;

        Wr.PutText(wr, "@" & nm & " = internal global %" & structNm & " {\n");
        IF isObj THEN
          EmitRTStructFieldsExt(wr, TCKinds, tcVals, OTCExtKinds, extVals^);
        ELSIF isArr THEN
          EmitRTStructFieldsExt(wr, TCKinds, tcVals, ATCExtKinds, extVals^);
        ELSE
          EmitRTStructFields(wr, TCKinds, tcVals);
        END;
        Wr.PutText(wr, "}\n");
      END;
    END;
  END EmitTypeCells;

(*----------------------------------------------- TypeLink / cell_ptrs emission *)

(* Emit TypeLink globals (MI_type_cell_ptrs chain) and MSIR_InitTypeLinks.
   Chain: TypeLink[0].defn = null (terminus), TypeLink[k].defn = &TypeLink[k-1].
   MI_type_cell_ptrs points to TypeLink[n-1] (head).
   MSIR_InitTypeLinks is a harness helper: for each TypeLink that has a matching
   TypeDesc (same uid and kind prefix), stores the TypeCell address into defn. *)
PROCEDURE EmitTypeLinks(wr: Wr.T;  m: MSIR.Module;  forRuntime: BOOLEAN) =
  (* Interface units do not emit MSIR_InitTypeLinks — the implementation unit
     owns the TypeDesc registration for shared types.  Each implementation unit
     emits @MSIR_InitTypeLinks_<Mod>_M3 and registers it via @llvm.global_ctors
     so the linker merges all modules' initializers without symbol conflicts. *)
  VAR
    nLinks   := MSIR.ModuleTypeLinkCount(m);
    nDescs   := MSIR.ModuleTypeDescCount(m);
    isIface  := MSIR.ModuleIsInterface(m);
    initName := "MSIR_InitTypeLinks_" & MSIR.ModuleName(m) & "_M3";
  BEGIN
    IF forRuntime THEN
      (* In runtime mode (MSIRObj), the CM3 runtime handles TypeLink traversal and
         TypeCell registration.  Just emit the TypeLink globals; no ctors needed.
         Emit these for interface units too: an interface that declares an object
         type emits its TypeCell (@tc_obj) and its module-info references the
         paired TypeLink (@tl_obj) in type_cell_ptrs, and a field-default INIT
         proc loads @tl_obj to reach the runtime dataOffset.  Skipping typelinks
         for interfaces (the old `IF isIface THEN RETURN` at the top) left
         @tl_obj_<uid> undefined at llc time (RdClass/WrClass in libm3).  The
         interface/impl carry the SAME typelinks as `internal` globals — no link
         conflict.  Only the standalone-mode init ctor is interface-specific. *)
      IF nLinks > 0 THEN
        Wr.PutText(wr, "\n; TypeLink globals (MI_type_cell_ptrs chain)\n");
        EmitRTStructType(wr, "TypeLink_t", TLKinds);
        FOR k := 0 TO nLinks - 1 DO
          VAR
            tl      := MSIR.ModuleTypeLink(m, k);
            nm      := MSIR.TypeLinkName(tl);
            uid     := MSIR.TypeLinkUID(tl);
            prevVal : TEXT;
            tlVals  : ARRAY [0 .. TL_nFields - 1] OF TEXT;
          BEGIN
            IF k = 0
              THEN prevVal := "null";
              ELSE prevVal := "@" & MSIR.TypeLinkName(MSIR.ModuleTypeLink(m, k-1));
            END;
            tlVals[TL_defn]     := prevVal;
            tlVals[TL_typecode] := Fmt.Int(uid);
            Wr.PutText(wr, "@" & nm & " = internal global %TypeLink_t {\n");
            EmitRTStructFields(wr, TLKinds, tlVals);
            Wr.PutText(wr, "}\n");
          END;
        END;
      END;
      RETURN;
    END;

    (* Standalone harness mode: emit TypeLinks + MSIR_InitTypeLinks ctor.
       Interface units carry no init ctor (the implementation unit owns
       TypeDesc registration), so skip the standalone path for them. *)
    IF isIface THEN RETURN END;

    IF nLinks = 0 THEN
      (* No-op init — registered via ctors so link-time symbol is unique. *)
      Wr.PutText(wr, "\ndefine void @" & initName & "() {\n");
      Wr.PutText(wr, "entry:\n");
      Wr.PutText(wr, "  ret void\n");
      Wr.PutText(wr, "}\n");
      Wr.PutText(wr, "@llvm.global_ctors = appending global"
                     & " [1 x { i32, ptr, ptr }] ["
                     & " { i32, ptr, ptr } { i32 65535, ptr @" & initName & ", ptr null }"
                     & " ]\n");
      RETURN;
    END;

    Wr.PutText(wr, "\n; TypeLink globals (MI_type_cell_ptrs chain)\n");
    EmitRTStructType(wr, "TypeLink_t", TLKinds);

    (* Emit each TypeLink global.
       TypeLink[0].defn = null (chain terminus).
       TypeLink[k].defn = ptr @TypeLink[k-1] for k >= 1. *)
    FOR k := 0 TO nLinks - 1 DO
      VAR
        tl      := MSIR.ModuleTypeLink(m, k);
        nm      := MSIR.TypeLinkName(tl);
        uid     := MSIR.TypeLinkUID(tl);
        prevVal : TEXT;
        tlVals  : ARRAY [0 .. TL_nFields - 1] OF TEXT;
      BEGIN
        IF k = 0
          THEN prevVal := "null";
          ELSE prevVal := "@" & MSIR.TypeLinkName(MSIR.ModuleTypeLink(m, k-1));
        END;
        tlVals[TL_defn]     := prevVal;
        tlVals[TL_typecode] := Fmt.Int(uid);
        Wr.PutText(wr, "@" & nm & " = internal global %TypeLink_t {\n");
        EmitRTStructFields(wr, TLKinds, tlVals);
        Wr.PutText(wr, "}\n");
      END;
    END;

    (* Emit per-module init: assign typecodes + store TypeCell addresses into defn.
       Each module emits a uniquely named function registered via @llvm.global_ctors. *)
    Wr.PutText(wr, "\ndefine void @" & initName & "() {\n");
    Wr.PutText(wr, "entry:\n");
    (* Assign sequential typecodes to all TypeDescs (harness-only fallback). *)
    VAR ip := "i" & Fmt.Int(Target.IntegerSize());
    BEGIN
      FOR j := 0 TO nDescs - 1 DO
        VAR d := MSIR.ModuleTypeDesc(m, j);
        BEGIN
          Wr.PutText(wr, "  store " & ip & " " & Fmt.Int(j + 1)
                         & ", ptr @" & MSIR.TypeDescName(d) & "\n");
        END;
      END;
    END;
    FOR k := 0 TO nLinks - 1 DO
      VAR
        tl   := MSIR.ModuleTypeLink(m, k);
        nm   := MSIR.TypeLinkName(tl);
        uid  := MSIR.TypeLinkUID(tl);
        tcNm : TEXT := NIL;
      BEGIN
        FOR j := 0 TO nDescs - 1 DO
          VAR d := MSIR.ModuleTypeDesc(m, j);
          BEGIN
            IF MSIR.TypeDescUID(d) = uid THEN
              (* Match TypeLink prefix to TypeDesc kind prefix:
                 tl_ref_ -> tc_ref_, tl_arr_ -> tc_arr_, tl_obj_ -> tc_obj_ *)
              IF Text.Length(nm) >= 6 AND Text.Length(MSIR.TypeDescName(d)) >= 6 THEN
                VAR prefix := Text.Sub(nm, 0, 6);
                    dn     := MSIR.TypeDescName(d);
                    dnpfx  := Text.Sub(dn, 0, 6);
                BEGIN
                  IF (Text.Equal(prefix, "tl_ref") AND Text.Equal(dnpfx, "tc_ref"))
                  OR (Text.Equal(prefix, "tl_arr") AND Text.Equal(dnpfx, "tc_arr"))
                  OR (Text.Equal(prefix, "tl_obj") AND Text.Equal(dnpfx, "tc_obj")) THEN
                    tcNm := dn;
                  END;
                END;
              END;
            END;
          END;
        END;
        IF tcNm # NIL THEN
          Wr.PutText(wr, "  store ptr @" & tcNm & ", ptr @" & nm & "\n");
        END;
      END;
    END;
    Wr.PutText(wr, "  ret void\n");
    Wr.PutText(wr, "}\n");
    Wr.PutText(wr, "@llvm.global_ctors = appending global"
                   & " [1 x { i32, ptr, ptr }] ["
                   & " { i32, ptr, ptr } { i32 65535, ptr @" & initName & ", ptr null }"
                   & " ]\n");
  END EmitTypeLinks;

(*----------------------------------------------- full_rev emission *)

(* Emit the null-terminated RT0.Revelation array for full REVEAL records.
   Returns the global name if any revelations exist, NIL otherwise.
   RTLinker.DeclareModuleTypes traverses m.full_rev calling NoteFullRevelation
   for each entry to map the opaque TypeLink UID to the revealed TypeCell slot. *)
PROCEDURE EmitRevelations(wr: Wr.T;  m: MSIR.Module;  modName, ip_t: TEXT): TEXT =
  VAR nRev := MSIR.ModuleRevelationCount(m);
      nm   := modName & "_M3_full_rev";
  BEGIN
    IF nRev = 0 THEN RETURN NIL END;
    (* RT0.Revelation = RECORD lhs_id, rhs_id: INTEGER END — terminated by { 0, 0 } *)
    Wr.PutText(wr, "\n; full_rev — REVEAL records for RTLinker.NoteFullRevelation\n");
    Wr.PutText(wr, "%RT0_RV_t = type { " & ip_t & ", " & ip_t & " }\n");
    Wr.PutText(wr, "@" & nm & " = internal global ["
                       & Fmt.Int(nRev + 1) & " x %RT0_RV_t] [\n");
    FOR i := 0 TO nRev - 1 DO
      VAR r := MSIR.ModuleRevelation(m, i);
      BEGIN
        Wr.PutText(wr, "  %RT0_RV_t { " & ip_t & " " & Fmt.Int(MSIR.RevelationLhsUID(r))
                       & ", " & ip_t & " " & Fmt.Int(MSIR.RevelationRhsUID(r)) & " },\n");
      END;
    END;
    Wr.PutText(wr, "  %RT0_RV_t { " & ip_t & " 0, " & ip_t & " 0 }\n]\n");
    RETURN nm;
  END EmitRevelations;

(*----------------------------------------------- gc_map emission *)

TYPE GcMapBytes = REF ARRAY OF INTEGER;

PROCEDURE GcMapAddByte(VAR bytes: GcMapBytes; VAR n: INTEGER; b: INTEGER) =
  BEGIN
    IF n >= NUMBER(bytes^) THEN
      VAR nb := NEW(GcMapBytes, 2 * NUMBER(bytes^));
      BEGIN SUBARRAY(nb^, 0, NUMBER(bytes^)) := bytes^; bytes := nb END;
    END;
    bytes[n] := b;  INC(n);
  END GcMapAddByte;

PROCEDURE GcMapSkipTo(VAR bytes: GcMapBytes; VAR n, cursor: INTEGER;
                       target: INTEGER) =
  VAR delta := target - cursor;
  BEGIN
    WHILE delta > 0 DO
      IF delta <= 255 THEN
        GcMapAddByte(bytes, n, 42);  GcMapAddByte(bytes, n, delta);
        INC(cursor, delta);  delta := 0;
      ELSE
        GcMapAddByte(bytes, n, 43);
        GcMapAddByte(bytes, n, Word.And(delta, 16_ff));
        GcMapAddByte(bytes, n, Word.And(Word.RightShift(delta, 8), 16_ff));
        INC(cursor, delta);  delta := 0;
      END;
    END;
  END GcMapSkipTo;

(* Emit a TipeMap byte-array global for module global scanning.
   Returns TRUE if any traced globals exist (and the global was emitted). *)
PROCEDURE EmitGcMapGlobal(wr: Wr.T;  m: MSIR.Module;
                           modName: TEXT;  miBytes: INTEGER): BOOLEAN =
  VAR bytes: GcMapBytes := NEW(GcMapBytes, 64);  n := 0;  cursor := 0;
      nGlob := MSIR.ModuleGlobalCount(m);  hasTraced := FALSE;
  BEGIN
    FOR i := 0 TO nGlob - 1 DO
      VAR g := MSIR.ModuleGlobal(m, i);
      BEGIN
        IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g)
           AND MSIR.GlobalIsTraced(g) THEN
          hasTraced := TRUE;
        END;
      END;
    END;
    IF NOT hasTraced THEN RETURN FALSE END;

    (* Skip past standard ModuleInfo fields. *)
    GcMapSkipTo(bytes, n, cursor, miBytes);

    (* Visit each traced struct-embedded global in allocation order. *)
    FOR i := 0 TO nGlob - 1 DO
      VAR g   := MSIR.ModuleGlobal(m, i);
          off := MSIR.GlobalByteOffset(g);
      BEGIN
        IF off >= 0 AND NOT MSIR.GlobalIsExternal(g) AND MSIR.GlobalIsTraced(g) THEN
          GcMapSkipTo(bytes, n, cursor, off);
          GcMapAddByte(bytes, n, 4);   (* Op.Ref: visit, advance cursor by address size *)
          INC(cursor, Target.AddressBytes());
        END;
      END;
    END;
    GcMapAddByte(bytes, n, 0);  (* Op.Stop *)

    Wr.PutText(wr, "@" & modName & "_M3_gc_map = internal constant ["
                   & Fmt.Int(n) & " x i8] c\"");
    FOR i := 0 TO n - 1 DO
      Wr.PutText(wr, "\\");
      Wr.PutText(wr, Fmt.Pad(Fmt.Unsigned(bytes[i], 16), 2, '0'));
    END;
    Wr.PutText(wr, "\"\n");
    RETURN TRUE;
  END EmitGcMapGlobal;

(*----------------------------------------------- module binder emission *)

(* RT0.ModuleInfo field indices (0-based, in declaration order). *)
CONST
  MI_file           = 0;
  MI_type_cells     = 1;
  MI_type_cell_ptrs = 2;
  MI_full_rev       = 3;
  MI_part_rev       = 4;
  MI_proc_info      = 5;
  MI_try_scopes     = 6;
  MI_var_map        = 7;
  MI_gc_map         = 8;
  MI_imports        = 9;
  MI_link_state     = 10;
  MI_binder         = 11;
  MI_gc_flags       = 12;

(* TRUE at each field index where the type is INTEGER (not ptr). *)
CONST MIIsInt = ARRAY [0..MSIR.MI_nFields-1] OF BOOLEAN {
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  TRUE,   (* MI_link_state *)
  FALSE,
  TRUE    (* MI_gc_flags *)
};

(* Emit the RTLinker binder @<Mod>_M3 and RT0.ModuleInfo struct @<Mod>_M3_info.

   Binder convention (RT0.Binder):
     mode=0 : return MI pointer (AddUnit path — do NOT run body)
     mode≠0 : run module body then return MI pointer (RunMainBody path) *)
PROCEDURE NameInExterns(externs: RefSeq.T;  name: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO externs.size() - 1 DO
      VAR p: MSIR.Proc := NARROW(externs.get(i), MSIR.Proc);
          pn := MSIR.ProcName(p);
      BEGIN
        IF pn # NIL AND Text.Equal(pn, name) THEN RETURN TRUE END;
      END;
    END;
    RETURN FALSE;
  END NameInExterns;

PROCEDURE EmitModuleBinder(wr: Wr.T;  m: MSIR.Module;  externs: RefSeq.T) =
  VAR
    modName    := MSIR.ModuleName(m);
    binderName := modName & "_M3";
    infoName   := "@" & modName & "_M3_info";
    bodyName   := "@" & modName & "__" & modName & "_M3";
    bodyExists := FALSE;
    ap         := Target.AddressBytes();   (* bytes per field slot *)
    miBytes    := MSIR.MI_nFields * ap;   (* RT0.ModuleInfo total size in bytes *)
    nFields    := MSIR.MI_nFields;
    nImports   := MSIR.ModuleImportBinderCount(m);
    fieldName  : TEXT;
    fieldType  : TEXT;
    fieldVal   : TEXT;
    gcMapName   : TEXT := NIL;  (* NIL if no traced module globals *)
    fullRevName : TEXT := NIL;  (* NIL if no full REVEAL records *)
    ip_t       := "i" & Fmt.Int(Target.IntegerSize());   (* INTEGER type string *)
    ap_t       := "i" & Fmt.Int(Target.AddressSize());   (* ADDRESS type string *)
  VAR isInterface  := MSIR.ModuleIsInterface(m);
      i3InImports  := FALSE;   (* TRUE if modName_I3 appears in import binders *)
  BEGIN
    IF isInterface THEN
      binderName := modName & "_I3";
      bodyName   := "@" & modName & "__" & modName & "_I3";
      (* An interface with VAR initializers (e.g. INTERFACE Remote with
         VAR VarVal := Arr{...}) compiles an init body proc named <Mod>_I3
         (LLVM symbol <Mod>__<Mod>_I3).  The binder must run it on mode # 0,
         exactly as the _M3 binder runs <Mod>__<Mod>_M3 — otherwise the
         interface globals stay zero-initialized and importers read 0. *)
      FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
        IF Text.Equal(MSIR.ProcName(MSIR.ModuleProc(m, i)), modName & "_I3") THEN
          bodyExists := TRUE;
        END;
      END;
    ELSE
      (* Check whether the implementation body proc was compiled (not abandoned). *)
      FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
        IF Text.Equal(MSIR.ProcName(MSIR.ModuleProc(m, i)), modName & "_M3") THEN
          bodyExists := TRUE;
        END;
      END;
      (* Check whether a separate interface unit (_I3) is listed in the imports.
         If so, that unit defines @<Mod>_I3; we only declare it here.
         If not (standalone implementation), we must define it ourselves. *)
      FOR k := 0 TO nImports - 1 DO
        IF Text.Equal(MSIR.ModuleImportBinder(m, k), modName & "_I3") THEN
          i3InImports := TRUE;
        END;
      END;
    END;

    (* Emit RT0.ImportInfo chain — one record per imported module binder.
       Each ImportInfo = { ptr import_ptr, ptr binder_fn, ptr next_ptr }.
       We define @<Mod>_I3 (the interface binder) first so it can be
       referenced without a preceding declare (which would conflict).
       All other binders are external and need explicit declare statements. *)
    IF nImports > 0 THEN
      (* Emit @<Mod>_I3, the module's own interface binder.
         - An interface unit emits a STRONG definition.
         - A non-interface unit ALWAYS emits a WEAK definition.  A MODULE Foo
           that implements INTERFACE Foo lists Foo_I3 in its own import chain
           (i3InImports = TRUE); when the interface and implementation are
           compiled/linked as a single unit (no separate INTERFACE Foo object),
           nobody else defines Foo_I3, so the implementation must.  Emitting it
           `weak` is always safe: if a separately compiled INTERFACE Foo IS
           linked, its strong definition overrides this weak fallback; otherwise
           this weak definition provides the binder.  (Previously a non-interface
           unit with i3InImports = TRUE only DECLARED the binder, leaving it
           undefined at link time — the dominant MSIR-FAIL "missing _I3" cause.)
         MaybeAddExtern skips the module's own _I3 so CollectExterns never
         emits a `declare` for it — LLVM 22 rejects declare+define for the same
         symbol in one module. *)
      VAR link := "";  BEGIN  IF NOT isInterface THEN link := "weak " END;
        Wr.PutText(wr, "\ndefine " & link & "ptr @" & modName & "_I3(" & ip_t & " %mode) {\n");
      END;
      (* For an interface with an init body, run it on mode # 0 (mirrors the
         _M3 binder), so VAR initializers in the interface execute. *)
      IF isInterface AND bodyExists THEN
        Wr.PutText(wr, "entry:\n");
        Wr.PutText(wr, "  %do_body = icmp ne " & ip_t & " %mode, 0\n");
        Wr.PutText(wr, "  br i1 %do_body, label %run, label %done\n");
        Wr.PutText(wr, "run:\n");
        Wr.PutText(wr, "  call void " & bodyName & "()\n");
        Wr.PutText(wr, "  br label %done\n");
        Wr.PutText(wr, "done:\n");
      ELSE
        Wr.PutText(wr, "entry:\n");
      END;
      Wr.PutText(wr, "  ret ptr " & infoName & "\n");
      Wr.PutText(wr, "}\n");

      Wr.PutText(wr, "\n; RT0.ImportInfo chain for " & modName & "\n");
      (* Declare external binders (skip modName_I3 and any already declared by
         CollectExterns — LLVM 22+ rejects duplicate declare statements). *)
      FOR k := 0 TO nImports - 1 DO
        VAR b := MSIR.ModuleImportBinder(m, k);
        BEGIN
          IF NOT Text.Equal(b, modName & "_I3") AND NOT NameInExterns(externs, b) THEN
            Wr.PutText(wr, "declare ptr @" & b & "(" & ip_t & ")\n");
          END;
        END;
      END;
      (* Emit ImportInfo records. *)
      FOR k := 0 TO nImports - 1 DO
        VAR
          b    := MSIR.ModuleImportBinder(m, k);
          name := modName & "_M3_imp." & Fmt.Int(k);
          next : TEXT;
        BEGIN
          IF k < nImports - 1
            THEN next := "@" & modName & "_M3_imp." & Fmt.Int(k + 1);
            ELSE next := "null";
          END;
          Wr.PutText(wr, "@" & name
                         & " = internal global { ptr, ptr, ptr } { ptr null, ptr @"
                         & b & ", ptr " & next & " }\n");
        END;
      END;
    END;

    (* Emit gc_map if there are any struct-embedded traced module globals. *)
    IF EmitGcMapGlobal(wr, m, modName, miBytes) THEN
      gcMapName := modName & "_M3_gc_map";
    END;

    (* Emit full_rev array for REVEAL records (RTLinker.NoteFullRevelation). *)
    fullRevName := EmitRevelations(wr, m, modName, ip_t);

    (* Emit named type — field types from MIIsInt array. *)
    Wr.PutText(wr, "\n");
    Wr.PutText(wr, "; RT0.ModuleInfo for " & modName
                   & " (" & Fmt.Int(nFields) & " fields, "
                   & Fmt.Int(miBytes) & " bytes)\n");
    Wr.PutText(wr, "%RT0_ModuleInfo_t = type { ");
    FOR k := 0 TO nFields - 1 DO
      IF k > 0 THEN Wr.PutText(wr, ", ") END;
      IF MIIsInt[k]
        THEN Wr.PutText(wr, ip_t);
        ELSE Wr.PutText(wr, "ptr");
      END;
    END;
    (* Append the user-global region as one opaque byte blob.  Globals are
       accessed by byte offset (StructFieldRef -> getelementptr i8) and via
       byte-offset aliases, never by struct-field index, so the region need not
       be individually typed — and a blob sized from the front-end's canonical
       offsets reproduces any reserved gap between the MI header and the first
       global (p289: a 40-byte gap meant importers read at +144 while a dense
       layout stored at +104). *)
    VAR embBytes := MSIR.ModuleGlobalStructSize(m) - miBytes;
    BEGIN
      IF embBytes > 0 THEN
        Wr.PutText(wr, ", [" & Fmt.Int(embBytes) & " x i8]");
      END;
    END;
    Wr.PutText(wr, " }\n");

    (* Emit global initializer — one CASE arm per RT0.ModuleInfo field.
       Interface units use internal linkage (mirrors C-mode 'static') to avoid
       duplicate-symbol conflicts with the implementation unit's exported info. *)
    IF isInterface
      THEN Wr.PutText(wr, infoName & " = internal global %RT0_ModuleInfo_t {\n");
      ELSE Wr.PutText(wr, infoName & " = global %RT0_ModuleInfo_t {\n");
    END;
    FOR k := 0 TO nFields - 1 DO
      CASE k OF
      | MI_file           => fieldType := "ptr"; fieldVal := "null";           fieldName := "file";
      | MI_type_cells     => fieldType := "ptr"; fieldName := "type_cells";
                             IF MSIR.ModuleTypeDescCount(m) > 0
                               THEN fieldVal := "@" & MSIR.TypeDescName(MSIR.ModuleTypeDesc(m, 0));
                               ELSE fieldVal := "null";
                             END;
      | MI_type_cell_ptrs => fieldType := "ptr"; fieldName := "type_cell_ptrs";
                             IF MSIR.ModuleTypeLinkCount(m) > 0
                               THEN fieldVal := "@" & MSIR.TypeLinkName(MSIR.ModuleTypeLink(m, MSIR.ModuleTypeLinkCount(m)-1));
                               ELSE fieldVal := "null";
                             END;
      | MI_full_rev       => fieldType := "ptr"; fieldName := "full_rev";
                             IF fullRevName # NIL
                               THEN fieldVal := "@" & fullRevName;
                               ELSE fieldVal := "null";
                             END;
      | MI_part_rev       => fieldType := "ptr"; fieldVal := "null";           fieldName := "part_rev";
      | MI_proc_info      => fieldType := "ptr"; fieldVal := "null";           fieldName := "proc_info";
      | MI_try_scopes     => fieldType := "ptr"; fieldVal := "null";           fieldName := "try_scopes";
      | MI_var_map        => fieldType := "ptr"; fieldName := "var_map";
                             IF gcMapName # NIL
                               THEN fieldVal := "@" & gcMapName;
                               ELSE fieldVal := "null";
                             END;
      | MI_gc_map         => fieldType := "ptr"; fieldName := "gc_map";
                             IF gcMapName # NIL
                               THEN fieldVal := "@" & gcMapName;
                               ELSE fieldVal := "null";
                             END;
      | MI_imports        => fieldType := "ptr"; fieldName := "imports";
                             IF nImports > 0
                               THEN fieldVal := "@" & modName & "_M3_imp.0";
                               ELSE fieldVal := "null";
                             END;
      | MI_link_state     => fieldType := ip_t; fieldVal := "0";               fieldName := "link_state";
      | MI_binder         => fieldType := "ptr"; fieldVal := "@" & binderName; fieldName := "binder";
      | MI_gc_flags       => fieldType := ip_t;
                             (* RT0.GC_both = GC_gen | GC_inc = 3 *)
                             fieldVal := "3";                                   fieldName := "gc_flags";
      ELSE                   fieldType := "ptr"; fieldVal := "null";            fieldName := "?";
      END;
      IF k < nFields - 1
        THEN Wr.PutText(wr, "  " & fieldType & " " & fieldVal & ",");
        ELSE (* Last standard field — comma only if the user-global blob follows. *)
             IF MSIR.ModuleGlobalStructSize(m) - miBytes > 0
               THEN Wr.PutText(wr, "  " & fieldType & " " & fieldVal & ",");
               ELSE Wr.PutText(wr, "  " & fieldType & " " & fieldVal);
             END;
      END;
      Wr.PutText(wr, "  ; " & fieldName & " (+" & Fmt.Int(k * ap) & ")\n");
    END;
    (* Zero-initialize the user-global blob as a single byte region.  Runtime
       module init bodies store the actual values via byte-offset GEPs. *)
    VAR embBytes := MSIR.ModuleGlobalStructSize(m) - miBytes;
    BEGIN
      IF embBytes > 0 THEN
        Wr.PutText(wr, "  [" & Fmt.Int(embBytes) & " x i8] zeroinitializer"
                       & "  ; user globals (" & Fmt.Int(embBytes) & " bytes)\n");
      END;
    END;
    Wr.PutText(wr, "}\n");

    (* Emit aliases for struct-embedded globals so C/CG code can find them
       by their mangled names (e.g. @Main__gCounter). *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      VAR g   := MSIR.ModuleGlobal(m, i);
          off := MSIR.GlobalByteOffset(g);
      BEGIN
        IF off >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN
          VAR lltype: TEXT;  aliaslink := "";
          BEGIN
            IF MSIR.GlobalIsTraced(g)
              THEN lltype := "ptr";
              ELSE lltype := LLTypeStr(MSIR.GlobalType(g));
            END;
            (* An interface variable's alias is emitted by BOTH the interface
               unit and its same-name implementation module (MODULE Z EXPORTS Z),
               each pointing at the canonical t.offset within the (weak, layout-
               identical) @<Mod>_M3_info.  Two strong defs are a link-time
               duplicate.  The front end flags the module unit's copy weak
               (GlobalSetWeak) so the interface's strong def wins; a module-
               PRIVATE global alias is not flagged and stays strong (so external
               references resolve). *)
            IF MSIR.GlobalIsWeak(g) THEN aliaslink := "weak " END;
            Wr.PutText(wr, "@" & MSIR.GlobalName(g)
                           & " = " & aliaslink & "alias " & lltype
                           & ", ptr getelementptr inbounds (i8, ptr "
                           & infoName & ", " & ap_t & " " & Fmt.Int(off) & ")\n");
          END;
        END;
      END;
    END;
    (* Interface binder @<Mod>_I3.  Define it when this is the interface unit or
       a standalone implementation; declare it when a separate interface exists. *)
    IF nImports = 0 THEN
      IF isInterface OR NOT i3InImports THEN
        VAR link := "";  BEGIN  IF NOT isInterface THEN link := "weak " END;
          Wr.PutText(wr, "\ndefine " & link & "ptr @" & modName & "_I3(" & ip_t
                         & " %mode) personality ptr @__gxx_personality_v0 {\n");
        END;
        (* Run the interface init body on mode # 0 (mirrors the _M3 binder), so
           an import-less interface's VAR initializers still execute. *)
        IF isInterface AND bodyExists THEN
          Wr.PutText(wr, "entry:\n");
          Wr.PutText(wr, "  %do_body = icmp ne " & ip_t & " %mode, 0\n");
          Wr.PutText(wr, "  br i1 %do_body, label %run, label %done\n");
          Wr.PutText(wr, "run:\n");
          Wr.PutText(wr, "  call void " & bodyName & "()\n");
          Wr.PutText(wr, "  br label %done\n");
          Wr.PutText(wr, "done:\n");
        ELSE
          Wr.PutText(wr, "entry:\n");
        END;
        Wr.PutText(wr, "  ret ptr " & infoName & "\n");
        Wr.PutText(wr, "}\n");
      ELSE
        Wr.PutText(wr, "\ndeclare ptr @" & modName & "_I3(" & ip_t & ")\n");
      END;
    END;

    (* Binder function: mode=0 → return MI; mode≠0 → run body + return MI.
       For interface modules, @<Mod>_I3 was already defined in the imports section
       and @<Mod>_M3 is only declared (external — provided by the implementation),
       so we skip defining the binder here to avoid a redefinition error. *)
    IF NOT isInterface THEN
      Wr.PutText(wr, "\ndefine ptr @" & binderName & "(" & ip_t
                     & " %mode) personality ptr @__gxx_personality_v0 {\n");
      IF bodyExists THEN
        Wr.PutText(wr, "entry:\n");
        Wr.PutText(wr, "  %do_body = icmp ne " & ip_t & " %mode, 0\n");
        Wr.PutText(wr, "  br i1 %do_body, label %run, label %done\n");
        Wr.PutText(wr, "run:\n");
        Wr.PutText(wr, "  call void " & bodyName & "()\n");
        Wr.PutText(wr, "  br label %done\n");
        Wr.PutText(wr, "done:\n");
      ELSE
        Wr.PutText(wr, "entry:\n");
      END;
      Wr.PutText(wr, "  ret ptr " & infoName & "\n");
      Wr.PutText(wr, "}\n");
    END;
  END EmitModuleBinder;

(* Emit an early global constructor that applies compile-time-constant record
   initializers to embedded user globals (recorded by Variable.DeclareGlobalMSIR).
   Needed in MSIRObj mode because @Mod_M3_info's user region is a zero blob, and
   globals like RTType's InfoMap tables (uids/types/brands) are read during
   RTLinker startup — before any module body runs.  A @llvm.global_ctors entry
   runs the initializer at image load, before main() → before InitRuntime.  The
   const value carries proper relocations (proc/text-literal pointers), so a
   single typed store per global suffices. *)
PROCEDURE EmitGlobalInitCtor(wr: Wr.T;  m: MSIR.Module) =
  VAR
    n        := MSIR.ModuleGlobalInitCount(m);
    modName  := MSIR.ModuleName(m);
    ctorName := "MSIR_InitGlobals_" & modName & "_M3";
    ap_t     := "i" & Fmt.Int(Target.AddressSize());
  BEGIN
    IF n = 0 THEN RETURN END;
    Wr.PutText(wr, "\n; const-initialised user globals (early ctor)\n");
    Wr.PutText(wr, "define void @" & ctorName & "() {\nentry:\n");
    FOR i := 0 TO n - 1 DO
      VAR off := MSIR.ModuleGlobalInitOffset(m, i);
          val := MSIR.ModuleGlobalInitValue(m, i);
      BEGIN
        Wr.PutText(wr, "  store ");
        LLTypedVal(wr, val);
        Wr.PutText(wr, ", ptr getelementptr inbounds (i8, ptr @"
                       & modName & "_M3_info, " & ap_t & " "
                       & Fmt.Int(off) & ")\n");
      END;
    END;
    Wr.PutText(wr, "  ret void\n}\n");
    Wr.PutText(wr, "@llvm.global_ctors = appending global"
                   & " [1 x { i32, ptr, ptr }] ["
                   & " { i32, ptr, ptr } { i32 65535, ptr @" & ctorName
                   & ", ptr null }"
                   & " ]\n");
  END EmitGlobalInitCtor;

(*------------------------------------------------------ module emission *)

PROCEDURE ModuleHasEH(m: MSIR.Module): BOOLEAN =
  BEGIN
    (* Always emit EH preamble since every function now gets personality for
       exception propagation. Check for procs to avoid empty modules. *)
    RETURN MSIR.ModuleProcCount(m) > 0;
  END ModuleHasEH;

PROCEDURE ProcHasGcOp(p: MSIR.Proc): BOOLEAN =
  VAR nb := MSIR.ProcBlockCount(p);
  BEGIN
    FOR bi := 0 TO nb - 1 DO
      VAR
        b  := MSIR.ProcBlock(p, bi);
        ni := MSIR.BlockInsnCount(b);
      BEGIN
        FOR ii := 0 TO ni - 1 DO
          VAR op := MSIR.InsnOp(MSIR.BlockInsn(b, ii));
          BEGIN
            IF op = MSIR.Op.GcLoad OR op = MSIR.Op.GcStore THEN
              RETURN TRUE;
            END;
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END ProcHasGcOp;

PROCEDURE ModuleHasGcOps(m: MSIR.Module): BOOLEAN =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF ProcHasGcOp(MSIR.ModuleProc(m, i)) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END ModuleHasGcOps;

PROCEDURE EmitOneTCEntry (wr: Wr.T;  ent: TCEntry) =
  (* Helper: emit one TYPECASE type table global.  Isolated so that all
     gc.loads of ent.uids stay in this proc's MSIR scope and do not leak
     pendingContainer into Module, avoiding a dominance violation on the
     subsequent pendingTC := NIL gc.store in Module. *)
  VAR n := NUMBER(ent.uids^);
  BEGIN
    Wr.PutText(wr, ent.name & " = internal global [" & Fmt.Int(n));
    Wr.PutText(wr, " x { ptr, i64 }] [");
    FOR k := 0 TO n - 1 DO
      IF k > 0 THEN Wr.PutText(wr, ", ") END;
      Wr.PutText(wr, "{ ptr, i64 } { ptr null, i64 " & Fmt.Int(ent.uids[k]) & " }");
    END;
    Wr.PutText(wr, "]\n");
  END EmitOneTCEntry;

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module;  forRuntime: BOOLEAN := FALSE) =
  VAR
    externs    := NEW(RefSeq.T).init();
    triple     := MSIR.ModuleTriple(m);
    datalayout := MSIR.ModuleDataLayout(m);
    needsEH    : BOOLEAN;
    needsGC    : BOOLEAN;
  BEGIN
    curEmitModule := m;
    auxN          := 0;
    tcN           := 0;
    pendingTC     := NIL;
    needsEH       := ModuleHasEH(m);
    needsGC       := ModuleHasGcOps(m);
    Wr.PutText(wr, "; ModuleID = '" & MSIR.ModuleName(m) & "'\n");
    Wr.PutText(wr, "source_filename = \"" & MSIR.ModuleName(m) & "\"\n");
    IF datalayout # NIL THEN
      Wr.PutText(wr, "target datalayout = \"" & datalayout & "\"\n");
    END;
    IF triple # NIL THEN
      Wr.PutText(wr, "target triple = \"" & triple & "\"\n");
    END;
    Wr.PutText(wr, "\n");

    (* EH — emitted once per module when any proc uses invoke.
       @_ZTI6_M3Exc is defined as linkonce_odr so each MSIR-compiled module
       is self-sufficient, avoiding dependence on the private/unexported copy
       in libm3core.dylib when the program links against the shared library. *)
    IF needsEH THEN
      VAR isDarwin := (Target.GetSystemName() # NIL) AND
                      (Text.Equal(Target.GetSystemName(), "ARM64_DARWIN") OR
                       Text.Equal(Target.GetSystemName(), "AMD64_DARWIN"));
      BEGIN
        Wr.PutText(wr, "@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]\n");
        IF isDarwin THEN
          (* Mach-O: hidden, no comdat; MSB of name pointer set per Darwin ABI. *)
          Wr.PutText(wr, "@_ZTS6_M3Exc = linkonce_odr hidden constant [8 x i8] c\"6_M3Exc\\00\", align 1\n");
          Wr.PutText(wr,
            "@_ZTI6_M3Exc = linkonce_odr hidden constant { ptr, ptr }" &
            " { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2)," &
            " ptr inttoptr (i64 add (i64 ptrtoint (ptr @_ZTS6_M3Exc to i64)," &
            " i64 -9223372036854775808) to ptr) }, align 8\n");
        ELSE
          (* ELF: dso_local, comdat; plain name pointer. *)
          Wr.PutText(wr, "$_ZTI6_M3Exc = comdat any\n");
          Wr.PutText(wr, "$_ZTS6_M3Exc = comdat any\n");
          Wr.PutText(wr, "@_ZTS6_M3Exc = linkonce_odr dso_local constant [8 x i8] c\"6_M3Exc\\00\", comdat, align 1\n");
          Wr.PutText(wr,
            "@_ZTI6_M3Exc = linkonce_odr dso_local constant { ptr, ptr }" &
            " { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2)," &
            " ptr @_ZTS6_M3Exc }, comdat, align 8\n");
        END;
      END;
      Wr.PutText(wr, "declare i32 @__gxx_personality_v0(...)\n");
      (* __cxa_get_exception_ptr, __cxa_begin_catch, __cxa_end_catch are all
         declared automatically by CollectExterns (they appear as Call callees
         in catch landingpads). *)
      Wr.PutText(wr, "\n");
    END;

    (* GC barrier externs — emitted when any proc uses gc.load / gc.store.
       LLVM 22+ treats declare+define for the same symbol as an invalid
       redefinition, so skip the declare when the barrier is a module proc
       (e.g. RTCollector.m3 defines CheckLoadTracedRef itself). *)
    IF needsGC THEN
      VAR barrierLoad  := MSIR.ModuleGCLoadBarrier(m);
          barrierStore := MSIR.ModuleGCStoreBarrier(m);
      BEGIN
        IF barrierLoad = NIL OR NOT IsModuleProc(m, barrierLoad) THEN
          Wr.PutText(wr, "declare void @"
                         & LLHookName(barrierLoad,
                                       "RTHooks__CheckLoadTracedRef")
                         & "(ptr)\n");
        END;
        IF barrierStore = NIL OR NOT IsModuleProc(m, barrierStore) THEN
          Wr.PutText(wr, "declare void @"
                         & LLHookName(barrierStore,
                                       "RTHooks__CheckStoreTraced")
                         & "(ptr)\n");
        END;
        Wr.PutText(wr, "\n");
      END;
    END;

    (* globals *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      EmitGlobal(wr, MSIR.ModuleGlobal(m, i), m);
    END;

    (* collect extern callees and TextLiteral vtable method procs *)
    CollectExterns(m, externs);
    FOR i := 0 TO 4 DO
      VAR p := MSIR.ModuleGetTextLitHook(m, i); BEGIN
        IF p # NIL AND NOT ProcSeen(externs, p) THEN
          externs.addhi(p);
        END;
      END;
    END;
    IF externs.size() > 0 THEN
      Wr.PutText(wr, "\n");
      FOR i := 0 TO externs.size() - 1 DO
        EmitDeclareFromSeq(wr, externs, i);
      END;
    END;

    (* Pre-assign DWARF metadata indices before emitting any define lines
       so that forward !dbg references are correct. *)
    BuildDebugInfo(m);

    (* Declare the llvm.dbg.declare intrinsic when variable debug info is present. *)
    IF dbgEnabled AND dbgVarN > 0 THEN
      Wr.PutText(wr, "declare void @llvm.dbg.declare(metadata, metadata, metadata)\n\n");
    END;

    (* internal proc definitions — guard against duplicate symbol names that
       can arise when a hook proc (e.g. RTHooks__CheckLoadTracedRef) is both
       compiled as a module proc and re-looked-up by EndUnit's HookProc call,
       landing two entries with the same LLSymbol in the proc list. *)
    VAR emittedNames := NEW(RefSeq.T).init();
    BEGIN
      FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
        VAR p := MSIR.ModuleProc(m, i);
            sym := LLSymbol(p);
            dup := FALSE;
        BEGIN
          IF sym # NIL THEN
            FOR j := 0 TO emittedNames.size() - 1 DO
              VAR s: TEXT := emittedNames.get(j); BEGIN
                IF Text.Equal(s, sym) THEN dup := TRUE; EXIT END;
              END;
            END;
          END;
          IF NOT dup THEN
            IF sym # NIL THEN emittedNames.addhi(sym) END;
            EmitProc(wr, p);
          END;
        END;
      END;
    END;

    (* TextLiteral globals *)
    EmitTextLiterals(wr, m);

    (* CONST array globals *)
    EmitConstArrays(wr, m);

    (* TypeCell / ObjectTypeCell globals for type_cells *)
    EmitTypeCells(wr, m, externs);

    (* TypeLink globals for type_cell_ptrs chain, plus MSIR_InitTypeLinks *)
    EmitTypeLinks(wr, m, forRuntime);

    (* Exception descriptors: { i64 uid, ptr null, i64 0 } = ExceptionDesc *)
    FOR i := 0 TO MSIR.ModuleExcDescCount(m) - 1 DO
      VAR d := MSIR.ModuleExcDesc(m, i);
      BEGIN
        Wr.PutText(wr, "\n@" & MSIR.ExcDescName(d)
                       & " = internal global { i64, ptr, i64 } { i64 "
                       & Fmt.Int(MSIR.ExcDescUID(d))
                       & ", ptr null, i64 0 }\n");
      END;
    END;

    (* RTLinker binder and ModuleInfo descriptor *)
    EmitModuleBinder(wr, m, externs);

    (* Early global constructor for const-initialised user globals.  Only in
       MSIRObj (forRuntime) mode: standalone mode already emits a @llvm.global_ctors
       for MSIR_InitTypeLinks, and LLVM allows only one such array per module. *)
    IF forRuntime THEN
      EmitGlobalInitCtor(wr, m);
    END;

    (* Emit mutable type-table globals collected during typecase emission.
       ScanTypecase lazily fills the defn pointer so these must be global
       (not constant).  TypecaseCell = { ptr defn, i64 uid } (16 bytes). *)
    IF pendingTC # NIL AND pendingTC.size() > 0 THEN
      VAR tcHook := MSIR.ModuleScanTypecase(m); BEGIN
      Wr.PutText(wr, "\n; TYPECASE type tables ("
                     & LLHookName(tcHook, "RTHooks__ScanTypecase")
                     & ")\n");
      (* Skip declare if ScanTypecase is defined in this module (LLVM 22+
         treats declare+define for the same symbol as an invalid redefinition.) *)
      IF tcHook = NIL OR NOT IsModuleProc(m, tcHook) THEN
        Wr.PutText(wr, "declare i64 @"
                       & LLHookName(tcHook, "RTHooks__ScanTypecase")
                       & "(ptr, ptr)\n");
      END;
      FOR ti := 0 TO pendingTC.size() - 1 DO
        EmitOneTCEntry(wr, NARROW(pendingTC.get(ti), TCEntry));
      END;
      pendingTC := NIL;
      END; (* VAR tcHook *)
    END;

    (* DWARF debug metadata section — emitted last so all forward !dbg
       refs in define lines are satisfied by the time the assembler sees them. *)
    IF dbgEnabled THEN
      EmitDebugMetadata(wr);
    END;
  END Module;

BEGIN
END MSIRToLLVM.
