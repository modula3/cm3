MODULE MSIRToLLVM;

IMPORT MSIR, Wr, Fmt, Thread, Text, RefSeq, TextWr, Word;
IMPORT M3RT, Target, TFloat;
<*FATAL Thread.Alerted, Wr.Failure*>

(*----------------------------------------------------- module-level state *)

TYPE TCEntry = REF RECORD
  name : TEXT;                   (* "@tc.table.N" *)
  uids : REF ARRAY OF LONGINT;   (* type UIDs; last entry is 0 (ELSE) *)
END;

VAR
  auxN:          INTEGER     := 0;
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
    IF IsModuleProc(curEmitModule, p) THEN
      RETURN MSIR.ModuleName(curEmitModule) & "__" & n;
    ELSIF Text.FindChar(n, '.') >= 0 THEN
      (* Fully qualified M3 name ("Fmt.Int") → replace dots with __ *)
      RETURN DotsToUnderscore(n);
    ELSIF IsModuleProcByName(curEmitModule, n) THEN
      (* Extern stub whose scope name was empty: unqualified name matches a
         module proc — prepend the module prefix. *)
      RETURN MSIR.ModuleName(curEmitModule) & "__" & n;
    ELSE
      RETURN n;  (* C extern or truly external M3 symbol *)
    END;
  END LLSymbol;

(* LLVM symbol name for a module-local global variable. *)
PROCEDURE LLGlobalSym(name: TEXT): TEXT =
  BEGIN RETURN MSIR.ModuleName(curEmitModule) & "__" & name END LLGlobalSym;

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
          Wr.PutText(wr, ", i64");
        END;
        Wr.PutText(wr, " }");
    | MSIR.TypeKind.HeapArray =>
        Wr.PutText(wr, "ptr");  (* opaque heap-array dope pointer *)
    | MSIR.TypeKind.FixedArray =>
        Wr.PutText(wr, "[");
        Wr.PutText(wr, Fmt.LongInt(MSIR.FixedArrayLen(t)));
        Wr.PutText(wr, " x ");
        LLType(wr, MSIR.FixedArrayElt(t));
        Wr.PutText(wr, "]");
    | MSIR.TypeKind.Subrange =>
        LLType(wr, MSIR.SubrangeParent(t));
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
   For float (32-bit): LLVM requires 0x + 16 hex chars of the 64-bit double
   that has the same value (i.e., the float widened to double).
   For double (64-bit): 0x + 16 hex chars of the 64-bit IEEE pattern.
   We use TFloat.ToBytes to get little-endian bytes, then emit big-endian hex. *)
PROCEDURE EmitFloatHex(wr: Wr.T;  v: MSIR.Value) =
  VAR
    f    : Target.Float;
    buf  : ARRAY [0..15] OF TFloat.Byte;
    nBytes : INTEGER;
    dbuf : ARRAY [0..7] OF TFloat.Byte;  (* always 8 bytes for LLVM *)
  BEGIN
    MSIR.GetFloatVal(v, f);
    nBytes := TFloat.ToBytes(f, buf);
    (* Zero-extend to 8 bytes. For 4-byte REAL, we widen to the 8-byte IEEE
       double with the same value by letting clang/LLVM do the right thing:
       emit 0x + 8 zero-padded digits for the 32-bit pattern.
       LLVM accepts 0xHHHHHHHH (8 hex = 4 bytes) for float literals. *)
    FOR i := 0 TO 7 DO dbuf[i] := 0 END;
    FOR i := 0 TO nBytes - 1 DO dbuf[i] := buf[i] END;
    Wr.PutText(wr, "0x");
    (* Big-endian hex: most-significant byte first. *)
    FOR i := nBytes - 1 TO 0 BY -1 DO
      Wr.PutChar(wr, HexDigit[Word.And(Word.RightShift(dbuf[i], 4), 16_f)]);
      Wr.PutChar(wr, HexDigit[Word.And(dbuf[i], 16_f)]);
    END;
    (* If 32-bit float: pad remaining 8 hex digits with zeros so LLVM sees
       a valid 16-hex-digit constant (float 0xHHHHHHHH00000000). *)
    FOR i := nBytes TO 7 DO
      Wr.PutText(wr, "00");
    END;
  END EmitFloatHex;

(* Emit just the LLVM name/constant for a value (no type prefix). *)
PROCEDURE LLOpVal(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "undef"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Wr.PutText(wr, Fmt.LongInt(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstFloat =>
        EmitFloatHex(wr, v);
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "null");
    | MSIR.ValueKind.ConstProc =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, LLSymbol(MSIR.GetConstProc(v)));
    | MSIR.ValueKind.ConstTextLit =>
        (* Emit as a constant-expression GEP: no separate instruction needed. *)
        Wr.PutText(wr, "getelementptr inbounds (i8, ptr @textlit_");
        Wr.PutText(wr, Fmt.Int(MSIR.GetTextLitUID(v)));
        Wr.PutText(wr, ", i64 " & Fmt.Int(Target.Address.bytes) & ")");
    | MSIR.ValueKind.GlobalRef =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, LLGlobalSym(MSIR.ValueName(v)));
    | MSIR.ValueKind.StructFieldRef =>
        (* getelementptr inbounds (i8, ptr @Mod_M3_info, i64 N) *)
        Wr.PutText(wr, "getelementptr inbounds (i8, ptr ");
        Wr.PutText(wr, MSIR.ValueName(v));   (* "@Mod_M3_info" *)
        Wr.PutText(wr, ", i64 ");
        Wr.PutText(wr, Fmt.Int(MSIR.GetStructFieldOffset(v)));
        Wr.PutText(wr, ")");
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

   Header layout (RT0.RefHeader = Target.Address.bytes before object ptr):
     bit M3RT.RH_gray_offset = gray bit (mask = 1 << RH_gray_offset) *)
PROCEDURE EmitGcReadBarrier(wr: Wr.T;  refName: TEXT) =
  VAR n: TEXT;
  BEGIN
    INC(auxN);
    n := Fmt.Int(auxN);
    (* nil check *)
    Wr.PutText(wr, "  %__gc_nil." & n & " = icmp eq ptr " & refName & ", null\n");
    Wr.PutText(wr, "  br i1 %__gc_nil." & n
                   & ", label %gc.skip." & n & ", label %gc.check." & n & "\n");
    (* misaligned check: low bit set → not a real heap pointer *)
    Wr.PutText(wr, "gc.check." & n & ":\n");
    Wr.PutText(wr, "  %__gc_int." & n & " = ptrtoint ptr " & refName & " to i64\n");
    Wr.PutText(wr, "  %__gc_low." & n & " = and i64 %__gc_int." & n & ", 1\n");
    Wr.PutText(wr, "  %__gc_ma."  & n & " = icmp ne i64 %__gc_low." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_ma." & n
                   & ", label %gc.skip." & n & ", label %gc.gray." & n & "\n");
    (* gray-bit check: read object header word (8 bytes before object ptr) *)
    Wr.PutText(wr, "gc.gray." & n & ":\n");
    Wr.PutText(wr, "  %__gc_hptr." & n
                   & " = getelementptr i8, ptr " & refName
                   & ", i64 -" & Fmt.Int(Target.Address.bytes) & "\n");
    Wr.PutText(wr, "  %__gc_hdr."  & n
                   & " = load i64, ptr %__gc_hptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_gb."   & n
                   & " = and i64 %__gc_hdr." & n
                   & ", " & Fmt.Int(Word.Shift(1, M3RT.RH_gray_offset)) & "\n");
    Wr.PutText(wr, "  %__gc_gr."   & n
                   & " = icmp ne i64 %__gc_gb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_gr." & n
                   & ", label %gc.slow." & n & ", label %gc.skip." & n & "\n");
    (* slow path *)
    Wr.PutText(wr, "gc.slow." & n & ":\n");
    Wr.PutText(wr, "  call void @"
                   & LLHookName(MSIR.ModuleGCLoadBarrier(curEmitModule),
                                 "RTHooks__CheckLoadTracedRef")
                   & "(ptr " & refName & ")\n");
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

   Fast path: read the header word (Target.Address.bytes before the object
   pointer); if the dirty bit (bit M3RT.RH_dirty_offset, 1<<RH_dirty_offset)
   is already set, skip the slow-path call.
   Otherwise call RTHooks__CheckStoreTraced.

   Header layout matches RT0.RefHeaderBits (M3RT constants):
     bit 0:                forwarded  (RH_forwarded_offset)
     bits 1-20:            typecode   (RH_typecode_offset, RH_typecode_size)
     bit RH_dirty_offset:  dirty
     bit RH_gray_offset:   gray *)
PROCEDURE EmitGcWriteBarrier(wr: Wr.T;  containerName: TEXT) =
  VAR n: TEXT;
  BEGIN
    INC(auxN);
    n := Fmt.Int(auxN);
    (* Read object header; skip barrier if already dirty. *)
    Wr.PutText(wr, "  %__gc_whptr." & n
                   & " = getelementptr i8, ptr " & containerName
                   & ", i64 -" & Fmt.Int(Target.Address.bytes) & "\n");
    Wr.PutText(wr, "  %__gc_whdr."  & n
                   & " = load i64, ptr %__gc_whptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_wdb."   & n
                   & " = and i64 %__gc_whdr." & n
                   & ", " & Fmt.Int(Word.Shift(1, M3RT.RH_dirty_offset)) & "\n");
    Wr.PutText(wr, "  %__gc_wdirty." & n
                   & " = icmp ne i64 %__gc_wdb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_wdirty." & n
                   & ", label %gc.wskip." & n & ", label %gc.wslow." & n & "\n");
    Wr.PutText(wr, "gc.wslow." & n & ":\n");
    Wr.PutText(wr, "  call void @"
                   & LLHookName(MSIR.ModuleGCStoreBarrier(curEmitModule),
                                 "RTHooks__CheckStoreTraced")
                   & "(ptr " & containerName & ")\n");
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
      Wr.PutText(wr, ", i64 ");
      LLOpVal(wr, MSIR.InsnOperand(i, 1 + k));
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
    op   := MSIR.InsnOp(i);
    res  := MSIR.InsnResult(i);
    nOps := MSIR.InsnOperandCount(i);
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
        VAR cnt := MSIR.InsnExtractIdx(i);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = alloca ");
          LLType(wr, MSIR.InsnTargetType(i));
          IF cnt > 1 THEN
            Wr.PutText(wr, ", i64 " & Fmt.Int(cnt));
          END;
          Wr.PutText(wr, "\n");
        END;

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
        EmitGcReadBarrier(wr, MSIR.ValueName(res));

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
          EmitGcWriteBarrier(wr, LLOpValStr(MSIR.InsnOperand(i, 2)));
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

    | MSIR.Op.FAdd => EmitBinop(wr, "fadd", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FSub => EmitBinop(wr, "fsub", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FMul => EmitBinop(wr, "fmul", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FDiv => EmitBinop(wr, "fdiv", res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.FNeg =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = fneg ");
        LLTypedVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.FPFloor, MSIR.Op.FPCeil, MSIR.Op.FPRound =>
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
          Wr.PutText(wr, ")\n");
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
          Wr.PutText(wr, "\n");
        END;

    | MSIR.Op.PtrAdd =>
        (* getelementptr i8, ptr %base, i64 N — advances base by N bytes *)
        VAR
          baseV := MSIR.InsnOperand(i, 0);
          idx   := MSIR.InsnExtractIdx(i);
        BEGIN
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = getelementptr i8, ptr ");
          LLOpVal(wr, baseV);
          Wr.PutText(wr, ", i64 " & Fmt.Int(idx) & "\n");
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
          Wr.PutText(wr, ")\n");
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
          Wr.PutText(wr, "\n");
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
          Wr.PutText(wr, ", i64 0, ");
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
          Wr.PutText(wr, ", " & Fmt.LongInt(dim + 1L) & "\n");
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
          (* Select the right LLVM cast for the source → destination types.
             ptr↔integer require inttoptr/ptrtoint; bitcast is only for
             same-sized scalar pairs or ptr↔ptr. *)
          VAR
            srcIsPtr := MSIR.Kind(srcT) = MSIR.TypeKind.Ptr
                     OR MSIR.Kind(srcT) = MSIR.TypeKind.GcRef
                     OR MSIR.Kind(srcT) = MSIR.TypeKind.GcSlot;
            dstIsPtr := MSIR.Kind(dstT) = MSIR.TypeKind.Ptr
                     OR MSIR.Kind(dstT) = MSIR.TypeKind.GcRef
                     OR MSIR.Kind(dstT) = MSIR.TypeKind.GcSlot;
          BEGIN
            IF srcIsPtr AND dstBits > 0 THEN
              convOp := "ptrtoint";
            ELSIF srcBits > 0 AND dstIsPtr THEN
              convOp := "inttoptr";
            ELSIF srcBits > 0 AND dstBits > 0 THEN
              IF    dstBits > srcBits THEN convOp := "sext";
              ELSIF dstBits < srcBits THEN convOp := "trunc";
              ELSE                        convOp := "bitcast";
              END;
            ELSE
              convOp := "bitcast";
            END;
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & convOp & " ");
          LLTypedVal(wr, src);
          Wr.PutText(wr, " to ");
          LLType(wr, dstT);
          Wr.PutText(wr, "\n");
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
          uids     : REF ARRAY OF LONGINT;
        BEGIN
          INC(auxN);
          tblN    := Fmt.Int(auxN);
          tblName := "@tc.table." & tblN;
          idxName := "%__tc_idx." & tblN;

          (* Collect UIDs for the pending type table. *)
          uids := NEW(REF ARRAY OF LONGINT, nClauses);
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
  BEGIN
    FOR i := 0 TO procs.size() - 1 DO
      IF procs.get(i) = p THEN RETURN TRUE END;
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
      IF Text.Equal(MSIR.ProcName(MSIR.ModuleProc(m, i)), name) THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END IsModuleProcByName;

PROCEDURE CollectExterns(m: MSIR.Module;  externs: RefSeq.T) =
  (* Walk all call insns in all internal procs, collect external callees. *)
  VAR
    np := MSIR.ModuleProcCount(m);
    p  : MSIR.Proc;
    nb : INTEGER;
    b  : MSIR.Block;
    ni : INTEGER;
    ins: MSIR.Insn;
    callee: MSIR.Proc;
  BEGIN
    FOR pi := 0 TO np - 1 DO
      p  := MSIR.ModuleProc(m, pi);
      nb := MSIR.ProcBlockCount(p);
      FOR bi := 0 TO nb - 1 DO
        b  := MSIR.ProcBlock(p, bi);
        ni := MSIR.BlockInsnCount(b);
        FOR ii := 0 TO ni - 1 DO
          ins := MSIR.BlockInsn(b, ii);
          IF MSIR.InsnOp(ins) = MSIR.Op.Call OR
             MSIR.InsnOp(ins) = MSIR.Op.Invoke THEN
            callee := MSIR.InsnCallee(ins);
            IF callee # NIL AND
               NOT IsModuleProc(m, callee) AND
               NOT IsModuleProcByName(m, MSIR.ProcName(callee)) AND
               NOT ProcSeen(externs, callee) THEN
              externs.addhi(callee);
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
    IF HasInvoke(p) THEN
      Wr.PutText(wr, " personality ptr @__gxx_personality_v0");
    END;
    Wr.PutText(wr, " {\n");

    FOR bi := 0 TO nb - 1 DO
      EmitBlock(wr, MSIR.ProcBlock(p, bi));
    END;

    Wr.PutText(wr, "}\n");
  END EmitProc;

(*----------------------------------------------------- global emission *)

PROCEDURE EmitGlobal(wr: Wr.T;  g: MSIR.Global) =
  VAR t := MSIR.GlobalType(g);
  BEGIN
    (* Struct-embedded globals live in @Mod_M3_info, not as standalone globals. *)
    IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN RETURN END;
    Wr.PutText(wr, "@");
    IF MSIR.GlobalIsExternal(g) THEN
      Wr.PutText(wr, MSIR.GlobalName(g));
      Wr.PutText(wr, " = external global ");
      LLType(wr, t);
    ELSE
      Wr.PutText(wr, LLGlobalSym(MSIR.GlobalName(g)));
      Wr.PutText(wr, " = global ");
      IF MSIR.GlobalIsTraced(g) THEN
        Wr.PutText(wr, "ptr null");  (* traced ref slot starts as null ptr *)
      ELSE
        LLType(wr, t);
        Wr.PutText(wr, " zeroinitializer");
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

(*----------------------------------------------- TypeCell / ObjectTypeCell emission *)

(* TypeCell layout (M3RT offsets, 64-bit, all byte values unless noted):
   [0]  typecode       i64  (0, assigned by RTLinker.FixTypes)
   [8]  selfID         i64  (fingerprint)
   [16] fp             i64  (fingerprint, same as selfID)
   [24] traced         i8   (1=traced)
   [25] kind           i8   (6=Ref, 13=Obj)
   [26] link_state     i8   (0=unlinked)
   [27] dataAlignment  i8   (bits, e.g. 64 for INTEGER)
   [28-31]             [4 x i8] padding (to align dataSize to 8 bytes)
   [32] dataSize       i64  (bytes)
   [40] type_map       ptr  (null)
   [48] gc_map         ptr  (null)
   [56] type_desc      ptr  (null)
   [64] initProc       ptr  (null)
   [72] brand          ptr  (null)
   [80] name           ptr  (null)
   [88] next           ptr  (→ next TypeDesc, or null)
   ObjectTypeCell extends at [96]:
   [96]  parentID        i64  (parent fingerprint)
   [104] linkProc        ptr  (null; defaultMethods pre-set instead)
   [112] dataOffset      i64  (bits: byte offset of field region, e.g. 64)
   [120] methodOffset    i64  (0)
   [128] methodSize      i64  (N * address bytes)
   [136] defaultMethods  ptr  (→ vtable array)
   [144] parent          ptr  (null for now) *)

PROCEDURE EmitTextLiterals(wr: Wr.T;  m: MSIR.Module) =
  (* Emit TextLiteral.T globals for every string literal in the module.
     Layout of each @textlit_N:
       { i64 gc_header, ptr method_list, i64 cnt, [len+1 x i8] chars }
     Literal data comes from TextExpr.LiteralCount/Chars/Cnt — the same
     per-module registry the CG path uses (SetUID tracking). *)
  VAR
    GcHeader := VAL(Word.Shift(M3RT.TEXT_typecode, M3RT.RH_typecode_offset), LONGINT);
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
        Wr.PutText(wr, "@textlit_" & Fmt.Int(uid) & " = internal constant { i64, ptr, i64, ["
                       & Fmt.Int(byteCount) & " x i8] } { i64 "
                       & Fmt.LongInt(GcHeader)
                       & ", ptr @textlit_methods, i64 " & Fmt.Int(cnt) & ", ["
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

PROCEDURE EmitTypeCells(wr: Wr.T;  m: MSIR.Module) =
  VAR
    n := MSIR.ModuleTypeDescCount(m);
  BEGIN
    IF n = 0 THEN RETURN END;

    Wr.PutText(wr, "\n; TypeCell / ObjectTypeCell globals\n");
    Wr.PutText(wr, "%TC_t  = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr }\n");
    Wr.PutText(wr, "%OTC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, ptr, i64, i64, i64, ptr, ptr }\n");
    Wr.PutText(wr, "%ATC_t = type { i64, i64, i64, i8, i8, i8, i8, [4 x i8], i64, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64 }\n");

    FOR k := 0 TO n - 1 DO
      VAR
        d       := MSIR.ModuleTypeDesc(m, k);
        nm      := MSIR.TypeDescName(d);
        knd     := MSIR.TypeDescKind(d);
        isObj   := knd = ORD(M3RT.TypeKind.Obj);
        isArr   := knd = ORD(M3RT.TypeKind.Array);
        nextVal : TEXT;
      BEGIN
        (* next pointer: chain TypeCells for MI_type_cells list *)
        IF k < n - 1
          THEN nextVal := "ptr @" & MSIR.TypeDescName(MSIR.ModuleTypeDesc(m, k+1));
          ELSE nextVal := "ptr null";
        END;

        IF isObj THEN
          (* ObjectTypeCell: emit vtable first, then the cell *)
          VAR nMethods := MSIR.TypeDescMethodCount(d);
          BEGIN
            IF nMethods > 0 THEN
              Wr.PutText(wr, "@" & nm & ".methods = internal constant [");
              Wr.PutText(wr, Fmt.Int(nMethods) & " x ptr] [");
              FOR j := 0 TO nMethods - 1 DO
                IF j > 0 THEN Wr.PutText(wr, ", ") END;
                Wr.PutText(wr, "ptr @" & MSIR.TypeDescMethod(d, j));
              END;
              Wr.PutText(wr, "]\n");
            END;
          END;
          Wr.PutText(wr, "@" & nm & " = internal global %OTC_t {\n");
          Wr.PutText(wr, "  i64 0,\n");  (* typecode *)
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n"); (* selfID *)
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n"); (* fp *)
          Wr.PutText(wr, "  i8 " & Fmt.Int(ORD(MSIR.TypeDescTraced(d))) & ",\n");
          Wr.PutText(wr, "  i8 " & Fmt.Int(ORD(M3RT.TypeKind.Obj)) & ",\n");  (* kind = Obj *)
          Wr.PutText(wr, "  i8 0, i8 " & Fmt.Int(MSIR.TypeDescAlign(d)) & ",\n");
          Wr.PutText(wr, "  [4 x i8] zeroinitializer,\n");
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescSize(d)) & ",\n"); (* dataSize *)
          Wr.PutText(wr, "  ptr null, ptr null, ptr null, ptr null, ptr null, ptr null,\n");
          Wr.PutText(wr, "  " & nextVal & ",\n");  (* TC_next *)
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescParentUID(d)) & ",\n"); (* parentID *)
          Wr.PutText(wr, "  ptr null,\n");  (* linkProc *)
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescDataOffset(d)) & ",\n"); (* dataOffset bits *)
          Wr.PutText(wr, "  i64 0,\n");  (* methodOffset *)
          VAR nMeth2 := MSIR.TypeDescMethodCount(d);
          BEGIN
            Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescMethodBytes(d)) & ",\n"); (* methodSize *)
            IF nMeth2 > 0
              THEN Wr.PutText(wr, "  ptr @" & nm & ".methods,\n");
              ELSE Wr.PutText(wr, "  ptr null,\n");
            END;
          END;
          Wr.PutText(wr, "  ptr null\n");  (* parent TypeCell *)
          Wr.PutText(wr, "}\n");
        ELSIF isArr THEN
          (* ArrayTypeCell: plain TC fields + nDimensions + elementSize *)
          Wr.PutText(wr, "@" & nm & " = internal global %ATC_t {\n");
          Wr.PutText(wr, "  i64 0,\n");  (* typecode *)
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n");
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n");
          Wr.PutText(wr, "  i8 " & Fmt.Int(ORD(MSIR.TypeDescTraced(d))) & ",\n");
          Wr.PutText(wr, "  i8 " & Fmt.Int(ORD(M3RT.TypeKind.Array)) & ",\n"); (* kind = Array *)
          Wr.PutText(wr, "  i8 0, i8 " & Fmt.Int(MSIR.TypeDescAlign(d)) & ",\n");
          Wr.PutText(wr, "  [4 x i8] zeroinitializer,\n");
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescSize(d)) & ",\n"); (* dopeSize *)
          Wr.PutText(wr, "  ptr null, ptr null, ptr null, ptr null, ptr null, ptr null,\n");
          Wr.PutText(wr, "  " & nextVal & ",\n");  (* TC_next *)
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescNDimensions(d)) & ",\n"); (* nDimensions *)
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescElementSize(d)) & "\n");  (* elementSize *)
          Wr.PutText(wr, "}\n");
        ELSE
          (* Plain TypeCell (REF, etc.) *)
          Wr.PutText(wr, "@" & nm & " = internal global %TC_t {\n");
          Wr.PutText(wr, "  i64 0,\n");  (* typecode *)
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n");
          Wr.PutText(wr, "  i64 " & Fmt.LongInt(MSIR.TypeDescUID(d)) & ",\n");
          Wr.PutText(wr, "  i8 " & Fmt.Int(ORD(MSIR.TypeDescTraced(d))) & ",\n");
          Wr.PutText(wr, "  i8 " & Fmt.Int(MSIR.TypeDescKind(d)) & ",\n"); (* kind *)
          Wr.PutText(wr, "  i8 0, i8 " & Fmt.Int(MSIR.TypeDescAlign(d)) & ",\n");
          Wr.PutText(wr, "  [4 x i8] zeroinitializer,\n");
          Wr.PutText(wr, "  i64 " & Fmt.Int(MSIR.TypeDescSize(d)) & ",\n");
          Wr.PutText(wr, "  ptr null, ptr null, ptr null, ptr null, ptr null, ptr null,\n");
          Wr.PutText(wr, "  " & nextVal & "\n");  (* TC_next *)
          Wr.PutText(wr, "}\n");
        END;
      END;
    END;
  END EmitTypeCells;

(*----------------------------------------------- TypeLink / cell_ptrs emission *)

(* Emit TypeLink globals (MI_type_cell_ptrs chain) and MSIR_InitTypeLinks.
   Each TypeLink is a { ptr defn, i64 uid } global.
   Chain: TypeLink[0].defn = null (terminus), TypeLink[k].defn = &TypeLink[k-1].
   MI_type_cell_ptrs points to TypeLink[n-1] (head).
   MSIR_InitTypeLinks is a harness helper: for each TypeLink that has a matching
   TypeDesc (same uid and kind prefix), stores the TypeCell address into defn. *)
PROCEDURE EmitTypeLinks(wr: Wr.T;  m: MSIR.Module) =
  VAR
    nLinks := MSIR.ModuleTypeLinkCount(m);
    nDescs := MSIR.ModuleTypeDescCount(m);
  BEGIN
    IF nLinks = 0 THEN
      (* Emit a no-op MSIR_InitTypeLinks so the harness always links. *)
      Wr.PutText(wr, "\ndefine void @MSIR_InitTypeLinks() {\n");
      Wr.PutText(wr, "entry:\n");
      Wr.PutText(wr, "  ret void\n");
      Wr.PutText(wr, "}\n");
      RETURN;
    END;

    Wr.PutText(wr, "\n; TypeLink globals (MI_type_cell_ptrs chain)\n");
    Wr.PutText(wr, "%TypeLink_t = type { ptr, i64 }\n");

    (* Emit each TypeLink global.
       TypeLink[0].defn = null (chain terminus).
       TypeLink[k].defn = ptr @TypeLink[k-1] for k >= 1. *)
    FOR k := 0 TO nLinks - 1 DO
      VAR
        tl   := MSIR.ModuleTypeLink(m, k);
        nm   := MSIR.TypeLinkName(tl);
        uid  := MSIR.TypeLinkUID(tl);
        prev : TEXT;
      BEGIN
        IF k = 0
          THEN prev := "ptr null";
          ELSE prev := "ptr @" & MSIR.TypeLinkName(MSIR.ModuleTypeLink(m, k-1));
        END;
        Wr.PutText(wr, "@" & nm & " = internal global %TypeLink_t { "
                       & prev & ", i64 " & Fmt.LongInt(uid) & " }\n");
      END;
    END;

    (* Emit MSIR_InitTypeLinks: for each TypeLink, if a TypeDesc with
       matching uid and kind exists, store the TypeCell address into defn. *)
    Wr.PutText(wr, "\ndefine void @MSIR_InitTypeLinks() {\n");
    Wr.PutText(wr, "entry:\n");
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
  END EmitTypeLinks;

(*----------------------------------------------- gc_map emission *)

(* Emit a TipeMap byte-array global for module global scanning.
   Returns TRUE if any traced globals exist (and the global was emitted). *)
PROCEDURE EmitGcMapGlobal(wr: Wr.T;  m: MSIR.Module;
                           modName: TEXT;  miBytes: INTEGER): BOOLEAN =
  TYPE ByteArr = REF ARRAY OF INTEGER;
  VAR bytes := NEW(ByteArr, 64);  n := 0;  cursor := 0;

  PROCEDURE AddByte(b: INTEGER) =
  BEGIN
    IF n >= NUMBER(bytes^) THEN
      VAR nb := NEW(ByteArr, 2 * NUMBER(bytes^));
      BEGIN SUBARRAY(nb^, 0, NUMBER(bytes^)) := bytes^; bytes := nb END;
    END;
    bytes[n] := b;  INC(n);
  END AddByte;

  PROCEDURE SkipTo(target: INTEGER) =
  VAR delta := target - cursor;
  BEGIN
    WHILE delta > 0 DO
      IF delta <= 255 THEN
        AddByte(42);  AddByte(delta);   (* SkipF_1 + 1-byte count *)
        INC(cursor, delta);  delta := 0;
      ELSE
        AddByte(43);  (* SkipF_2 + 2-byte little-endian count *)
        AddByte(Word.And(delta, 16_ff));
        AddByte(Word.And(Word.RightShift(delta, 8), 16_ff));
        INC(cursor, delta);  delta := 0;
      END;
    END;
  END SkipTo;

  VAR nGlob := MSIR.ModuleGlobalCount(m);  hasTraced := FALSE;
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
    SkipTo(miBytes);

    (* Visit each traced struct-embedded global in allocation order. *)
    FOR i := 0 TO nGlob - 1 DO
      VAR g   := MSIR.ModuleGlobal(m, i);
          off := MSIR.GlobalByteOffset(g);
      BEGIN
        IF off >= 0 AND NOT MSIR.GlobalIsExternal(g) AND MSIR.GlobalIsTraced(g) THEN
          SkipTo(off);
          AddByte(4);   (* Op.Ref: visit, advance cursor by address size *)
          INC(cursor, Target.Address.bytes);
        END;
      END;
    END;
    AddByte(0);  (* Op.Stop *)

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

(* Emit the RTLinker binder @<Mod>_M3 and RT0.ModuleInfo struct @<Mod>_M3_info.

   The struct layout is derived entirely from M3RT.MI_* byte offsets and
   Target.Address.bytes so it stays correct if ModuleInfo grows new fields.

   Field type rules (from RT0.ModuleInfo):
     - MI_link_state and MI_gc_flags are INTEGER  → i64 in LLVM
     - all other fields are ADDRESS or PROC       → ptr in LLVM
   Fields are AP = Target.Address.bytes bytes each (IP = AP on all targets).

   Binder convention (RT0.Binder):
     mode=0 : return MI pointer (AddUnit path — do NOT run body)
     mode≠0 : run module body then return MI pointer (RunMainBody path) *)
PROCEDURE EmitModuleBinder(wr: Wr.T;  m: MSIR.Module) =
  VAR
    modName    := MSIR.ModuleName(m);
    binderName := modName & "_M3";
    infoName   := "@" & modName & "_M3_info";
    bodyName   := "@" & modName & "__" & modName & "_M3";
    bodyExists := FALSE;
    cs         := Target.Char.size;        (* bits per byte = 8 *)
    ap         := Target.Address.bytes;   (* bytes per field slot *)
    miBytes    := M3RT.MI_SIZE DIV cs;    (* total struct size in bytes *)
    nFields    := miBytes DIV ap;         (* number of fields *)
    nImports   := MSIR.ModuleImportBinderCount(m);
    fieldName  : TEXT;
    fieldType  : TEXT;
    fieldVal   : TEXT;
    byteOff    : INTEGER;
    gcMapName  : TEXT := NIL;  (* NIL if no traced module globals *)
  BEGIN
    <* ASSERT M3RT.MI_SIZE MOD cs = 0,
       "RT0.ModuleInfo size not a multiple of char size" *>
    <* ASSERT miBytes MOD ap = 0,
       "RT0.ModuleInfo byte size not a multiple of address size" *>

    (* Check whether the module body proc was compiled (not abandoned). *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF Text.Equal(MSIR.ProcName(MSIR.ModuleProc(m, i)), modName & "_M3") THEN
        bodyExists := TRUE;
      END;
    END;

    (* Emit RT0.ImportInfo chain — one record per imported module binder.
       Each ImportInfo = { ptr import_ptr, ptr binder_fn, ptr next_ptr }.
       We define @<Mod>_I3 (the interface binder) first so it can be
       referenced without a preceding declare (which would conflict).
       All other binders are external and need explicit declare statements. *)
    IF nImports > 0 THEN
      (* Define the interface binder for this module before the ImportInfo globals. *)
      Wr.PutText(wr, "\ndefine ptr @" & modName & "_I3(i64 %mode) {\n");
      Wr.PutText(wr, "entry:\n");
      Wr.PutText(wr, "  ret ptr " & infoName & "\n");
      Wr.PutText(wr, "}\n");

      Wr.PutText(wr, "\n; RT0.ImportInfo chain for " & modName & "\n");
      (* Declare external binders (skip modName_I3 which we just defined). *)
      FOR k := 0 TO nImports - 1 DO
        VAR b := MSIR.ModuleImportBinder(m, k);
        BEGIN
          IF NOT Text.Equal(b, modName & "_I3") THEN
            Wr.PutText(wr, "declare ptr @" & b & "(i64)\n");
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

    (* Emit named type — field types derived from M3RT offsets. *)
    Wr.PutText(wr, "\n");
    Wr.PutText(wr, "; RT0.ModuleInfo for " & modName
                   & " (" & Fmt.Int(nFields) & " fields, "
                   & Fmt.Int(miBytes) & " bytes)\n");
    Wr.PutText(wr, "%RT0_ModuleInfo_t = type { ");
    FOR k := 0 TO nFields - 1 DO
      IF k > 0 THEN Wr.PutText(wr, ", ") END;
      byteOff := k * ap;
      IF byteOff = M3RT.MI_link_state DIV cs OR byteOff = M3RT.MI_gc_flags DIV cs
        THEN Wr.PutText(wr, "i64");
        ELSE Wr.PutText(wr, "ptr");
      END;
    END;
    (* Append user global fields to the struct type. *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      VAR g := MSIR.ModuleGlobal(m, i);
      BEGIN
        IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN
          Wr.PutText(wr, ", ");
          IF MSIR.GlobalIsTraced(g)
            THEN Wr.PutText(wr, "ptr");  (* traced ref: always ptr *)
            ELSE LLType(wr, MSIR.GlobalType(g));
          END;
        END;
      END;
    END;
    Wr.PutText(wr, " }\n");

    (* Emit global initializer — values derived from M3RT offsets. *)
    Wr.PutText(wr, infoName & " = global %RT0_ModuleInfo_t {\n");
    FOR k := 0 TO nFields - 1 DO
      byteOff := k * ap;
      (* Determine LLVM type, value, and M3RT field name for this slot. *)
      IF    byteOff = M3RT.MI_file DIV cs           THEN fieldType := "ptr"; fieldVal := "null";             fieldName := "MI_file";
      ELSIF byteOff = M3RT.MI_type_cells DIV cs     THEN fieldType := "ptr";
                                                          fieldName := "MI_type_cells";
                                                          IF MSIR.ModuleTypeDescCount(m) > 0
                                                            THEN fieldVal := "@" & MSIR.TypeDescName(MSIR.ModuleTypeDesc(m, 0));
                                                            ELSE fieldVal := "null";
                                                          END;
      ELSIF byteOff = M3RT.MI_type_cell_ptrs DIV cs THEN fieldType := "ptr";
                                                          fieldName := "MI_type_cell_ptrs";
                                                          IF MSIR.ModuleTypeLinkCount(m) > 0
                                                            THEN fieldVal := "@" & MSIR.TypeLinkName(MSIR.ModuleTypeLink(m, MSIR.ModuleTypeLinkCount(m)-1));
                                                            ELSE fieldVal := "null";
                                                          END;
      ELSIF byteOff = M3RT.MI_full_rev DIV cs       THEN fieldType := "ptr"; fieldVal := "null";             fieldName := "MI_full_rev";
      ELSIF byteOff = M3RT.MI_part_rev DIV cs       THEN fieldType := "ptr"; fieldVal := "null";             fieldName := "MI_part_rev";
      ELSIF byteOff = M3RT.MI_proc_info DIV cs      THEN fieldType := "ptr"; fieldVal := "null";             fieldName := "MI_proc_info";
      ELSIF byteOff = M3RT.MI_try_scopes DIV cs     THEN fieldType := "ptr"; fieldVal := "null";             fieldName := "MI_try_scopes";
      ELSIF byteOff = M3RT.MI_var_map DIV cs        THEN fieldType := "ptr";
                                                          fieldName := "MI_var_map";
                                                          IF gcMapName # NIL
                                                            THEN fieldVal := "@" & gcMapName;
                                                            ELSE fieldVal := "null";
                                                          END;
      ELSIF byteOff = M3RT.MI_gc_map DIV cs         THEN fieldType := "ptr";
                                                          fieldName := "MI_gc_map";
                                                          IF gcMapName # NIL
                                                            THEN fieldVal := "@" & gcMapName;
                                                            ELSE fieldVal := "null";
                                                          END;
      ELSIF byteOff = M3RT.MI_imports DIV cs        THEN fieldType := "ptr";
                                                          fieldName := "MI_imports";
                                                          IF nImports > 0
                                                            THEN fieldVal := "@" & modName & "_M3_imp.0";
                                                            ELSE fieldVal := "null";
                                                          END;
      ELSIF byteOff = M3RT.MI_link_state DIV cs     THEN fieldType := "i64"; fieldVal := "0";               fieldName := "MI_link_state";
      ELSIF byteOff = M3RT.MI_binder DIV cs         THEN fieldType := "ptr"; fieldVal := "@" & binderName;  fieldName := "MI_binder";
      ELSIF byteOff = M3RT.MI_gc_flags DIV cs       THEN fieldType := "i64";
                                                   (* RT0.GC_both = GC_gen | GC_inc = 3; literal used
                                                      because RT0 is in m3core, not m3middle. *)
                                                   fieldVal := "3";                                   fieldName := "MI_gc_flags";
      ELSE                                         fieldType := "ptr"; fieldVal := "null";             fieldName := "?";
      END;
      IF k < nFields - 1
        THEN Wr.PutText(wr, "  " & fieldType & " " & fieldVal & ",");
        ELSE (* Last standard field — comma only if user globals follow. *)
             VAR hasEmbedded := FALSE;
             BEGIN
               FOR gi := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
                 VAR g := MSIR.ModuleGlobal(m, gi);
                 BEGIN
                   IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN
                     hasEmbedded := TRUE;
                   END;
                 END;
               END;
               IF hasEmbedded
                 THEN Wr.PutText(wr, "  " & fieldType & " " & fieldVal & ",");
                 ELSE Wr.PutText(wr, "  " & fieldType & " " & fieldVal);
               END;
             END;
      END;
      Wr.PutText(wr, "  ; " & fieldName & " (+" & Fmt.Int(byteOff) & ")\n");
    END;
    (* Append zero initializers for struct-embedded user globals. *)
    VAR embGlobs: RefSeq.T := NEW(RefSeq.T).init();
    BEGIN
      FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
        VAR g := MSIR.ModuleGlobal(m, i);
        BEGIN
          IF MSIR.GlobalByteOffset(g) >= 0 AND NOT MSIR.GlobalIsExternal(g) THEN
            embGlobs.addhi(g);
          END;
        END;
      END;
      FOR i := 0 TO embGlobs.size() - 1 DO
        VAR g: MSIR.Global := embGlobs.get(i);
        BEGIN
          IF MSIR.GlobalIsTraced(g)
            THEN Wr.PutText(wr, "  ptr null");
            ELSE Wr.PutText(wr, "  "); LLType(wr, MSIR.GlobalType(g));
                 Wr.PutText(wr, " zeroinitializer");
          END;
          IF i < embGlobs.size() - 1 THEN Wr.PutText(wr, ",") END;
          Wr.PutText(wr, "  ; " & MSIR.GlobalName(g)
                         & " (+" & Fmt.Int(MSIR.GlobalByteOffset(g)) & ")\n");
        END;
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
          VAR lltype: TEXT;
          BEGIN
            IF MSIR.GlobalIsTraced(g)
              THEN lltype := "ptr";
              ELSE lltype := LLTypeStr(MSIR.GlobalType(g));
            END;
            Wr.PutText(wr, "@" & LLGlobalSym(MSIR.GlobalName(g))
                           & " = alias " & lltype
                           & ", ptr getelementptr inbounds (i8, ptr "
                           & infoName & ", i64 " & Fmt.Int(off) & ")\n");
          END;
        END;
      END;
    END;

    (* Interface binder @<Mod>_I3 — only needed when no imports section
       emitted it already (that section defines it first to avoid declare
       conflicts).  For modules with no imports, emit it here. *)
    IF nImports = 0 THEN
      Wr.PutText(wr, "\ndefine ptr @" & modName & "_I3(i64 %mode) {\n");
      Wr.PutText(wr, "entry:\n");
      Wr.PutText(wr, "  ret ptr " & infoName & "\n");
      Wr.PutText(wr, "}\n");
    END;

    (* Binder function: mode=0 → return MI; mode≠0 → run body + return MI. *)
    Wr.PutText(wr, "\ndefine ptr @" & binderName & "(i64 %mode) {\n");
    IF bodyExists THEN
      Wr.PutText(wr, "entry:\n");
      Wr.PutText(wr, "  %do_body = icmp ne i64 %mode, 0\n");
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
  END EmitModuleBinder;

(*------------------------------------------------------ module emission *)

PROCEDURE ModuleHasEH(m: MSIR.Module): BOOLEAN =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      IF HasInvoke(MSIR.ModuleProc(m, i)) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
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

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module) =
  VAR
    externs    := NEW(RefSeq.T).init();
    triple     := MSIR.ModuleTriple(m);
    datalayout := MSIR.ModuleDataLayout(m);
    needsEH    : BOOLEAN;
    needsGC    : BOOLEAN;
  BEGIN
    curEmitModule := m;
    auxN          := 0;
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

    (* EH externs — emitted once per module when any proc uses invoke *)
    IF needsEH THEN
      Wr.PutText(wr, "@_ZTI6_M3Exc = external constant ptr\n");
      Wr.PutText(wr, "declare i32 @__gxx_personality_v0(...)\n");
      (* __cxa_get_exception_ptr, __cxa_begin_catch, __cxa_end_catch are all
         declared automatically by CollectExterns (they appear as Call callees
         in catch landingpads). *)
      Wr.PutText(wr, "\n");
    END;

    (* GC barrier externs — emitted when any proc uses gc.load / gc.store *)
    IF needsGC THEN
      Wr.PutText(wr, "declare void @"
                     & LLHookName(MSIR.ModuleGCLoadBarrier(m),
                                   "RTHooks__CheckLoadTracedRef")
                     & "(ptr)\n");
      Wr.PutText(wr, "declare void @"
                     & LLHookName(MSIR.ModuleGCStoreBarrier(m),
                                   "RTHooks__CheckStoreTraced")
                     & "(ptr)\n");
      Wr.PutText(wr, "\n");
    END;

    (* globals *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      EmitGlobal(wr, MSIR.ModuleGlobal(m, i));
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
        EmitDeclare(wr, externs.get(i));
      END;
    END;

    (* internal proc definitions *)
    FOR i := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      EmitProc(wr, MSIR.ModuleProc(m, i));
    END;

    (* TextLiteral globals *)
    EmitTextLiterals(wr, m);

    (* CONST array globals *)
    EmitConstArrays(wr, m);

    (* TypeCell / ObjectTypeCell globals for type_cells *)
    EmitTypeCells(wr, m);

    (* TypeLink globals for type_cell_ptrs chain, plus MSIR_InitTypeLinks *)
    EmitTypeLinks(wr, m);

    (* Exception descriptors: { i64 uid, ptr null, i64 0 } = ExceptionDesc *)
    FOR i := 0 TO MSIR.ModuleExcDescCount(m) - 1 DO
      VAR d := MSIR.ModuleExcDesc(m, i);
      BEGIN
        Wr.PutText(wr, "\n@" & MSIR.ExcDescName(d)
                       & " = internal global { i64, ptr, i64 } { i64 "
                       & Fmt.LongInt(MSIR.ExcDescUID(d))
                       & ", ptr null, i64 0 }\n");
      END;
    END;

    (* RTLinker binder and ModuleInfo descriptor *)
    EmitModuleBinder(wr, m);

    (* Emit mutable type-table globals collected during typecase emission.
       ScanTypecase lazily fills the defn pointer so these must be global
       (not constant).  TypecaseCell = { ptr defn, i64 uid } (16 bytes). *)
    IF pendingTC # NIL AND pendingTC.size() > 0 THEN
      Wr.PutText(wr, "\n; TYPECASE type tables ("
                     & LLHookName(MSIR.ModuleScanTypecase(m),
                                   "RTHooks__ScanTypecase")
                     & ")\n");
      Wr.PutText(wr, "declare i64 @"
                     & LLHookName(MSIR.ModuleScanTypecase(m),
                                   "RTHooks__ScanTypecase")
                     & "(ptr, ptr)\n");
      FOR ti := 0 TO pendingTC.size() - 1 DO
        VAR ent := NARROW(pendingTC.get(ti), TCEntry);
            n   := NUMBER(ent.uids^);
        BEGIN
          Wr.PutText(wr, ent.name & " = internal global [" & Fmt.Int(n));
          Wr.PutText(wr, " x { ptr, i64 }] [");
          FOR k := 0 TO n - 1 DO
            IF k > 0 THEN Wr.PutText(wr, ", ") END;
            (* Each element needs the struct type prefix in LLVM array literals. *)
            Wr.PutText(wr, "{ ptr, i64 } { ptr null, i64 " & Fmt.LongInt(ent.uids[k]) & " }");
          END;
          Wr.PutText(wr, "]\n");
        END;
      END;
      pendingTC := NIL;
    END;
  END Module;

BEGIN
END MSIRToLLVM.
