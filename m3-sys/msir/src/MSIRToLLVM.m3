MODULE MSIRToLLVM;

IMPORT MSIR, Wr, Fmt, Thread, Text, RefSeq, TextWr;
<*FATAL Thread.Alerted, Wr.Failure*>

(*----------------------------------------------------- module-level state *)

VAR
  auxN:          INTEGER     := 0;
  curEmitModule: MSIR.Module := NIL;

(*------------------------------------------------------ auxiliary naming *)

PROCEDURE NewAux(): TEXT =
  BEGIN INC(auxN); RETURN "%__ll" & Fmt.Int(auxN) END NewAux;

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

(* Emit just the LLVM name/constant for a value (no type prefix). *)
PROCEDURE LLOpVal(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "undef"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Wr.PutText(wr, Fmt.LongInt(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "null");
    | MSIR.ValueKind.GlobalRef =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, LLGlobalSym(MSIR.ValueName(v)));
    ELSE
        (* InsnResult names already have % prefix; proc param names are bare. *)
        VAR n: TEXT := MSIR.ValueName(v);
        BEGIN
          IF Text.Length(n) > 0 AND Text.GetChar(n, 0) # '%' THEN
            Wr.PutText(wr, "%");
          END;
          Wr.PutText(wr, n);
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

   Header layout (RT0.RefHeader = one word at -8 relative to object ptr):
     bit 22 = gray bit (RH_gray_offset = 22, mask = 1<<22 = 4194304) *)
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
                   & " = getelementptr i8, ptr " & refName & ", i64 -8\n");
    Wr.PutText(wr, "  %__gc_hdr."  & n
                   & " = load i64, ptr %__gc_hptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_gb."   & n
                   & " = and i64 %__gc_hdr." & n & ", 4194304\n");
    Wr.PutText(wr, "  %__gc_gr."   & n
                   & " = icmp ne i64 %__gc_gb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_gr." & n
                   & ", label %gc.slow." & n & ", label %gc.skip." & n & "\n");
    (* slow path *)
    Wr.PutText(wr, "gc.slow." & n & ":\n");
    Wr.PutText(wr, "  call void @RTHooks__CheckLoadTracedRef(ptr " & refName & ")\n");
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

   Fast path: read the header word (one word = 8 bytes before the object
   pointer); if the dirty bit (bit 21, mask 2097152 = 1<<21) is already
   set, skip the slow-path call.  Otherwise call RTHooks__CheckStoreTraced.

   Header layout matches RT0.RefHeaderBits:
     bit 0:    forwarded
     bits 1-20: typecode
     bit 21:   dirty  (RH_dirty_offset)
     bit 22:   gray *)
PROCEDURE EmitGcWriteBarrier(wr: Wr.T;  containerName: TEXT) =
  VAR n: TEXT;
  BEGIN
    INC(auxN);
    n := Fmt.Int(auxN);
    (* Read object header; skip barrier if already dirty. *)
    Wr.PutText(wr, "  %__gc_whptr." & n
                   & " = getelementptr i8, ptr " & containerName & ", i64 -8\n");
    Wr.PutText(wr, "  %__gc_whdr."  & n
                   & " = load i64, ptr %__gc_whptr." & n & "\n");
    Wr.PutText(wr, "  %__gc_wdb."   & n
                   & " = and i64 %__gc_whdr." & n & ", 2097152\n");
    Wr.PutText(wr, "  %__gc_wdirty." & n
                   & " = icmp ne i64 %__gc_wdb." & n & ", 0\n");
    Wr.PutText(wr, "  br i1 %__gc_wdirty." & n
                   & ", label %gc.wskip." & n & ", label %gc.wslow." & n & "\n");
    Wr.PutText(wr, "gc.wslow." & n & ":\n");
    Wr.PutText(wr, "  call void @RTHooks__CheckStoreTraced(ptr " & containerName & ")\n");
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
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = alloca ");
        LLType(wr, MSIR.InsnTargetType(i));
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

    | MSIR.Op.IAdd => EmitBinop(wr, "add",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.ISub => EmitBinop(wr, "sub",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));
    | MSIR.Op.IMul => EmitBinop(wr, "mul",  res, MSIR.InsnOperand(i,0), MSIR.InsnOperand(i,1));

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

    | MSIR.Op.LandingPad =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = landingpad ");
        LLType(wr, MSIR.ValueType(res));
        Wr.PutText(wr, "\n");
        IF MSIR.InsnIsCleanup(i) THEN
          Wr.PutText(wr, "          cleanup\n");
        ELSE
          Wr.PutText(wr, "          catch ptr @_ZTI7_M3Exc\n");
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
          Wr.PutText(wr, ", i64 0, i64 ");
          LLOpVal(wr, idxV);
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
          IF srcBits > 0 AND dstBits > 0 THEN
            IF dstBits > srcBits    THEN convOp := "sext";
            ELSIF dstBits < srcBits THEN convOp := "trunc";
            ELSE                         convOp := "bitcast";
            END;
          ELSE
            convOp := "bitcast";
          END;
          Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = " & convOp & " ");
          LLTypedVal(wr, src);
          Wr.PutText(wr, " to ");
          LLType(wr, dstT);
          Wr.PutText(wr, "\n");
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
          IF MSIR.InsnOp(MSIR.BlockInsn(b, ii)) = MSIR.Op.Invoke THEN
            RETURN TRUE;
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
    Wr.PutText(wr, "@");
    Wr.PutText(wr, LLGlobalSym(MSIR.GlobalName(g)));
    Wr.PutText(wr, " = global ");
    IF MSIR.GlobalIsTraced(g) THEN
      Wr.PutText(wr, "ptr null");  (* traced ref slot starts as null ptr *)
    ELSE
      LLType(wr, t);
      Wr.PutText(wr, " zeroinitializer");
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
      Wr.PutText(wr, "@_ZTI7_M3Exc = external constant ptr\n");
      Wr.PutText(wr, "declare i32 @__gxx_personality_v0(...)\n");
      Wr.PutText(wr, "\n");
    END;

    (* GC barrier externs — emitted when any proc uses gc.load / gc.store *)
    IF needsGC THEN
      Wr.PutText(wr, "declare void @RTHooks__CheckLoadTracedRef(ptr)\n");
      Wr.PutText(wr, "declare void @RTHooks__CheckStoreTraced(ptr)\n");
      Wr.PutText(wr, "\n");
    END;

    (* globals *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      EmitGlobal(wr, MSIR.ModuleGlobal(m, i));
    END;

    (* collect extern callees *)
    CollectExterns(m, externs);
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
  END Module;

BEGIN
END MSIRToLLVM.
