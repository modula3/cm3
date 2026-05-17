MODULE MSIRVerifier;

IMPORT MSIR, RefSeq, Fmt, Text;

TYPE Ctx = REF RECORD
  errors: RefSeq.T;        (* of TEXT *)
  procName: TEXT;
  blockLabel: TEXT;
  insnIndex: INTEGER;
END;

PROCEDURE Loc(c: Ctx): TEXT =
  BEGIN
    RETURN c.procName & ": block " & c.blockLabel
         & ": insn " & Fmt.Int(c.insnIndex) & ": ";
  END Loc;

PROCEDURE Err(c: Ctx; msg: TEXT) =
  BEGIN
    c.errors.addhi(Loc(c) & msg);
  END Err;

PROCEDURE ErrAt(c: Ctx;  where, msg: TEXT) =
  BEGIN
    c.errors.addhi(c.procName & ": " & where & ": " & msg);
  END ErrAt;

(*------------------------------------------ terminator-classification *)

PROCEDURE IsTerminator(op: MSIR.Op): BOOLEAN =
  BEGIN
    CASE op OF
    | MSIR.Op.Br, MSIR.Op.CondBr, MSIR.Op.Ret, MSIR.Op.Unreachable,
      MSIR.Op.UnwindTo, MSIR.Op.RetThroughEnvelope,
      MSIR.Op.Raise, MSIR.Op.Typecase,
      MSIR.Op.Invoke, MSIR.Op.InvokeIndirect, MSIR.Op.Resume => RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END IsTerminator;

(*------------------------------------------ helper predicates *)

PROCEDURE IsScalar (t: MSIR.T): BOOLEAN =
  BEGIN
    CASE MSIR.Kind(t) OF
    | MSIR.TypeKind.I1,  MSIR.TypeKind.I8,  MSIR.TypeKind.I16,
      MSIR.TypeKind.I32, MSIR.TypeKind.I64,
      MSIR.TypeKind.W8,  MSIR.TypeKind.W16,
      MSIR.TypeKind.W32, MSIR.TypeKind.W64,
      MSIR.TypeKind.F32, MSIR.TypeKind.F64, MSIR.TypeKind.F128 =>
      RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsScalar;

(*------------------------------------------ instruction-level checks *)

PROCEDURE CheckBranchTarget(c: Ctx;  i: MSIR.Insn;  k: INTEGER) =
  VAR
    target := MSIR.InsnBrTarget(i, k);
    nArgs  := MSIR.InsnBrArgCount(i, k);
    nParams: INTEGER;
  BEGIN
    IF target = NIL THEN
      Err(c, "branch target is NIL");
      RETURN;
    END;
    nParams := MSIR.BlockParamCount(target);
    IF nArgs # nParams THEN
      Err(c, "branch arg count " & Fmt.Int(nArgs)
           & " does not match target '" & MSIR.BlockLabel(target)
           & "' parameter count " & Fmt.Int(nParams));
      RETURN;
    END;
    FOR j := 0 TO nArgs - 1 DO
      IF NOT MSIR.Equal(MSIR.ValueType(MSIR.InsnBrArg(i, k, j)),
                        MSIR.ValueType(MSIR.BlockParamValue(target, j))) THEN
        Err(c, "branch arg " & Fmt.Int(j) & " type mismatch with target '"
             & MSIR.BlockLabel(target) & "'");
      END;
    END;
  END CheckBranchTarget;

PROCEDURE CheckBinaryArith(c: Ctx;  i: MSIR.Insn) =
  VAR
    a := MSIR.InsnOperand(i, 0);
    b := MSIR.InsnOperand(i, 1);
  BEGIN
    IF MSIR.InsnOperandCount(i) # 2 THEN
      Err(c, "binary op expects 2 operands");
      RETURN;
    END;
    IF NOT MSIR.Equal(MSIR.ValueType(a), MSIR.ValueType(b)) THEN
      Err(c, "binary op operand type mismatch");
    END;
  END CheckBinaryArith;

PROCEDURE IsPointerLike(t: MSIR.T): BOOLEAN =
  VAR k := MSIR.Kind(t);
  BEGIN
    RETURN k = MSIR.TypeKind.Ptr OR k = MSIR.TypeKind.GcRef OR k = MSIR.TypeKind.GcSlot;
  END IsPointerLike;

PROCEDURE CheckICmp(c: Ctx;  i: MSIR.Insn) =
  VAR a, b: MSIR.Value;
  BEGIN
    IF MSIR.InsnOperandCount(i) # 2 THEN
      Err(c, "icmp expects 2 operands");
      RETURN;
    END;
    a := MSIR.InsnOperand(i, 0);
    b := MSIR.InsnOperand(i, 1);
    IF NOT MSIR.Equal(MSIR.ValueType(a), MSIR.ValueType(b)) THEN
      (* In LLVM opaque-pointer mode all pointer kinds lower to ptr — compatible *)
      IF NOT (IsPointerLike(MSIR.ValueType(a)) AND IsPointerLike(MSIR.ValueType(b))) THEN
        Err(c, "icmp operand type mismatch");
      END;
    END;
    IF MSIR.Kind(MSIR.ValueType(MSIR.InsnResult(i))) # MSIR.TypeKind.I1 THEN
      Err(c, "icmp result must be i1");
    END;
  END CheckICmp;

PROCEDURE CheckCondBr(c: Ctx;  i: MSIR.Insn) =
  BEGIN
    IF MSIR.InsnOperandCount(i) # 1 THEN
      Err(c, "cond_br expects 1 operand");
      RETURN;
    END;
    IF MSIR.Kind(MSIR.ValueType(MSIR.InsnOperand(i, 0)))
         # MSIR.TypeKind.I1 THEN
      Err(c, "cond_br condition must be i1");
    END;
    CheckBranchTarget(c, i, 0);
    CheckBranchTarget(c, i, 1);
  END CheckCondBr;

PROCEDURE CheckGcLoad(c: Ctx;  i: MSIR.Insn) =
  VAR slotT: MSIR.T;
  BEGIN
    IF MSIR.InsnOperandCount(i) # 1 THEN
      Err(c, "gc.load expects 1 operand"); RETURN;
    END;
    slotT := MSIR.ValueType(MSIR.InsnOperand(i, 0));
    IF MSIR.Kind(slotT) # MSIR.TypeKind.GcSlot THEN
      Err(c, "gc.load operand must be gc_slot");
    END;
  END CheckGcLoad;

PROCEDURE CheckGcStore(c: Ctx;  i: MSIR.Insn) =
  VAR n := MSIR.InsnOperandCount(i);  slotT, contT: MSIR.T;
  BEGIN
    IF n # 2 AND n # 3 THEN
      Err(c, "gc.store expects 2 or 3 operands"); RETURN;
    END;
    (* ops[0]=value, ops[1]=slot, ops[2]=container (optional). *)
    slotT := MSIR.ValueType(MSIR.InsnOperand(i, 1));
    IF MSIR.Kind(slotT) # MSIR.TypeKind.GcSlot THEN
      Err(c, "gc.store second operand must be gc_slot");
    END;
    IF n = 3 THEN
      contT := MSIR.ValueType(MSIR.InsnOperand(i, 2));
      IF MSIR.Kind(contT) # MSIR.TypeKind.GcRef AND
         MSIR.Kind(contT) # MSIR.TypeKind.Ptr THEN
        Err(c, "gc.store third operand (container) must be gc_ref or ptr");
      END;
    END;
  END CheckGcStore;

PROCEDURE CheckCall(c: Ctx;  i: MSIR.Insn) =
  VAR
    callee := MSIR.InsnCallee(i);
    nArgs  := MSIR.InsnOperandCount(i);
    nParams: INTEGER;
  BEGIN
    IF callee = NIL THEN
      Err(c, "call/invoke callee is NIL"); RETURN;
    END;
    nParams := MSIR.ProcParamCount(callee);
    IF nArgs # nParams THEN
      Err(c, "call to '" & MSIR.ProcName(callee)
           & "': arg count " & Fmt.Int(nArgs)
           & " does not match procedure parameter count "
           & Fmt.Int(nParams));
    END;
  END CheckCall;

PROCEDURE CheckDispatch(c: Ctx;  i: MSIR.Insn) =
  VAR
    obj := MSIR.InsnOperand(i, 0);
    objT := MSIR.ValueType(obj);
    selector := MSIR.InsnSelector(i);
    eltT: MSIR.T;
  BEGIN
    IF MSIR.Kind(objT) = MSIR.TypeKind.GcRef THEN
      eltT := MSIR.EltType(objT);
    ELSE
      eltT := objT;
    END;
    IF MSIR.Kind(eltT) # MSIR.TypeKind.Object THEN
      Err(c, "dispatch operand must be Object-typed"); RETURN;
    END;
    IF MSIR.ObjectMethodIndex(eltT, selector) < 0 THEN
      Err(c, "dispatch selector '" & selector
           & "' not found in type '" & MSIR.ObjectName(eltT) & "'");
    END;
  END CheckDispatch;

PROCEDURE CheckTypecase(c: Ctx;  i: MSIR.Insn) =
  VAR
    nClauses := MSIR.InsnTypecaseClauseCount(i);
    sawElse := FALSE;
  BEGIN
    IF nClauses = 0 THEN
      Err(c, "typecase has no clauses"); RETURN;
    END;
    FOR k := 0 TO nClauses - 1 DO
      VAR cl := MSIR.InsnTypecaseClause(i, k);
      BEGIN
        IF cl.isElse THEN sawElse := TRUE END;
        IF cl.block = NIL THEN
          Err(c, "typecase clause " & Fmt.Int(k) & " has NIL block");
        END;
      END;
    END;
    IF NOT sawElse THEN
      Err(c, "typecase missing mandatory ELSE clause");
    END;
  END CheckTypecase;

PROCEDURE CheckRet(c: Ctx;  i: MSIR.Insn;  resultT: MSIR.T) =
  BEGIN
    IF MSIR.InsnOperandCount(i) = 0 THEN
      IF MSIR.Kind(resultT) # MSIR.TypeKind.Void THEN
        Err(c, "ret without value, but procedure returns non-void");
      END;
    ELSE
      IF MSIR.Kind(resultT) = MSIR.TypeKind.Void THEN
        Err(c, "ret with value, but procedure returns void");
      ELSIF NOT MSIR.Equal(MSIR.ValueType(MSIR.InsnOperand(i, 0)), resultT) THEN
        Err(c, "ret value type does not match procedure result type");
      END;
    END;
  END CheckRet;

PROCEDURE CheckLoad(c: Ctx;  i: MSIR.Insn) =
  VAR res   := MSIR.InsnResult(i);
      addrT : MSIR.T;
  BEGIN
    IF MSIR.InsnOperandCount(i) # 1 THEN Err(c, "load expects 1 operand"); RETURN END;
    addrT := MSIR.ValueType(MSIR.InsnOperand(i, 0));
    IF MSIR.Kind(addrT) # MSIR.TypeKind.Ptr AND
       MSIR.Kind(addrT) # MSIR.TypeKind.GcRef AND
       MSIR.Kind(addrT) # MSIR.TypeKind.GcSlot THEN
      Err(c, "load address must be ptr, gc_ref, or gc_slot type"); RETURN;
    END;
    IF res = NIL THEN Err(c, "load must have a result"); RETURN END;
    IF MSIR.Kind(MSIR.EltType(addrT)) # MSIR.TypeKind.Void AND
       NOT MSIR.Equal(MSIR.EltType(addrT), MSIR.ValueType(res)) THEN
      Err(c, "load result type does not match pointer element type");
    END;
  END CheckLoad;

PROCEDURE CheckStore(c: Ctx;  i: MSIR.Insn) =
  VAR valT, addrT: MSIR.T;
  BEGIN
    IF MSIR.InsnOperandCount(i) # 2 THEN Err(c, "store expects 2 operands"); RETURN END;
    valT  := MSIR.ValueType(MSIR.InsnOperand(i, 0));
    addrT := MSIR.ValueType(MSIR.InsnOperand(i, 1));
    IF MSIR.Kind(addrT) # MSIR.TypeKind.Ptr AND
       MSIR.Kind(addrT) # MSIR.TypeKind.GcRef AND
       MSIR.Kind(addrT) # MSIR.TypeKind.GcSlot THEN
      Err(c, "store destination must be ptr, gc_ref, or gc_slot type"); RETURN;
    END;
    IF MSIR.Kind(MSIR.EltType(addrT)) # MSIR.TypeKind.Void AND
       NOT MSIR.Equal(MSIR.EltType(addrT), valT) THEN
      (* In LLVM opaque-pointer mode pointer types are all compatible *)
      IF NOT (IsPointerLike(MSIR.EltType(addrT)) AND IsPointerLike(valT)) THEN
        Err(c, "store value type does not match pointer element type");
      END;
    END;
    IF MSIR.InsnResult(i) # NIL THEN Err(c, "store must not have a result") END;
  END CheckStore;

PROCEDURE CheckAlloca(c: Ctx;  i: MSIR.Insn) =
  VAR res     := MSIR.InsnResult(i);
      targetT := MSIR.InsnTargetType(i);
      resT    : MSIR.T;
  BEGIN
    IF res = NIL THEN Err(c, "alloca must have a result"); RETURN END;
    resT := MSIR.ValueType(res);
    IF MSIR.Kind(resT) # MSIR.TypeKind.Ptr THEN
      Err(c, "alloca result must be ptr type"); RETURN;
    END;
    IF targetT # NIL AND NOT MSIR.Equal(MSIR.EltType(resT), targetT) THEN
      Err(c, "alloca result ptr element type does not match declared target type");
    END;
  END CheckAlloca;

PROCEDURE CheckConvert(c: Ctx;  i: MSIR.Insn) =
  VAR res := MSIR.InsnResult(i);
  BEGIN
    IF MSIR.InsnOperandCount(i) # 1 THEN Err(c, "convert expects 1 operand"); RETURN END;
    IF res = NIL THEN Err(c, "convert must have a result"); RETURN END;
    (* Allow ptr↔scalar (inttoptr / ptrtoint) in addition to scalar↔scalar. *)
    VAR srcT := MSIR.ValueType(MSIR.InsnOperand(i, 0));  dstT := MSIR.ValueType(res);
        srcOk := IsScalar(srcT)
              OR MSIR.Kind(srcT) = MSIR.TypeKind.Ptr
              OR MSIR.Kind(srcT) = MSIR.TypeKind.GcRef
              OR MSIR.Kind(srcT) = MSIR.TypeKind.GcSlot;
        dstOk := IsScalar(dstT)
              OR MSIR.Kind(dstT) = MSIR.TypeKind.Ptr
              OR MSIR.Kind(dstT) = MSIR.TypeKind.GcRef
              OR MSIR.Kind(dstT) = MSIR.TypeKind.GcSlot;
    BEGIN
      IF NOT srcOk THEN Err(c, "convert source must be a scalar or ptr type") END;
      IF NOT dstOk THEN Err(c, "convert target must be a scalar or ptr type") END;
    END;
  END CheckConvert;

PROCEDURE CheckFieldAddr(c: Ctx;  i: MSIR.Insn) =
  VAR
    objT, innerT : MSIR.T;
    fname        : TEXT;
    res          := MSIR.InsnResult(i);
    found        : BOOLEAN;
  BEGIN
    IF MSIR.InsnOperandCount(i) # 1 THEN Err(c, "field_addr expects 1 operand"); RETURN END;
    objT := MSIR.ValueType(MSIR.InsnOperand(i, 0));
    CASE MSIR.Kind(objT) OF
    | MSIR.TypeKind.Ptr, MSIR.TypeKind.GcRef, MSIR.TypeKind.GcSlot =>
        innerT := MSIR.EltType(objT);
    ELSE Err(c, "field_addr: object must be ptr or gc_ref"); RETURN;
    END;
    fname := MSIR.InsnSelector(i);
    CASE MSIR.Kind(innerT) OF
    | MSIR.TypeKind.Struct =>
        found := FALSE;
        FOR k := 0 TO MSIR.StructFieldCount(innerT) - 1 DO
          IF Text.Equal(MSIR.StructField(innerT, k).name, fname) THEN found := TRUE END;
        END;
        IF NOT found THEN Err(c, "field_addr: field '" & fname & "' not found") END;
    | MSIR.TypeKind.Object =>
        IF MSIR.ObjectFieldIndex(innerT, fname) < 0 THEN
          Err(c, "field_addr: field '" & fname & "' not found in '"
               & MSIR.ObjectName(innerT) & "'");
        END;
    ELSE Err(c, "field_addr: inner type must be Struct or Object");
    END;
    IF res = NIL THEN Err(c, "field_addr must have a result"); RETURN END;
    IF MSIR.Kind(MSIR.ValueType(res)) # MSIR.TypeKind.Ptr THEN
      Err(c, "field_addr result must be ptr type");
    END;
  END CheckFieldAddr;

PROCEDURE CheckArrayElemAddr(c: Ctx;  i: MSIR.Insn) =
  VAR
    arrT  : MSIR.T;
    res   := MSIR.InsnResult(i);
  BEGIN
    IF MSIR.InsnOperandCount(i) # 2 THEN
      Err(c, "array.elem_addr expects 2 operands"); RETURN;
    END;
    arrT := MSIR.ValueType(MSIR.InsnOperand(i, 0));
    IF MSIR.Kind(arrT) # MSIR.TypeKind.Ptr THEN
      Err(c, "array.elem_addr: operand must be ptr-to-fixed-array"); RETURN;
    END;
    IF MSIR.Kind(MSIR.EltType(arrT)) # MSIR.TypeKind.FixedArray THEN
      Err(c, "array.elem_addr: ptr element must be fixed-array type"); RETURN;
    END;
    IF NOT IsScalar(MSIR.ValueType(MSIR.InsnOperand(i, 1))) THEN
      Err(c, "array.elem_addr: index must be a scalar type");
    END;
    IF res = NIL THEN Err(c, "array.elem_addr must have a result"); RETURN END;
    IF MSIR.Kind(MSIR.ValueType(res)) # MSIR.TypeKind.Ptr THEN
      Err(c, "array.elem_addr result must be ptr type");
    END;
  END CheckArrayElemAddr;

PROCEDURE CheckInsn(c: Ctx;  i: MSIR.Insn;  resultT: MSIR.T) =
  BEGIN
    CASE MSIR.InsnOp(i) OF
    | MSIR.Op.IAdd, MSIR.Op.ISub, MSIR.Op.IMul,
      MSIR.Op.IDiv, MSIR.Op.IMod,
      MSIR.Op.IAnd, MSIR.Op.IOr,  MSIR.Op.IXor,
      MSIR.Op.IShl, MSIR.Op.ILShr, MSIR.Op.IAShr =>
        CheckBinaryArith(c, i);
    | MSIR.Op.FAdd, MSIR.Op.FSub, MSIR.Op.FMul, MSIR.Op.FDiv =>
        CheckBinaryArith(c, i);
    | MSIR.Op.FNeg =>
        (* single float operand, same type result — treated like unary arith *)
    | MSIR.Op.ICmp =>
        CheckICmp(c, i);
    | MSIR.Op.FCmp =>
        (* float comparison: two float operands, i1 result — similar to ICmp *)
        CheckICmp(c, i);
    | MSIR.Op.Br =>
        CheckBranchTarget(c, i, 0);
    | MSIR.Op.CondBr =>
        CheckCondBr(c, i);
    | MSIR.Op.UnwindTo =>
        CheckBranchTarget(c, i, 0);
    | MSIR.Op.GcLoad =>
        CheckGcLoad(c, i);
    | MSIR.Op.GcStore =>
        CheckGcStore(c, i);
    | MSIR.Op.Call, MSIR.Op.Invoke =>
        CheckCall(c, i);
    | MSIR.Op.Dispatch =>
        CheckDispatch(c, i);
    | MSIR.Op.Typecase =>
        CheckTypecase(c, i);
    | MSIR.Op.Ret =>
        CheckRet(c, i, resultT);
    | MSIR.Op.Load =>
        CheckLoad(c, i);
    | MSIR.Op.Store =>
        CheckStore(c, i);
    | MSIR.Op.Alloca =>
        CheckAlloca(c, i);
    | MSIR.Op.AllocaDyn =>
        (* byteCount operand must be an integer; result is ptr void *)
        VAR res := MSIR.InsnResult(i);  resT: MSIR.T;
        BEGIN
          IF res = NIL THEN Err(c, "alloca.dyn must have a result"); RETURN END;
          resT := MSIR.ValueType(res);
          IF MSIR.Kind(resT) # MSIR.TypeKind.Ptr THEN
            Err(c, "alloca.dyn result must be ptr type");
          END;
        END;
    | MSIR.Op.Convert =>
        CheckConvert(c, i);
    | MSIR.Op.SIToFP, MSIR.Op.FPToSI,
      MSIR.Op.FPExt,  MSIR.Op.FPTrunc,
      MSIR.Op.ZExt,   MSIR.Op.SExt, MSIR.Op.Trunc,
      MSIR.Op.FPFloor, MSIR.Op.FPCeil, MSIR.Op.FPRound, MSIR.Op.FPAbs =>
        (* Cast/rounding ops: one operand, typed result. No further v0 checks. *)
    | MSIR.Op.FieldAddr =>
        CheckFieldAddr(c, i);
    | MSIR.Op.ArrayElemAddr =>
        CheckArrayElemAddr(c, i);
    ELSE
        (* Other ops: no v0 checks beyond what the builders enforce. *)
    END;
  END CheckInsn;

(*------------------------------------------ block-level checks *)

PROCEDURE CheckBlock(c: Ctx;  b: MSIR.Block;  resultT: MSIR.T;
                     allowEmpty: BOOLEAN) =
  VAR n := MSIR.BlockInsnCount(b);
  BEGIN
    c.blockLabel := MSIR.BlockLabel(b);
    IF n = 0 THEN
      IF NOT allowEmpty THEN
        ErrAt(c, "block " & c.blockLabel, "empty block");
      END;
      RETURN;
    END;
    (* Check terminator placement: only the last insn may be a terminator. *)
    FOR k := 0 TO n - 1 DO
      VAR i := MSIR.BlockInsn(b, k);
      BEGIN
        c.insnIndex := k;
        CheckInsn(c, i, resultT);
        IF IsTerminator(MSIR.InsnOp(i)) AND k # n - 1 THEN
          Err(c, "terminator op precedes end of block");
        END;
      END;
    END;
    (* The last insn must be a terminator (unless this is a finally block,
       which is the one place we currently allow no terminator — the
       lowering inserts one). 'allowEmpty' covers that case. *)
    IF NOT IsTerminator(MSIR.InsnOp(MSIR.BlockInsn(b, n - 1))) THEN
      IF NOT allowEmpty THEN
        c.insnIndex := n - 1;
        Err(c, "block does not end with a terminator");
      END;
    END;
  END CheckBlock;

PROCEDURE CheckEnvelope(c: Ctx;  e: MSIR.Envelope;  resultT: MSIR.T) =
  BEGIN
    IF MSIR.EnvelopeBody(e) = NIL THEN
      ErrAt(c, "envelope", "body is NIL");
      RETURN;
    END;
    CheckBlock(c, MSIR.EnvelopeBody(e), resultT, FALSE);
    IF MSIR.IsTryFinally(e) THEN
      IF MSIR.EnvelopeFinally(e) = NIL THEN
        ErrAt(c, "envelope", "try_finally has no finally block");
      ELSE
        (* Finally blocks are allowed to lack a terminator in v0. *)
        CheckBlock(c, MSIR.EnvelopeFinally(e), resultT, TRUE);
      END;
    ELSE
      IF MSIR.EnvelopeHandlerCount(e) = 0 THEN
        ErrAt(c, "envelope", "try_except has no handlers");
      END;
      FOR k := 0 TO MSIR.EnvelopeHandlerCount(e) - 1 DO
        VAR h := MSIR.EnvelopeHandler(e, k);
        BEGIN
          IF h.block = NIL THEN
            ErrAt(c, "envelope",
                  "handler " & Fmt.Int(k) & " has NIL block");
          ELSE
            CheckBlock(c, h.block, resultT, FALSE);
          END;
        END;
      END;
    END;
  END CheckEnvelope;

(*------------------------------------------ procedure / module entries *)

PROCEDURE VerifyProc(p: MSIR.Proc): REF ARRAY OF TEXT =
  VAR
    c := NEW(Ctx);
    resultT := MSIR.ProcResultType(p);
  BEGIN
    c.errors := NEW(RefSeq.T).init();
    c.procName := MSIR.ProcName(p);
    IF MSIR.ProcItemCount(p) = 0 THEN
      (* zero-block procedure = forward declaration; nothing to verify *)
    ELSE
      IF NOT MSIR.ProcItemIsBlock(p, 0) THEN
        c.errors.addhi(c.procName & ": first item must be a block");
      END;
      FOR k := 0 TO MSIR.ProcItemCount(p) - 1 DO
        IF MSIR.ProcItemIsBlock(p, k) THEN
          CheckBlock(c, NARROW(MSIR.ProcItem(p, k), MSIR.Block),
                     resultT, FALSE);
        ELSE
          CheckEnvelope(c, NARROW(MSIR.ProcItem(p, k), MSIR.Envelope),
                        resultT);
        END;
      END;
    END;
    RETURN ResultArray(c.errors);
  END VerifyProc;

PROCEDURE ResultArray(errs: RefSeq.T): REF ARRAY OF TEXT =
  VAR
    n := errs.size();
    r: REF ARRAY OF TEXT;
  BEGIN
    IF n = 0 THEN RETURN NIL END;
    r := NEW(REF ARRAY OF TEXT, n);
    FOR i := 0 TO n - 1 DO r[i] := errs.get(i) END;
    RETURN r;
  END ResultArray;

PROCEDURE VerifyModule(m: MSIR.Module): REF ARRAY OF TEXT =
  VAR
    all := NEW(RefSeq.T).init();
  BEGIN
    FOR k := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      VAR errs := VerifyProc(MSIR.ModuleProc(m, k));
      BEGIN
        IF errs # NIL THEN
          FOR j := 0 TO LAST(errs^) DO all.addhi(errs[j]) END;
        END;
      END;
    END;
    RETURN ResultArray(all);
  END VerifyModule;

BEGIN
END MSIRVerifier.
