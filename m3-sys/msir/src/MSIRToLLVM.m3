MODULE MSIRToLLVM;

IMPORT MSIR, Wr, Fmt, Thread, Text, RefSeq;
<*FATAL Thread.Alerted, Wr.Failure*>

(*------------------------------------------------------ auxiliary naming *)

VAR auxN: INTEGER := 0;

PROCEDURE NewAux(): TEXT =
  BEGIN INC(auxN); RETURN "%__ll" & Fmt.Int(auxN) END NewAux;

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
        (* GlobalRef names have no @ in MSIR; add it for LLVM *)
        Wr.PutText(wr, "@");
        Wr.PutText(wr, MSIR.ValueName(v));
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

    | MSIR.Op.Load, MSIR.Op.GcLoad =>
        Wr.PutText(wr, "  " & MSIR.ValueName(res) & " = load ");
        LLType(wr, MSIR.ValueType(res));
        Wr.PutText(wr, ", ptr ");
        LLOpVal(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "\n");

    | MSIR.Op.Store, MSIR.Op.GcStore =>
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

    | MSIR.Op.Call, MSIR.Op.Invoke =>
        VAR callee := MSIR.InsnCallee(i);
        BEGIN
          Wr.PutText(wr, "  ");
          IF res # NIL THEN
            Wr.PutText(wr, MSIR.ValueName(res) & " = ");
          END;
          Wr.PutText(wr, "call ");
          LLType(wr, MSIR.ProcResultType(callee));
          Wr.PutText(wr, " @");
          Wr.PutText(wr, MSIR.ProcName(callee));
          Wr.PutText(wr, "(");
          FOR k := 0 TO nOps - 1 DO
            IF k > 0 THEN Wr.PutText(wr, ", ") END;
            LLTypedVal(wr, MSIR.InsnOperand(i, k));
          END;
          Wr.PutText(wr, ")\n");
        END;

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
      Wr.PutText(wr, MSIR.ProcName(p));
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
    Wr.PutText(wr, MSIR.ProcName(p));
    EmitParamList(wr, p);
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
    Wr.PutText(wr, MSIR.GlobalName(g));
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
    Wr.PutText(wr, MSIR.ProcName(p));
    EmitParamTypeList(wr, p);
    Wr.PutText(wr, "\n");
  END EmitDeclare;

(*------------------------------------------------------ module emission *)

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module) =
  VAR externs := NEW(RefSeq.T).init();
  BEGIN
    Wr.PutText(wr, "; ModuleID = '" & MSIR.ModuleName(m) & "'\n");
    Wr.PutText(wr, "; MSIR v0 LLVM IR prototype — not production quality\n\n");

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
