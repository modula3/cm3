MODULE MSIRPrinter;

IMPORT MSIR, Wr, Fmt, Thread;

<*FATAL Thread.Alerted, Wr.Failure*>

PROCEDURE Type(wr: Wr.T;  t: MSIR.T) =
  BEGIN
    IF t = NIL THEN Wr.PutText(wr, "<nil-type>"); RETURN END;
    CASE MSIR.Kind(t) OF
    | MSIR.TypeKind.Void => Wr.PutText(wr, "void");
    | MSIR.TypeKind.I1   => Wr.PutText(wr, "i1");
    | MSIR.TypeKind.I8   => Wr.PutText(wr, "i8");
    | MSIR.TypeKind.I16  => Wr.PutText(wr, "i16");
    | MSIR.TypeKind.I32  => Wr.PutText(wr, "i32");
    | MSIR.TypeKind.I64  => Wr.PutText(wr, "i64");
    | MSIR.TypeKind.W8   => Wr.PutText(wr, "word8");
    | MSIR.TypeKind.W16  => Wr.PutText(wr, "word16");
    | MSIR.TypeKind.W32  => Wr.PutText(wr, "word32");
    | MSIR.TypeKind.W64  => Wr.PutText(wr, "word64");
    | MSIR.TypeKind.F32  => Wr.PutText(wr, "f32");
    | MSIR.TypeKind.F64  => Wr.PutText(wr, "f64");
    | MSIR.TypeKind.F128 => Wr.PutText(wr, "f128");
    | MSIR.TypeKind.Ptr =>
        Wr.PutText(wr, "ptr ");
        Type(wr, MSIR.EltType(t));
    | MSIR.TypeKind.GcRef =>
        Wr.PutText(wr, "gc_ref ");
        Type(wr, MSIR.EltType(t));
    | MSIR.TypeKind.GcSlot =>
        Wr.PutText(wr, "gc_slot ");
        Type(wr, MSIR.EltType(t));
    | MSIR.TypeKind.Struct =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, MSIR.StructName(t));
    | MSIR.TypeKind.Object =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, MSIR.ObjectName(t));
    | MSIR.TypeKind.OpenArray =>
        Wr.PutText(wr, "openarray<");
        Wr.PutText(wr, Fmt.Int(MSIR.OpenArrayRank(t)));
        Wr.PutText(wr, "> ");
        Type(wr, MSIR.OpenArrayElt(t));
    | MSIR.TypeKind.HeapArray =>
        Wr.PutText(wr, "heap_array<");
        Wr.PutText(wr, Fmt.Int(MSIR.HeapArrayRank(t)));
        Wr.PutText(wr, "> ");
        Type(wr, MSIR.HeapArrayElt(t));
    | MSIR.TypeKind.FixedArray =>
        Wr.PutText(wr, "[");
        Wr.PutText(wr, Fmt.LongInt(MSIR.FixedArrayLen(t)));
        Wr.PutText(wr, "]");
        Type(wr, MSIR.FixedArrayElt(t));
    | MSIR.TypeKind.Subrange =>
        Wr.PutText(wr, "subrange<");
        Type(wr, MSIR.SubrangeParent(t));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.SubrangeLo(t)));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.SubrangeHi(t)));
        Wr.PutText(wr, ">");
    | MSIR.TypeKind.Set =>
        Wr.PutText(wr, "set<");
        Type(wr, MSIR.SetElt(t));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.SetLo(t)));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.SetHi(t)));
        Wr.PutText(wr, ">");
    | MSIR.TypeKind.ProcType =>
        Wr.PutText(wr, "proc(...)");          (* v0: terse *)
    END;
  END Type;

PROCEDURE Value(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "<nil-value>"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Type(wr, MSIR.ValueType(v));
        Wr.PutText(wr, " ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "nil");
    | MSIR.ValueKind.Param,
      MSIR.ValueKind.BlockParam,
      MSIR.ValueKind.InsnResult,
      MSIR.ValueKind.GlobalRef =>
        Wr.PutText(wr, MSIR.ValueName(v));
    END;
  END Value;

(* For places where we want just the SSA name without type prefix, e.g.
   inside operand lists. *)
PROCEDURE NameRef(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "<nil-value>"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Wr.PutText(wr, Fmt.LongInt(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "nil");
    ELSE
        Wr.PutText(wr, MSIR.ValueName(v));
    END;
  END NameRef;

PROCEDURE PredText(p: MSIR.CmpPred): TEXT =
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
  END PredText;

PROCEDURE OpText(op: MSIR.Op): TEXT =
  BEGIN
    CASE op OF
    | MSIR.Op.Alloca             => RETURN "alloca";
    | MSIR.Op.Load               => RETURN "load";
    | MSIR.Op.Store              => RETURN "store";
    | MSIR.Op.GcLoad             => RETURN "gc.load";
    | MSIR.Op.GcStore            => RETURN "gc.store";
    | MSIR.Op.FieldAddr          => RETURN "field_addr";
    | MSIR.Op.IAdd               => RETURN "iadd";
    | MSIR.Op.ISub               => RETURN "isub";
    | MSIR.Op.IMul               => RETURN "imul";
    | MSIR.Op.IDiv               => RETURN "idiv";
    | MSIR.Op.IMod               => RETURN "imod";
    | MSIR.Op.ICmp               => RETURN "icmp";
    | MSIR.Op.Br                 => RETURN "br";
    | MSIR.Op.CondBr             => RETURN "cond_br";
    | MSIR.Op.Ret                => RETURN "ret";
    | MSIR.Op.Unreachable        => RETURN "unreachable";
    | MSIR.Op.UnwindTo           => RETURN "unwind_to";
    | MSIR.Op.RetThroughEnvelope => RETURN "ret_through_envelope";
    | MSIR.Op.Call               => RETURN "call";
    | MSIR.Op.Invoke             => RETURN "invoke";
    | MSIR.Op.New                => RETURN "new";
    | MSIR.Op.Dispatch           => RETURN "dispatch";
    | MSIR.Op.Narrow             => RETURN "narrow";
    | MSIR.Op.Istype             => RETURN "istype";
    | MSIR.Op.Typecase           => RETURN "typecase";
    | MSIR.Op.Raise              => RETURN "raise";
    | MSIR.Op.LandingPad         => RETURN "landingpad";
    | MSIR.Op.ExtractValue       => RETURN "extractvalue";
    | MSIR.Op.Resume             => RETURN "resume";
    | MSIR.Op.OpenArraySize      => RETURN "openarray.size";
    | MSIR.Op.OpenArrayElemAddr  => RETURN "openarray.elem_addr";
    | MSIR.Op.Subarray           => RETURN "subarray";
    | MSIR.Op.SubscriptCheck     => RETURN "subscript_check";
    | MSIR.Op.NilCheck           => RETURN "nil_check";
    | MSIR.Op.RangeCheck         => RETURN "range_check";
    | MSIR.Op.Convert            => RETURN "convert";
    | MSIR.Op.SetUnion           => RETURN "set_union";
    | MSIR.Op.SetIntersect       => RETURN "set_intersect";
    | MSIR.Op.SetDifference      => RETURN "set_difference";
    | MSIR.Op.SetMember          => RETURN "set_member";
    | MSIR.Op.OpenArrayNew       => RETURN "openarray.new";
    | MSIR.Op.OpenArrayDeref     => RETURN "openarray.deref";
    | MSIR.Op.ArrayElemAddr      => RETURN "array.elem_addr";
    END;
  END OpText;

PROCEDURE BrTargetWithArgs(wr: Wr.T;  i: MSIR.Insn;  k: INTEGER) =
  VAR n := MSIR.InsnBrArgCount(i, k);
  BEGIN
    Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, k)));
    IF n > 0 THEN
      Wr.PutText(wr, "(");
      FOR j := 0 TO n - 1 DO
        IF j > 0 THEN Wr.PutText(wr, ", ") END;
        NameRef(wr, MSIR.InsnBrArg(i, k, j));
      END;
      Wr.PutText(wr, ")");
    END;
  END BrTargetWithArgs;

PROCEDURE Insn(wr: Wr.T;  i: MSIR.Insn) =
  VAR
    op := MSIR.InsnOp(i);
    res := MSIR.InsnResult(i);
    nOps := MSIR.InsnOperandCount(i);
  BEGIN
    Wr.PutText(wr, "  ");
    IF res # NIL THEN
      Wr.PutText(wr, MSIR.ValueName(res));
      Wr.PutText(wr, " = ");
    END;
    Wr.PutText(wr, OpText(op));
    CASE op OF
    | MSIR.Op.Load =>
        Wr.PutText(wr, " ");
        Type(wr, MSIR.ValueType(res));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.Store =>
        Wr.PutText(wr, " ");
        Value(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.IAdd, MSIR.Op.ISub, MSIR.Op.IMul,
      MSIR.Op.IDiv, MSIR.Op.IMod =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.ICmp =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, PredText(MSIR.InsnCmpPred(i)));
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.Br =>
        Wr.PutText(wr, " ");
        BrTargetWithArgs(wr, i, 0);
    | MSIR.Op.CondBr =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        BrTargetWithArgs(wr, i, 0);
        Wr.PutText(wr, ", ");
        BrTargetWithArgs(wr, i, 1);
    | MSIR.Op.Ret =>
        IF nOps > 0 THEN
          Wr.PutText(wr, " ");
          NameRef(wr, MSIR.InsnOperand(i, 0));
        END;
    | MSIR.Op.Unreachable => (* nothing *)
    | MSIR.Op.Call =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, MSIR.ProcName(MSIR.InsnCallee(i)));
        Wr.PutText(wr, "(");
        FOR k := 0 TO nOps - 1 DO
          IF k > 0 THEN Wr.PutText(wr, ", ") END;
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
        Wr.PutText(wr, ")");
        IF res # NIL THEN
          Wr.PutText(wr, " -> ");
          Type(wr, MSIR.ValueType(res));
        END;
    | MSIR.Op.Alloca =>
        Wr.PutText(wr, " ");
        Type(wr, MSIR.InsnTargetType(i));
    | MSIR.Op.GcLoad =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.GcStore =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.FieldAddr =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", .");
        Wr.PutText(wr, MSIR.InsnSelector(i));
    | MSIR.Op.ArrayElemAddr =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.New =>
        Wr.PutText(wr, " ");
        Type(wr, MSIR.InsnTargetType(i));
    | MSIR.Op.Dispatch =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", .");
        Wr.PutText(wr, MSIR.InsnSelector(i));
        FOR k := 1 TO nOps - 1 DO
          Wr.PutText(wr, ", ");
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
        IF res # NIL THEN
          Wr.PutText(wr, " -> ");
          Type(wr, MSIR.ValueType(res));
        END;
    | MSIR.Op.Narrow, MSIR.Op.Istype =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        Type(wr, MSIR.InsnTargetType(i));
    | MSIR.Op.Typecase =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, " {\n");
        FOR k := 0 TO MSIR.InsnTypecaseClauseCount(i) - 1 DO
          VAR cl := MSIR.InsnTypecaseClause(i, k);
          BEGIN
            Wr.PutText(wr, "    | ");
            IF cl.isElse THEN
              Wr.PutText(wr, "ELSE");
            ELSE
              Type(wr, cl.targetType);
            END;
            Wr.PutText(wr, " => ");
            Wr.PutText(wr, MSIR.BlockLabel(cl.block));
            Wr.PutText(wr, "\n");
          END;
        END;
        Wr.PutText(wr, "  }");
    | MSIR.Op.Invoke =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, MSIR.ProcName(MSIR.InsnCallee(i)));
        Wr.PutText(wr, "(");
        FOR k := 0 TO nOps - 1 DO
          IF k > 0 THEN Wr.PutText(wr, ", ") END;
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
        Wr.PutText(wr, ")");
        IF res # NIL THEN
          Wr.PutText(wr, " -> ");
          Type(wr, MSIR.ValueType(res));
        END;
    | MSIR.Op.Raise =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, MSIR.InsnSelector(i));
        IF nOps > 0 THEN
          Wr.PutText(wr, ", ");
          NameRef(wr, MSIR.InsnOperand(i, 0));
        END;
    | MSIR.Op.UnwindTo =>
        Wr.PutText(wr, " ");
        BrTargetWithArgs(wr, i, 0);
    | MSIR.Op.RetThroughEnvelope =>
        IF nOps > 0 THEN
          Wr.PutText(wr, " ");
          NameRef(wr, MSIR.InsnOperand(i, 0));
        END;
    | MSIR.Op.OpenArraySize =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.OpenArrayElemAddr =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        FOR k := 1 TO nOps - 1 DO
          Wr.PutText(wr, ", ");
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
    | MSIR.Op.Subarray =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 2));
    | MSIR.Op.SubscriptCheck =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.NilCheck =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.RangeCheck =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 2));
    | MSIR.Op.Convert =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, " to ");
        Type(wr, MSIR.InsnTargetType(i));
    | MSIR.Op.SetUnion, MSIR.Op.SetIntersect,
      MSIR.Op.SetDifference, MSIR.Op.SetMember =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.OpenArrayNew =>
        Wr.PutText(wr, " <element=");
        Type(wr, MSIR.InsnTargetType(i));
        Wr.PutText(wr, ", rank=");
        Wr.PutText(wr, Fmt.Int(nOps));
        Wr.PutText(wr, ">");
        FOR k := 0 TO nOps - 1 DO
          Wr.PutText(wr, ", ");
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
    | MSIR.Op.OpenArrayDeref =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.LandingPad =>
        IF MSIR.InsnIsCleanup(i) THEN
          Wr.PutText(wr, " cleanup");
        ELSE
          Wr.PutText(wr, " catch _ZTI7_M3Exc");
        END;
    | MSIR.Op.ExtractValue =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.Int(MSIR.InsnExtractIdx(i)));
    | MSIR.Op.Resume =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    ELSE (* no extra operands printed for other ops *)
    END;
    Wr.PutText(wr, "\n");
  END Insn;

PROCEDURE Block(wr: Wr.T;  b: MSIR.Block) =
  VAR n := MSIR.BlockParamCount(b);
  BEGIN
    Wr.PutText(wr, MSIR.BlockLabel(b));
    IF n > 0 THEN
      Wr.PutText(wr, "(");
      FOR j := 0 TO n - 1 DO
        IF j > 0 THEN Wr.PutText(wr, ", ") END;
        VAR v := MSIR.BlockParamValue(b, j);
        BEGIN
          Wr.PutText(wr, MSIR.ValueName(v));
          Wr.PutText(wr, ": ");
          Type(wr, MSIR.ValueType(v));
        END;
      END;
      Wr.PutText(wr, ")");
    END;
    Wr.PutText(wr, ":\n");
    FOR k := 0 TO MSIR.BlockInsnCount(b) - 1 DO
      Insn(wr, MSIR.BlockInsn(b, k));
    END;
  END Block;

PROCEDURE ParamModeText(m: MSIR.ParamMode): TEXT =
  BEGIN
    CASE m OF
    | MSIR.ParamMode.ByValue   => RETURN "";
    | MSIR.ParamMode.Var       => RETURN "var ";
    | MSIR.ParamMode.Readonly  => RETURN "readonly ";
    END;
  END ParamModeText;

PROCEDURE Envelope(wr: Wr.T;  e: MSIR.Envelope) =
  BEGIN
    IF MSIR.IsTryFinally(e) THEN
      Wr.PutText(wr, "try_finally {\n");
      Block(wr, MSIR.EnvelopeBody(e));
      Wr.PutText(wr, "} finally {\n");
      Block(wr, MSIR.EnvelopeFinally(e));
      Wr.PutText(wr, "}\n");
    ELSE
      Wr.PutText(wr, "try {\n");
      Block(wr, MSIR.EnvelopeBody(e));
      Wr.PutText(wr, "} except {\n");
      FOR k := 0 TO MSIR.EnvelopeHandlerCount(e) - 1 DO
        VAR h := MSIR.EnvelopeHandler(e, k);
        BEGIN
          Wr.PutText(wr, "  | ");
          Wr.PutText(wr, h.exceptionSym);
          Wr.PutText(wr, " => ");
          Block(wr, h.block);
        END;
      END;
      Wr.PutText(wr, "}\n");
    END;
  END Envelope;

PROCEDURE CcText(cc: MSIR.CallingConvention): TEXT =
  BEGIN
    CASE cc OF
    | MSIR.CallingConvention.M3     => RETURN "m3";
    | MSIR.CallingConvention.C      => RETURN "c";
    | MSIR.CallingConvention.WinAPI => RETURN "winapi";
    END;
  END CcText;

PROCEDURE LinkageText(l: MSIR.Linkage): TEXT =
  BEGIN
    CASE l OF
    | MSIR.Linkage.External => RETURN "external";
    | MSIR.Linkage.Internal => RETURN "internal";
    END;
  END LinkageText;

PROCEDURE ProcAttrs(wr: Wr.T;  p: MSIR.Proc) =
  VAR
    nRaises := MSIR.ProcRaisesCount(p);
  BEGIN
    IF nRaises > 0 THEN
      Wr.PutText(wr, "  raises {");
      FOR i := 0 TO nRaises - 1 DO
        IF i > 0 THEN Wr.PutText(wr, ", ") END;
        Wr.PutText(wr, MSIR.ProcRaises(p, i));
      END;
      Wr.PutText(wr, "}\n");
    END;
    IF MSIR.ProcIsNoReturn(p) THEN
      Wr.PutText(wr, "  noreturn\n");
    END;
    IF MSIR.ProcGetLinkage(p) # MSIR.Linkage.External THEN
      Wr.PutText(wr, "  linkage ");
      Wr.PutText(wr, LinkageText(MSIR.ProcGetLinkage(p)));
      Wr.PutText(wr, "\n");
    END;
    IF MSIR.ProcGetCallingConvention(p) # MSIR.CallingConvention.M3 THEN
      Wr.PutText(wr, "  cc ");
      Wr.PutText(wr, CcText(MSIR.ProcGetCallingConvention(p)));
      Wr.PutText(wr, "\n");
    END;
  END ProcAttrs;

PROCEDURE Proc(wr: Wr.T;  p: MSIR.Proc) =
  VAR n := MSIR.ProcParamCount(p);
  BEGIN
    Wr.PutText(wr, "proc ");
    Wr.PutText(wr, MSIR.ProcName(p));
    Wr.PutText(wr, "(");
    FOR i := 0 TO n - 1 DO
      IF i > 0 THEN Wr.PutText(wr, ", ") END;
      VAR v    := MSIR.ProcParam(p, i);
          mode := MSIR.ProcParamMode(p, i);
          vt   := MSIR.ValueType(v);
      BEGIN
        Wr.PutText(wr, ParamModeText(mode));
        Wr.PutText(wr, MSIR.ValueName(v));
        Wr.PutText(wr, ": ");
        (* For VAR/READONLY, the param is ptr T internally;
           print the element type T since mode already implies indirection. *)
        IF (mode = MSIR.ParamMode.Var OR mode = MSIR.ParamMode.Readonly)
           AND MSIR.Kind(vt) = MSIR.TypeKind.Ptr THEN
          Type(wr, MSIR.EltType(vt));
        ELSE
          Type(wr, vt);
        END;
      END;
    END;
    Wr.PutText(wr, ") -> ");
    Type(wr, MSIR.ProcResultType(p));
    Wr.PutText(wr, "\n");
    ProcAttrs(wr, p);
    FOR k := 0 TO MSIR.ProcItemCount(p) - 1 DO
      VAR item := MSIR.ProcItem(p, k);
      BEGIN
        IF MSIR.ProcItemIsBlock(p, k) THEN
          Block(wr, NARROW(item, MSIR.Block));
        ELSE
          Envelope(wr, NARROW(item, MSIR.Envelope));
        END;
      END;
    END;
    Wr.PutText(wr, "\n");
  END Proc;

PROCEDURE Module(wr: Wr.T;  m: MSIR.Module) =
  VAR
    nImp    := MSIR.ModuleImportCount(m);
    nGlobal := MSIR.ModuleGlobalCount(m);
    nProc   := MSIR.ModuleProcCount(m);
  BEGIN
    Wr.PutText(wr, "module ");
    Wr.PutText(wr, MSIR.ModuleName(m));
    Wr.PutText(wr, "\n");
    IF nImp > 0 THEN
      Wr.PutText(wr, "  imports {");
      FOR i := 0 TO nImp - 1 DO
        IF i > 0 THEN Wr.PutText(wr, ", ") END;
        Wr.PutText(wr, MSIR.ModuleImport(m, i));
      END;
      Wr.PutText(wr, "}\n");
    END;
    Wr.PutText(wr, "\n");
    FOR i := 0 TO nGlobal - 1 DO
      VAR g := MSIR.ModuleGlobal(m, i);
      BEGIN
        Wr.PutText(wr, "global ");
        Wr.PutText(wr, MSIR.GlobalName(g));
        Wr.PutText(wr, " : ");
        IF MSIR.GlobalIsTraced(g) THEN
          Wr.PutText(wr, "gc_slot ");
        END;
        Type(wr, MSIR.GlobalType(g));
        Wr.PutText(wr, "\n");
      END;
    END;
    IF nGlobal > 0 THEN Wr.PutText(wr, "\n") END;
    FOR k := 0 TO nProc - 1 DO
      Proc(wr, MSIR.ModuleProc(m, k));
    END;
  END Module;

BEGIN
END MSIRPrinter.
