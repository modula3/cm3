MODULE MSIRPrinter;

IMPORT MSIR, Wr, Fmt, Thread, Text;

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
    | MSIR.TypeKind.IWide =>
        Wr.PutText(wr, "i");
        Wr.PutText(wr, Fmt.Int(MSIR.BitWidth(t)));
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

PROCEDURE PrintQuotedText(wr: Wr.T;  chars: TEXT;  cnt: INTEGER) =
  VAR len := ABS(cnt);
  BEGIN
    IF chars = NIL THEN Wr.PutText(wr, "\"\""); RETURN END;
    Wr.PutChar(wr, '"');
    FOR i := 0 TO len - 1 DO
      VAR c := ORD(Text.GetChar(chars, i));
      BEGIN
        IF c >= 32 AND c < 127 AND c # ORD('"') AND c # ORD('\\') THEN
          Wr.PutChar(wr, VAL(c, CHAR));
        ELSE
          Wr.PutChar(wr, '\\');
          Wr.PutText(wr, Fmt.Pad(Fmt.Unsigned(c, 16), 2, '0'));
        END;
      END;
    END;
    Wr.PutChar(wr, '"');
  END PrintQuotedText;

PROCEDURE Value(wr: Wr.T;  v: MSIR.Value) =
  BEGIN
    IF v = NIL THEN Wr.PutText(wr, "<nil-value>"); RETURN END;
    CASE MSIR.GetValueKind(v) OF
    | MSIR.ValueKind.ConstInt =>
        Type(wr, MSIR.ValueType(v));
        Wr.PutText(wr, " ");
        Wr.PutText(wr, Fmt.LongInt(MSIR.GetIntVal(v)));
    | MSIR.ValueKind.ConstFloat =>
        Type(wr, MSIR.ValueType(v));
        Wr.PutText(wr, " ");
        Wr.PutText(wr, MSIR.ValueName(v));
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "nil");
    | MSIR.ValueKind.ConstProc =>
        Wr.PutText(wr, "ptr @");
        Wr.PutText(wr, MSIR.ProcName(MSIR.GetConstProc(v)));
    | MSIR.ValueKind.ConstTextLit =>
        PrintQuotedText(wr, MSIR.GetTextLitChars(v), MSIR.GetTextLitCnt(v));
    | MSIR.ValueKind.StructFieldRef =>
        Wr.PutText(wr, MSIR.ValueName(v));  (* @Mod_M3_info *)
        Wr.PutText(wr, "+");
        Wr.PutText(wr, Fmt.Int(MSIR.GetStructFieldOffset(v)));
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
    | MSIR.ValueKind.ConstFloat =>
        Wr.PutText(wr, MSIR.ValueName(v));
    | MSIR.ValueKind.ConstNil =>
        Wr.PutText(wr, "nil");
    | MSIR.ValueKind.ConstProc =>
        Wr.PutText(wr, "@");
        Wr.PutText(wr, MSIR.ProcName(MSIR.GetConstProc(v)));
    | MSIR.ValueKind.ConstTextLit =>
        PrintQuotedText(wr, MSIR.GetTextLitChars(v), MSIR.GetTextLitCnt(v));
    | MSIR.ValueKind.StructFieldRef =>
        Wr.PutText(wr, MSIR.ValueName(v));
        Wr.PutText(wr, "+");
        Wr.PutText(wr, Fmt.Int(MSIR.GetStructFieldOffset(v)));
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

PROCEDURE OpText(op: MSIR.Op): TEXT =
  BEGIN
    CASE op OF
    | MSIR.Op.Alloca             => RETURN "alloca";
    | MSIR.Op.AllocaDyn          => RETURN "alloca.dyn";
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
    | MSIR.Op.IAnd               => RETURN "iand";
    | MSIR.Op.IOr                => RETURN "ior";
    | MSIR.Op.IXor               => RETURN "ixor";
    | MSIR.Op.IShl               => RETURN "ishl";
    | MSIR.Op.ILShr              => RETURN "ilshr";
    | MSIR.Op.IAShr              => RETURN "iashr";
    | MSIR.Op.IUDiv              => RETURN "iudiv";
    | MSIR.Op.IURem              => RETURN "iurem";
    | MSIR.Op.IRotL              => RETURN "irotl";
    | MSIR.Op.IRotR              => RETURN "irotr";
    | MSIR.Op.Select             => RETURN "select";
    | MSIR.Op.ICmp               => RETURN "icmp";
    | MSIR.Op.FAdd               => RETURN "fadd";
    | MSIR.Op.FSub               => RETURN "fsub";
    | MSIR.Op.FMul               => RETURN "fmul";
    | MSIR.Op.FDiv               => RETURN "fdiv";
    | MSIR.Op.FNeg               => RETURN "fneg";
    | MSIR.Op.FCmp               => RETURN "fcmp";
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
    | MSIR.Op.SIToFP             => RETURN "sitofp";
    | MSIR.Op.FPToSI             => RETURN "fptosi";
    | MSIR.Op.FPExt              => RETURN "fpext";
    | MSIR.Op.FPTrunc            => RETURN "fptrunc";
    | MSIR.Op.ZExt               => RETURN "zext";
    | MSIR.Op.SExt               => RETURN "sext";
    | MSIR.Op.Trunc              => RETURN "trunc";
    | MSIR.Op.FPFloor            => RETURN "fpfloor";
    | MSIR.Op.FPCeil             => RETURN "fpceil";
    | MSIR.Op.FPRound            => RETURN "fpround";
    | MSIR.Op.FPAbs              => RETURN "fpabs";
    | MSIR.Op.SetUnion           => RETURN "set_union";
    | MSIR.Op.SetIntersect       => RETURN "set_intersect";
    | MSIR.Op.SetDifference      => RETURN "set_difference";
    | MSIR.Op.SetMember          => RETURN "set_member";
    | MSIR.Op.OpenArrayNew       => RETURN "openarray.new";
    | MSIR.Op.OpenArrayDeref     => RETURN "openarray.deref";
    | MSIR.Op.ArrayElemAddr      => RETURN "array.elem_addr";
    | MSIR.Op.PtrAdd             => RETURN "ptr.add";
    | MSIR.Op.GepByte            => RETURN "gep.byte";
    | MSIR.Op.CallIndirect       => RETURN "call.indirect";
    | MSIR.Op.InvokeIndirect     => RETURN "invoke.indirect";
    | MSIR.Op.AtomicFence        => RETURN "atomic.fence";
    | MSIR.Op.AtomicLoad         => RETURN "atomic.load";
    | MSIR.Op.AtomicStore        => RETURN "atomic.store";
    | MSIR.Op.AtomicRMW          => RETURN "atomic.rmw";
    | MSIR.Op.AtomicCmpXchg      => RETURN "atomic.cmpxchg";
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
      MSIR.Op.IDiv, MSIR.Op.IMod,
      MSIR.Op.IUDiv, MSIR.Op.IURem,
      MSIR.Op.IAnd, MSIR.Op.IOr, MSIR.Op.IXor,
      MSIR.Op.IShl, MSIR.Op.ILShr, MSIR.Op.IAShr,
      MSIR.Op.IRotL, MSIR.Op.IRotR =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.Select =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 2));
    | MSIR.Op.FAdd, MSIR.Op.FSub, MSIR.Op.FMul, MSIR.Op.FDiv =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.FNeg,
      MSIR.Op.FPFloor, MSIR.Op.FPCeil, MSIR.Op.FPRound, MSIR.Op.FPAbs =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.ICmp =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, PredText(MSIR.InsnCmpPred(i)));
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.FCmp =>
        Wr.PutText(wr, " ");
        Wr.PutText(wr, FCmpPredText(MSIR.InsnFCmpPred(i)));
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
    | MSIR.Op.AllocaDyn =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.GcLoad =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.GcStore =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        IF MSIR.InsnOperandCount(i) = 3 THEN
          Wr.PutText(wr, " [container: ");
          NameRef(wr, MSIR.InsnOperand(i, 2));
          Wr.PutText(wr, "]");
        END;
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
    | MSIR.Op.SIToFP, MSIR.Op.FPToSI,
      MSIR.Op.FPExt,  MSIR.Op.FPTrunc,
      MSIR.Op.ZExt,   MSIR.Op.SExt, MSIR.Op.Trunc =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, " to ");
        Type(wr, MSIR.ValueType(MSIR.InsnResult(i)));
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
          Wr.PutText(wr, " catch _ZTI6_M3Exc");
        END;
    | MSIR.Op.ExtractValue =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        Wr.PutText(wr, Fmt.Int(MSIR.InsnExtractIdx(i)));
    | MSIR.Op.Resume =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.PtrAdd =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", " & Fmt.Int(MSIR.InsnExtractIdx(i)));
    | MSIR.Op.GepByte =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
    | MSIR.Op.CallIndirect, MSIR.Op.InvokeIndirect =>
        (* ops[0]=fn, ops[1..n-1]=args *)
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, "(");
        FOR k := 1 TO nOps - 1 DO
          IF k > 1 THEN Wr.PutText(wr, ", ") END;
          NameRef(wr, MSIR.InsnOperand(i, k));
        END;
        Wr.PutText(wr, ")");
        IF MSIR.InsnBrTarget(i, 0) # NIL THEN
          Wr.PutText(wr, " to ");
          Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, 0)));
          Wr.PutText(wr, " unwind ");
          Wr.PutText(wr, MSIR.BlockLabel(MSIR.InsnBrTarget(i, 1)));
        END;
    | MSIR.Op.AtomicFence =>
        (* no operands; ordering is implicit in the op *)
    | MSIR.Op.AtomicLoad =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
    | MSIR.Op.AtomicStore =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        IF nOps = 3 THEN
          Wr.PutText(wr, " [container=");
          NameRef(wr, MSIR.InsnOperand(i, 2));
          Wr.PutText(wr, "]");
        END;
    | MSIR.Op.AtomicRMW =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        IF nOps = 3 THEN
          Wr.PutText(wr, " [container=");
          NameRef(wr, MSIR.InsnOperand(i, 2));
          Wr.PutText(wr, "]");
        END;
    | MSIR.Op.AtomicCmpXchg =>
        Wr.PutText(wr, " ");
        NameRef(wr, MSIR.InsnOperand(i, 0));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 1));
        Wr.PutText(wr, ", ");
        NameRef(wr, MSIR.InsnOperand(i, 2));
        IF nOps = 4 THEN
          Wr.PutText(wr, " [container=");
          NameRef(wr, MSIR.InsnOperand(i, 3));
          Wr.PutText(wr, "]");
        END;
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
