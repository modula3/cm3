(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Oct 31 11:00:15 PST 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

MODULE Codex86;

IMPORT Fmt, TargetMap, M3x86Rep, M3ID, M3CG_Ops, Word, M3ObjFile, Wrx86;
IMPORT TIntN, TWordN, Target, TInt;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT ByteOffset, ByteSize, No_label;
FROM M3CG IMPORT Type, MType, Label;
FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3x86Rep IMPORT Operand, MVar, Regno, OLoc, VLoc, x86Var, x86Proc, NRegs;
FROM M3x86Rep IMPORT OperandSize, GetOperandSize, RegistersForByteOperations;
FROM M3x86Rep IMPORT RegName, SplitOperand, TypeIs64, SplitImm, OperandPart;
FROM M3x86Rep IMPORT GetTypeSize, TZero, TOne, EAX, EDX, ESP, EBP, ECX;

FROM M3ObjFile IMPORT Seg;

REVEAL T = Public BRANDED "Codex86.T" OBJECT
        parent        : M3x86Rep.U := NIL;
        obj           : M3ObjFile.T := NIL;
        debug         := FALSE;
        Err           : ErrorHandler := NIL;
        opcode        : ARRAY [0 .. NRegs] OF Operand;
        current_proc  : x86Proc;
        textsym       : INTEGER;
        tempsize      := 0;
        temparr       : REF ARRAY OF MVar;
        templimit     := 0;
        fspilltop     := 0;
        fspillhigh    := 0;
        fstackspill   : REF ARRAY OF Operand;
        fspilllimit   := 0;
        fstacksize    := 0;
        fstackloaded  := 0;
        ftop_mem      : MVar;
        labarr        : REF ARRAY OF x86Label;
        lablimit      := 0;
        next_label_id := 0;
        f_litlist     : FLiteral := NIL;
        abscall_list  : AbsCall := NIL;
        flitvar       : x86Var := NIL;
        n_tags        : INTEGER := 0;
        tags          : ARRAY [0..19] OF TEXT;
      OVERRIDES
        init := init;
        end := end;
        wrFlush := wrFlush;
        set_obj := set_obj;
        set_current_proc := set_current_proc;
        set_textsym := set_textsym;
        intCall := intCall;
        relCall := relCall;
        absCall := absCall;
        rmCall := rmCall;
        cleanretOp := cleanretOp;
        brOp := brOp;
        setccOp := setccOp;
        noargOp := noargOp;
        noargFOp := noargFOp;
        immFOp := immFOp;
        binFOp := binFOp;
        memFOp := memFOp;
        assert_fstack := assert_fstack;
        f_ensureloaded := f_ensureloaded;
        f_pushnew := f_pushnew;
        f_exitproc := f_exitproc;
        fstack_push := fstack_push;
        fstack_pop := fstack_pop;
        fstack_swap := fstack_swap;
        fstack_discard := fstack_discard;
        f_loadlit := f_loadlit;
        immOp := immOp;
        binOp := binOp;
        tableOp := tableOp;
        swapOp := swapOp;
        movOp := movOp;
        movDummyReloc := movDummyReloc;
        movImmT := movImmT;
        movImmI := movImmI;
        MOVSWOp := MOVSWOp;
        STOSWOp := STOSWOp;
        CBWOp := CBWOp;
        lock_exchange := lock_exchange;
        lock_compare_exchange := lock_compare_exchange;
        write_lock_prefix := write_lock_prefix;
        pushOp := pushOp;
        popOp := popOp;
        incOp := incOp;
        decOp := decOp;
        bitTestAndSetOp := bitTestAndSetOp;
        bitTestOp := bitTestOp;
        unOp := unOp;
        mulOp := mulOp;
        imulOp := imulOp;
        imulImm := imulImm;
        divOp := divOp;
        idivOp := idivOp;
        diffdivOp := diffdivOp;
        diffmodOp := diffmodOp;
        reserve_labels := reserve_labels;
        set_label := set_label;
        case_jump := case_jump;
        load_ind := load_ind;
        fast_load_ind := fast_load_ind;
        store_ind := store_ind;
        f_loadind := f_loadind;
        f_storeind := f_storeind;
        log_label_init := log_label_init;
        get_frame := get_frame;
        set_error_handler := set_error_handler;
      END;

TYPE FLiteral = REF RECORD
  arr: FloatBytes;
  flit_size: INTEGER;
  loc: ByteOffset;
  link: FLiteral;
END;

PROCEDURE intCall (t: T; label: Label) =
  VAR ins: Instruction;
  BEGIN
    check_label(t, label, "intCall");
    WITH lab = t.labarr[label], curs = t.obj.cursor(Seg.Text) DO

      ins.opcode := 16_E8;
      ins.dsize := 4;
      IF NOT lab.no_address THEN
        ins.disp := lab.offset - (curs + 5);
      END;

      Mn(t, "CALL");  MnLabel(t, label);
      writecode(t, ins);

      IF lab.no_address THEN
        log_unknown_label(t, label, t.obj.cursor(Seg.Text) - 4, FALSE);
      END
    END
  END intCall;

PROCEDURE relCall (t: T; rel: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    ins.opcode := 16_E8;
    ins.disp   := rel;
    ins.dsize  := 4;
    Mn(t, "CALL PC +");  MnImmInt(t, rel);
    writecode(t, ins);
  END relCall;

TYPE AbsCall = REF RECORD
  sym: INTEGER;
  loc: ByteOffset;
  link: AbsCall;
END;

PROCEDURE absCall (t: T; p: x86Proc) =
  VAR ins: Instruction;
  BEGIN
    ins.opcode  := 16_FF;
    ins.modrm   := 16_15;
    ins.mrm_present := TRUE;
    ins.dsize   := 4;
    Mn(t, "CALL");  MnProc(t, p);
    writecode(t, ins);
    t.abscall_list := NEW(AbsCall, loc := t.obj.cursor(Seg.Text) - 4,
                          sym := p.symbol, link := t.abscall_list);
  END absCall;

PROCEDURE rmCall (t: T; READONLY op: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT op.loc = OLoc.register OR op.loc = OLoc.mem *>
    ins.opcode := 16_FF;
    Mn(t, "CALL r/m32");  MnOp(t, op);
    build_modrm(t, op, t.opcode[2], ins);
    writecode(t, ins);

    IF op.loc = OLoc.mem THEN
      log_global_var(t, op.mvar, -4);
    END
  END rmCall;

PROCEDURE cleanretOp (t: T; psize: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT psize < 16_8000 *>
    Mn(t, "RET");  MnImmInt(t, psize);
    ins.opcode := 16_C2;
    ins.imm    := psize;
    ins.imsize := 2;
    writecode(t, ins);
  END cleanretOp;

PROCEDURE brOp (t: T; br: Cond; label: Label) =
  VAR ins: Instruction;
  BEGIN
    check_label(t, label, "brOp");
    WITH lab = t.labarr[label], curs = t.obj.cursor(Seg.Text) DO
      IF NOT lab.no_address THEN
        ins.disp := lab.offset - (curs + 2);
      END;

      IF ins.disp > 16_7F OR ins.disp < -16_80 OR lab.no_address AND NOT lab.short THEN
        IF lab.no_address THEN
          ins.disp := 0;
        ELSE
          ins.disp := lab.offset - (curs + 5);
        END;

        Mn(t, bropcode[br].name);  MnLabel(t, label);

        IF br # Cond.Always THEN
          DEC(ins.disp);
          ins.escape := TRUE;
          ins.opcode := bropcode[br].rel8 + 16_10;
        ELSE
          ins.opcode := 16_E9;
        END;
        ins.dsize := 4;
        writecode (t, ins);
      ELSE
        Mn(t, bropcode[br].name, " rel8");  MnLabel(t, label);
        IF br # Cond.Always THEN
          ins.opcode := bropcode[br].rel8;
        ELSE
          ins.opcode := 16_EB;
        END;
        ins.dsize  := 1;
        writecode (t, ins);
      END;

      IF lab.no_address THEN
        IF lab.short THEN
          log_unknown_label(t, label, t.obj.cursor(Seg.Text) - 1, FALSE);
        ELSE
          log_unknown_label(t, label, t.obj.cursor(Seg.Text) - 4, FALSE);
        END
      END
    END
  END brOp;

PROCEDURE setccOp (t: T; READONLY op: Operand; cond: Cond) =
  VAR ins: Instruction;
  BEGIN
    (* Be careful using registers here. setccOp only sets the lower byte.
     * Caller must zero the upper bytes, before setting the condition flags.
     *)
    <* ASSERT op.loc = OLoc.register OR
              (op.loc = OLoc.mem AND CG_Bytes[op.mvar.mvar_type] = 1) *>
    build_modrm(t, op, t.opcode[0], ins);
    ins.escape := TRUE;
    ins.opcode := condopcode[cond].opc;
    Mn(t, "SETCC ", CondName[cond]);  MnOp(t, op);
    writecode(t, ins);
    IF op.loc = OLoc.mem THEN
      log_global_var(t, op.mvar, -4);
    END
  END setccOp;

PROCEDURE prepare_stack (t: T; op: FOp; forcenomem := FALSE) =
  BEGIN
    WITH opc = fopcode[op] DO
      IF (NOT opc.takesmem) OR forcenomem THEN
        IF (opc.stackin > 0 OR opc.stackdiff # 0) AND t.ftop_inmem THEN
          fstack_loadtop(t);
        END;
        IF opc.stackdiff > 0 THEN
          fstack_ensure(t, opc.stackdiff);
        END;
        fstack_check(t, opc.stackin, "prepare_stack");
      ELSE
        IF t.ftop_inmem THEN
          IF opc.memdiff > 0 THEN
            fstack_ensure(t, opc.memdiff);
          END;
          fstack_check(t, opc.min, "prepare_stack");
        ELSE
          IF opc.stackdiff > 0 THEN
            fstack_ensure(t, opc.stackdiff);
          END;
          fstack_check(t, opc.stackin, "prepare_stack");
        END
      END
    END
  END prepare_stack;

PROCEDURE noargFOp (t: T; op: FOp) =
  VAR ins: Instruction;
  BEGIN
    prepare_stack(t, op);
    Mn(t, fopcode[op].name);
    ins.opcode  := fopcode[op].stbase;
    ins.modrm   := fopcode[op].stmodrm;
    ins.mrm_present := TRUE;
    writecode(t, ins);
    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END noargFOp;

PROCEDURE immFOp (t: T; op: FOp; im: FIm) =
  VAR ins: Instruction;
  BEGIN
    prepare_stack(t, op, TRUE);
    Mn(t, imcode[im].name, " ", FImName[im]);
    ins.opcode := imcode[im].opcode;
    writecode(t, ins);
    Mn(t, fopcode[op].name, " ST1");
    ins.opcode := fopcode[op].stbase;
    ins.modrm  := fopcode[op].stmodrm + 1;
    ins.mrm_present := TRUE;
    writecode(t, ins);
    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END immFOp;

PROCEDURE binFOp (t: T; op: FOp; st: INTEGER) =
  VAR mem, ins: Instruction;
  BEGIN
    <* ASSERT st < 8 *>
    prepare_stack(t, op);
    IF t.ftop_inmem THEN
      Mn(t, fopcode[op].name, " ST");  MnMVar(t, t.ftop_mem);
      IF t.ftop_mem.mvar_type = Type.Reel THEN
        mem.opcode := fopcode[op].m32;
      ELSE
        mem.opcode := fopcode[op].m64;
      END;
      build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem, optype := t.ftop_mem.mvar_type},
                  t.opcode[fopcode[op].memop], mem);
      writecode(t, mem);
      log_global_var(t, t.ftop_mem, -4);
      INC(t.fstacksize, fopcode[op].stackdiff);
      INC(t.fstackloaded, fopcode[op].memdiff);
      t.ftop_inmem := FALSE;
      RETURN;
    END;

    IF t.debug THEN
      Mn(t, fopcode[op].name, "P ST, ST", Fmt.Int(st));
    END;
    ins.opcode  := fopcode[op].stbase;
    ins.modrm   := fopcode[op].stmodrm + st;
    ins.mrm_present := TRUE;
    writecode(t, ins);

    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END binFOp;

PROCEDURE memFOp (t: T; op: FOp; mvar: MVar) =
  VAR ins: Instruction;
  BEGIN
    prepare_stack(t, op);

    Mn(t, fopcode[op].name, " m");  MnMVar(t, mvar);
    build_modrm(t, Operand {loc := OLoc.mem, mvar := mvar, optype := mvar.mvar_type},
                t.opcode[fopcode[op].memop], ins);
    ins.opcode := fopcode[op].m32;
    writecode(t, ins);
    log_global_var(t, mvar, -4);
    INC(t.fstacksize, fopcode[op].memdiff);
    INC(t.fstackloaded, fopcode[op].memdiff);
  END memFOp;

PROCEDURE noargOp (t: T; op: Op) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, opcode[op].name);
    ins.opcode := opcode[op].imm32;
    writecode(t, ins);
  END noargOp;

PROCEDURE testOp(t: T; READONLY a, b: Operand) =
  (* This is very limited, because the tables and build_modrm are
   * so difficult to understand.
   * This is enough for shift_double.
   *)
  VAR ins: Instruction;
  BEGIN
    <* ASSERT a.loc = OLoc.register AND a.reg[0] = ECX *>
    <* ASSERT b.loc = OLoc.imm AND TIntN.EQ(b.imm, TIntN.ThirtyTwo) *>

    Mn(t, "TEST"); MnOp(t, a); MnOp(t, b);
    ins.opcode := 16_F6;
    ins.mrm_present := TRUE;
    ins.modrm := 16_C1;
    ins.imsize := 1;
    ins.imm := 32;
    writecode(t, ins);

  END testOp;

PROCEDURE shift_double_immediate(t: T; singleOp: Op; READONLY destA: ARRAY OperandPart OF Operand; READONLY imm: TIntN.T) =
  VAR right_signed := (singleOp = Op.oSAR);
      left  := ORD(singleOp = Op.oSHL);
      right := ORD((singleOp = Op.oSHR) OR right_signed);
      doubleOp := VAL(left * ORD(Op.oSHLD) + right * ORD(Op.oSHRD), Op);
      immMinus32: TIntN.T;
  BEGIN
    IF TIntN.GE(imm, TIntN.ThirtyTwo) THEN
      IF TIntN.NE(imm, TIntN.ThirtyTwo) THEN
        EVAL TIntN.Subtract(imm, TIntN.ThirtyTwo, immMinus32);
        (* Ideally we'd do a virtual move in the register allocator. *)
        t.movOp(destA[left], destA[right]);
        t.immOp(singleOp, destA[left], immMinus32);
      ELSE
        t.movOp(destA[left], destA[right]);
      END;
      IF right_signed THEN
        t.immOp(singleOp, destA[right], TIntN.ThirtyOne);
      ELSE
        t.binOp(Op.oXOR, destA[right], destA[right]);
      END;
    ELSE
      (* left shift low into high or right shift high into low *)
      shift_double_op(t, doubleOp, destA[left], destA[right], Operand{loc := OLoc.imm, imm := imm});
      t.immOp(singleOp, destA[right], imm);
    END
  END shift_double_immediate;

PROCEDURE add_double_immediate(t: T; READONLY destA: ARRAY OperandPart OF Operand; immA: ARRAY OperandPart OF TIntN.T) =
  BEGIN
    t.immOp(Op.oADD, destA[0], immA[0]);
    t.immOp(Op.oADC, destA[1], immA[1]);
  END add_double_immediate;

PROCEDURE add_double(t: T; READONLY destA, srcA: ARRAY OperandPart OF Operand) =
  BEGIN
    t.binOp(Op.oADD, destA[0], srcA[0]);
    t.binOp(Op.oADC, destA[1], srcA[1]);
  END add_double;

PROCEDURE subtract_double_immediate(t: T; READONLY destA: ARRAY OperandPart OF Operand; immA: ARRAY OperandPart OF TIntN.T) =
  BEGIN
    t.immOp(Op.oSUB, destA[0], immA[0]);
    t.immOp(Op.oSBB, destA[1], immA[1]);
  END subtract_double_immediate;

PROCEDURE subtract_double(t: T; READONLY destA, srcA: ARRAY OperandPart OF Operand) =
  BEGIN
    t.binOp(Op.oSUB, destA[0], srcA[0]);
    t.binOp(Op.oSBB, destA[1], srcA[1]);
  END subtract_double;

PROCEDURE compare_double_immediate(t: T; READONLY destA: ARRAY OperandPart OF Operand; immA: ARRAY OperandPart OF TIntN.T) =
  VAR end_label := t.reserve_labels(1, TRUE);
  BEGIN
    t.immOp(Op.oCMP, destA[1], immA[1]);
    t.brOp(Cond.NE, end_label);
    t.immOp(Op.oCMP, destA[0], immA[0]);
    t.set_label(end_label);
  END compare_double_immediate;

PROCEDURE compare_double(t: T; READONLY destA, srcA: ARRAY OperandPart OF Operand) =
  VAR end_label := t.reserve_labels(1, TRUE);
  BEGIN
    t.binOp(Op.oCMP, destA[1], srcA[1]);
    t.brOp(Cond.NE, end_label);
    t.binOp(Op.oCMP, destA[0], srcA[0]);
    t.set_label(end_label);
  END compare_double;

PROCEDURE binOp_double(t: T; op: Op; READONLY destA, srcA: ARRAY OperandPart OF Operand) =
(* oOR, oXOR, oAND *)
  BEGIN
    t.binOp(op, destA[0], srcA[0]);
    t.binOp(op, destA[1], srcA[1]);
  END binOp_double;

PROCEDURE unOp_double(t: T; op: Op; READONLY destA: ARRAY OperandPart OF Operand) =
(* oNOT *)
  BEGIN
    t.unOp(op, destA[0]);
    t.unOp(op, destA[1]);
  END unOp_double;

PROCEDURE shift_double_ecx(t: T; singleOp: Op; READONLY destA: ARRAY OperandPart OF Operand) =
  VAR right_signed := (singleOp = Op.oSAR);
      left  := ORD(singleOp = Op.oSHL);
      right := ORD((singleOp = Op.oSHR) OR right_signed);
      doubleOp := VAL(left * ORD(Op.oSHLD) + right * ORD(Op.oSHRD), Op);
      end_label := t.reserve_labels(1, TRUE);
  BEGIN
    (* caller already put shift count in ECX *)
    shift_double_op(t, doubleOp, destA[left], destA[right], t.reg[ECX]);
    t.unOp(singleOp, destA[right]);
    testOp(t, t.reg[ECX], Operand{loc := OLoc.imm, imm := TIntN.ThirtyTwo});
    t.brOp(Cond.E, end_label);
    t.movOp(destA[left], destA[right]);
    IF right_signed THEN
      t.immOp(singleOp, destA[right], TIntN.ThirtyOne);
    ELSE
      t.binOp(Op.oXOR, destA[right], destA[right]);
    END;
    t.set_label(end_label);
  END shift_double_ecx;

PROCEDURE negate_double(t: T; READONLY destA: ARRAY OperandPart OF Operand) =
  BEGIN
    t.unOp(Op.oNEG, destA[0]);
    t.binOp(Op.oADC, destA[1], Operand {loc := OLoc.imm, imm := TZero, optype := Type.Word32});
    t.unOp(Op.oNEG, destA[1]);
  END negate_double;

PROCEDURE shift_double_op (t: T; op: Op; READONLY dest, src, shiftCount: Operand) =
  (* SHLD and SHRD are three operand instructions.
   * The source is where to shift bits from, must be a register.
   * The shift count is either [-128..127] or in CL (we use all of ECX).
   *)
  VAR ins: Instruction;
  BEGIN

    <* ASSERT NOT TypeIs64(src.optype) *>
    <* ASSERT NOT TypeIs64(dest.optype) *>
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>

    ins.escape := TRUE;

    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    <* ASSERT src.loc = OLoc.register *>
    <* ASSERT shiftCount.loc = OLoc.register OR shiftCount.loc = OLoc.imm *>
    <* ASSERT shiftCount.loc # OLoc.register OR shiftCount.reg[0] = ECX *>
    <* ASSERT shiftCount.loc # OLoc.imm OR (TIntN.GE(shiftCount.imm, TIntN.Min8) AND TIntN.LE(shiftCount.imm, TIntN.Max8)) *>

    build_modrm(t, dest, src, ins);
    ins.opcode := opcode[op].rmr;

    IF shiftCount.loc = OLoc.imm THEN
      IF TWordN.GT(shiftCount.imm, TWordN.Max8) THEN
        Err(t, "shift_double_op: shift count must fit in a byte:" & TIntN.ToDiagnosticText(shiftCount.imm));
      END;
      IF NOT TIntN.ToHostInteger(shiftCount.imm, ins.imm) THEN
        Err(t, "shift_double_op: ToHostInteger(shiftCount.imm) failed:" & TIntN.ToDiagnosticText(shiftCount.imm));
      END;
      ins.imsize := 1;
      ins.opcode := opcode[op].imm8;
    END;

    Mn(t, opcode[op].name);  MnOp(t, dest);  MnOp(t, src); MnOp(t, shiftCount);

    writecode(t, ins);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    END;
  END shift_double_op;

PROCEDURE immOp1 (t: T; op: Op; READONLY dest: Operand; READONLY imm: TIntN.T) =
  VAR ins: Instruction;
      immalt := TIntN.T{n := 4 * GetOperandSize(dest), x := imm.x};
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>

    IF NOT TIntN.ToHostInteger(imm, ins.imm) THEN
      Err(t, "immOp1: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(imm));
    END;

    IF TIntN.GE(immalt, TIntN.Min8) AND TIntN.LE(immalt, TIntN.Max8) THEN
      ins.imsize := 1;
    ELSE
      ins.imsize := 4;
    END;

    Mn(t, opcode[op].name);  MnOp(t, dest);  MnImmTInt(t, imm);

    IF dest.loc = OLoc.register AND dest.reg[0] = EAX AND ins.imsize = 4 THEN
      ins.opcode := opcode[op].Aimm32;
      writecode(t, ins);
    ELSE
      build_modrm(t, dest, t.opcode[opcode[op].immop], ins);
      IF ins.imsize = 1 THEN
        IF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.mvar_type] = 1 THEN
          ins.opcode := opcode[op].imm32 - 1;
          writecode(t, ins);
          log_global_var(t, dest.mvar, -5);
        ELSIF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.mvar_type] = 2 THEN
          ins.prefix := TRUE;
          ins.opcode := opcode[op].imm8;
          writecode(t, ins);
          log_global_var(t, dest.mvar, -5);
        ELSE
          ins.opcode := opcode[op].imm8;
        
          (* shifts by a constant 1 have a smaller encoding available *)

          IF op IN SET OF Op{Op.oSHL, Op.oSHR, Op.oSAR} AND TIntN.EQ(imm, TIntN.One) THEN
            INC(ins.opcode, 16_10);
            ins.imsize := 0;
          END;

          writecode(t, ins);
          IF dest.loc = OLoc.mem THEN
            log_global_var(t, dest.mvar, -5);
          END
        END
      ELSE
        <* ASSERT dest.loc # OLoc.mem OR CG_Bytes[dest.mvar.mvar_type] = 4 *>
        ins.opcode := opcode[op].imm32;
        writecode(t, ins);
        IF dest.loc = OLoc.mem THEN
          log_global_var(t, dest.mvar, -8);
        END
      END
    END
  END immOp1;

PROCEDURE immOp (t: T; op: Op; READONLY dest: Operand; READONLY imm: TIntN.T) =
  VAR destA: ARRAY OperandPart OF Operand;
      immA: ARRAY OperandPart OF TIntN.T;
      immSize := SplitImm(dest.optype, imm, immA);
      destSize := SplitOperand(dest, destA);
  BEGIN

    <* ASSERT immSize = destSize *>
    <* ASSERT NOT TypeIs64(destA[0].optype) *>
    <* ASSERT NOT TypeIs64(destA[destSize - 1].optype) *>

    IF (immSize = 2) AND (op IN SET OF Op{Op.oCMP, Op.oADD, Op.oSUB, Op.oSHL, Op.oSHR, Op.oSAR}) THEN
      CASE op OF
        | Op.oADD => add_double_immediate(t, destA, immA);
        | Op.oSUB => subtract_double_immediate(t, destA, immA);
        | Op.oCMP => compare_double_immediate(t, destA, immA);
        | Op.oSHL,
          Op.oSHR,
          Op.oSAR => shift_double_immediate(t, op, destA, imm);
        ELSE
            <* ASSERT FALSE *>
      END
    ELSE
      FOR i := 0 TO destSize - 1 DO
        immOp1(t, op, destA[i], immA[i]);
      END;
    END;

    <* ASSERT destA[0].stackp = destA[destSize - 1].stackp *>

  END immOp;

PROCEDURE binOp1 (t: T; op: Op; READONLY dest, src: Operand) =
  VAR ins: Instruction;
  BEGIN

    <* ASSERT NOT TypeIs64(src.optype) *>
    <* ASSERT NOT TypeIs64(dest.optype) *>
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>

    IF src.loc = OLoc.imm THEN
      immOp(t, op, dest, src.imm);
      RETURN;
    END;

    <* ASSERT NOT (op IN SET OF Op{Op.oSHLD, Op.oSHRD}) *>

    <* ASSERT src.loc = OLoc.register OR dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.mvar_type] = 4 *>
    <* ASSERT dest.loc # OLoc.mem OR CG_Bytes[dest.mvar.mvar_type] = 4 *>

    IF dest.loc = OLoc.register THEN
      build_modrm(t, src, dest, ins);
      ins.opcode := opcode[op].rrm + 1;
    ELSE
      <* ASSERT src.loc = OLoc.register AND CG_Bytes[src.mvar.mvar_type] = 4 *>
      build_modrm(t, dest, src, ins);
      ins.opcode := opcode[op].rmr + 1;
    END;

    Mn(t, opcode[op].name);  MnOp(t, dest);  MnOp(t, src);

    writecode(t, ins);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END binOp1;

PROCEDURE binOp (t: T; op: Op; READONLY dest, src: Operand) =
  VAR destA: ARRAY OperandPart OF Operand;
      srcA: ARRAY OperandPart OF Operand;
      srcSize := SplitOperand(src, srcA);
      destSize := SplitOperand(dest, destA);
  BEGIN

    <* ASSERT NOT TypeIs64(srcA[0].optype) *>
    <* ASSERT NOT TypeIs64(destA[0].optype) *>
    <* ASSERT NOT TypeIs64(srcA[srcSize - 1].optype) *>
    <* ASSERT NOT TypeIs64(destA[destSize - 1].optype) *>

    IF srcSize # destSize THEN
      Err(t, "binOp: size mismatch: destSize:" & Fmt.Int(destSize)
        & " srcSize:" & Fmt.Int(srcSize)
        & " src.optype:" & Target.TypeNames[src.optype]
        & " dest.optype:" & Target.TypeNames[dest.optype]);
    END;

    IF srcSize = 2 THEN
      CASE op OF
        | Op.oADD => add_double(t, destA, srcA);
        | Op.oSUB => subtract_double(t, destA, srcA);
        | Op.oCMP => compare_double(t, destA, srcA);
        | Op.oOR,
          Op.oXOR,
          Op.oAND => binOp_double(t, op, destA, srcA);
        ELSE
          <* ASSERT FALSE *>
      END
    ELSE
      <* ASSERT srcSize = 1 *>
      binOp1(t, op, destA[0], srcA[0]);
    END;

    <* ASSERT destA[0].stackp = destA[destSize - 1].stackp *>
    <* ASSERT srcA[0].stackp = srcA[srcSize - 1].stackp *>

  END binOp;

PROCEDURE tableOp (t: T; op: Op; READONLY dest, index: Operand; scale: INTEGER; table: MVar) =
  VAR ins: Instruction;  fully_known := FALSE;
  BEGIN
    <* ASSERT dest.loc = OLoc.register AND index.loc = OLoc.register *>

    ins.disp := table.mvar_offset;
    IF table.var.loc = VLoc.temp THEN
      <* ASSERT table.var.parent = t.current_proc *>
      INC(ins.disp, table.var.offset);
      fully_known := TRUE;
    END;

    ins.mrm_present := TRUE;
    IF (NOT fully_known) OR (ins.disp > 16_7F) OR (ins.disp < -16_80) THEN
      ins.dsize := 4;
      ins.modrm := dest.reg[0] * 8 + 4;
      IF fully_known THEN
        INC (ins.modrm, 16_80);
      END;
    ELSE
      ins.dsize := 1;
      ins.modrm := 16_40 + dest.reg[0] * 8 + 4;
    END;

    ins.sib_present := TRUE;
    CASE scale OF
    | 1 => ins.sib := 0;
    | 2 => ins.sib := 16_40;
    | 4 => ins.sib := 16_80;
    | 8 => ins.sib := 16_C0;
    ELSE
      Err(t, "tableOp called with invalid scale parameter");
    END;
    INC(ins.sib, index.reg[0] * 8);
    INC(ins.sib, 5);

    Mn(t, opcode[op].name);  MnOp(t, dest); MnMVar(t, table);
    Mn(t, "::["); MnOp(t, index); Mn(t, " *"); MnImmInt (t, scale);
    Mn(t, " +");  MnImmInt(t, ins.disp);  Mn(t, " ]");

    ins.opcode := opcode[op].rrm + 1;
    writecode(t, ins);
    log_global_var(t, table, -4);
  END tableOp;

PROCEDURE swapOp1 (t: T; READONLY dest, src: Operand) =
  VAR xchg, ins: Instruction;  otherreg: Regno;
  BEGIN
    <* ASSERT (dest.loc = OLoc.register OR dest.loc = OLoc.mem) AND
              (src.loc = OLoc.register OR src.loc = OLoc.mem) *>

    IF dest.loc = OLoc.register AND src.loc = OLoc.register AND (dest.reg[0] = EAX OR src.reg[0] = EAX) THEN
      IF dest.reg[0] = EAX THEN
        otherreg := src.reg[0];
      ELSE
        otherreg := dest.reg[0];
      END;
      Mn(t, "XCHG ");  MnOp(t, dest);   MnOp(t, src);
      xchg.opcode := 16_90 + otherreg;
      writecode (t, xchg);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      <* ASSERT src.loc = OLoc.register OR CG_Bytes[src.mvar.mvar_type] = 4 *>
      build_modrm(t, src, dest, ins);
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      <* ASSERT dest.loc = OLoc.register OR CG_Bytes[dest.mvar.mvar_type] = 4 *>
      build_modrm (t, dest, src, ins);
    END;
    Mn(t, "XCHG ");  MnOp(t, dest);  MnOp(t, src);
    ins.opcode := 16_87;
    writecode(t, ins);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END swapOp1;

PROCEDURE swapOp (t: T; READONLY dest, src: Operand) =
  VAR destA: ARRAY OperandPart OF Operand;
      srcA: ARRAY OperandPart OF Operand;
      srcSize := SplitOperand(src, srcA);
      destSize := SplitOperand(dest, destA);
  BEGIN

    IF srcSize # destSize THEN
      Err(t, "swapOp: size mismatch: destSize:" & Fmt.Int(destSize)
        & " srcSize:" & Fmt.Int(srcSize)
        & " src.optype:" & Target.TypeNames[src.optype]
        & " dest.optype:" & Target.TypeNames[dest.optype]);
    END;

    <* ASSERT NOT TypeIs64(srcA[0].optype) *>
    <* ASSERT NOT TypeIs64(destA[0].optype) *>
    <* ASSERT NOT TypeIs64(srcA[srcSize - 1].optype) *>
    <* ASSERT NOT TypeIs64(destA[destSize - 1].optype) *>

    FOR i := 0 TO destSize - 1 DO
      swapOp1(t, destA[i], srcA[i]);
    END;

    <* ASSERT destA[0].stackp = destA[destSize - 1].stackp *>
    <* ASSERT srcA[0].stackp = srcA[srcSize - 1].stackp *>

  END swapOp;

CONST
  MOVSW = Instruction { prefix := TRUE, opcode := 16_A5 };
  STOSW = Instruction { prefix := TRUE, opcode := 16_AB };
  CBW   = Instruction { prefix := TRUE, opcode := 16_98 };

PROCEDURE MOVSWOp (t: T) =
  BEGIN
    Mn(t, "MOVSW");
    writecode (t, MOVSW);
  END MOVSWOp;

PROCEDURE STOSWOp (t: T) =
  BEGIN
    Mn(t, "STOSW");
    writecode(t, STOSW);
  END STOSWOp;

PROCEDURE CBWOp (t: T) =
  BEGIN
    Mn(t, "CBW");
    writecode(t, CBW);
  END CBWOp;

PROCEDURE write_lock_prefix (t: T) =
  BEGIN
    t.obj.append(Seg.Text, 16_F0, 1); (* lock prefix *)
  END write_lock_prefix;

PROCEDURE lock_compare_exchange (t: T; READONLY dest, src: Operand; type: Type) =
  VAR opcode := 16_B1;
      src_reg := src.reg[0];
      dest_reg := dest.reg[0];
  BEGIN
    Mn(t, "LOCK CMPXCHG ");  MnOp(t, dest);  MnOp(t, src);

    <* ASSERT dest.loc = OLoc.register *> (* mem would be correct, but we have a bug *)
    (* ASSERT src.loc = OLoc.register *)

    t.write_lock_prefix();

    CASE type OF
      | Type.Int8, Type.Word8 => DEC(opcode);        
      | Type.Int16, Type.Word16 => t.obj.append(Seg.Text, 16_66, 1); (* 16 bit size prefix *)
      | Type.Int32, Type.Word32, Type.Addr => (* nothing *)
      | Type.Int64, Type.Word64 => opcode := 16_C7;
                                   src_reg := 1;
      ELSE
        Err(t, "invalid type in lock_compare_exchange");
    END;

    writecode(t, Instruction{escape := TRUE, opcode := opcode, mrm_present := TRUE, modrm := src_reg * 8 + dest_reg});

  END lock_compare_exchange;

PROCEDURE lock_exchange (t: T; READONLY dest, src: Operand; type: Type) =
  VAR opcode := 16_87;
  BEGIN
    Mn(t, "XCHG ");  MnOp(t, dest);  MnOp(t, src);

    <* ASSERT dest.loc = OLoc.register *> (* mem would be correct, but we have a bug *)
    <* ASSERT src.loc = OLoc.register *>

    (* No lock prefix needed, as long as one operand references memory, the
     * xchg instruction is special and is always locked.
     *)

    CASE type OF
      | Type.Int8, Type.Word8 => DEC(opcode);        
      | Type.Int16, Type.Word16 => t.obj.append(Seg.Text, 16_66, 1); (* 16 bit size prefix *)
      | Type.Int32, Type.Word32, Type.Addr => (* nothing *)
      ELSE
        Err(t, "invalid type in lock_exchange");
    END;

    writecode(t, Instruction{opcode := opcode, mrm_present := TRUE, modrm := src.reg[0] * 8 + dest.reg[0]});
  END lock_exchange;

PROCEDURE movOp1 (t: T; READONLY dest, src: Operand) =
  VAR ins: Instruction;  mnemonic: TEXT := NIL;
  BEGIN

    IF dest.loc = OLoc.register THEN
      t.parent.proc_reguse[dest.reg[0]] := TRUE;
    END;

    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF src.loc = OLoc.imm THEN
      movImmT(t, dest, src.imm);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND dest.reg[0] = EAX AND src.loc = OLoc.mem AND CG_Bytes[src.mvar.mvar_type] = 4 AND src.mvar.var.loc = VLoc.global THEN
      Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
      ins.opcode := 16_A1;
      ins.disp   := src.mvar.mvar_offset;
      ins.dsize  := 4;
      writecode (t, ins);
      log_global_var(t, src.mvar, -4);
      RETURN;
    END;

    IF src.loc = OLoc.register AND src.reg[0] = EAX AND dest.loc = OLoc.mem AND dest.mvar.var.loc = VLoc.global THEN
      Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
      ins.opcode := 16_A2;
      get_op_size(dest.mvar.mvar_type, ins);
      ins.disp := dest.mvar.mvar_offset;
      ins.dsize := 4;
      writecode(t, ins);
      log_global_var(t, dest.mvar, -4);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND src.loc = OLoc.mem AND CG_Bytes[src.mvar.mvar_type] < 4 THEN
      CASE src.mvar.mvar_type OF
      | Type.Word8  => ins.opcode := 16_8A;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[dest.reg[0]], t.reg[dest.reg[0]]);
      | Type.Word16 => ins.opcode := 16_8B;  ins.prefix := TRUE;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[dest.reg[0]], t.reg[dest.reg[0]]);
      | Type.Int8   => ins.opcode := 16_BE;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      | Type.Int16  => ins.opcode := 16_BF;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      ELSE
        Err(t, "Unknown type of size other than dword in movOp");
      END;
      build_modrm(t, src, dest, ins);
      Mn(t, mnemonic);  MnOp(t, dest);  MnOp(t, src);
      writecode(t, ins);
      log_global_var(t, src.mvar, -4);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      build_modrm(t, src, dest, ins);
      ins.opcode := 16_8A;
      IF src.loc # OLoc.register THEN
        get_op_size(src.mvar.mvar_type, ins);
      ELSE
        INC(ins.opcode);
      END
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      build_modrm(t, dest, src, ins);
      ins.opcode := 16_88;
      get_op_size(dest.mvar.mvar_type, ins);
    END;

    Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
    writecode(t, ins);

    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END movOp1;

PROCEDURE movOp (t: T; READONLY dest, src: Operand) =
  VAR destA: ARRAY OperandPart OF Operand;
      srcA: ARRAY OperandPart OF Operand;
      srcSize := SplitOperand(src, srcA);
      destSize := SplitOperand(dest, destA);
  BEGIN

    IF srcSize # destSize THEN
      Err(t, "movOp: size mismatch: destSize:" & Fmt.Int(destSize)
        & " srcSize:" & Fmt.Int(srcSize)
        & " src.optype:" & Target.TypeNames[src.optype]
        & " dest.optype:" & Target.TypeNames[dest.optype]);
    END;

    <* ASSERT NOT TypeIs64(srcA[0].optype) *>
    <* ASSERT NOT TypeIs64(destA[0].optype) *>
    <* ASSERT NOT TypeIs64(srcA[srcSize - 1].optype) *>
    <* ASSERT NOT TypeIs64(destA[destSize - 1].optype) *>

    FOR i := 0 TO destSize - 1 DO
      movOp1(t, destA[i], srcA[i]);
    END;

    <* ASSERT destA[0].stackp = destA[destSize - 1].stackp *>
    <* ASSERT srcA[0].stackp = srcA[srcSize - 1].stackp *>

  END movOp;

PROCEDURE movDummyReloc(t: T; READONLY dest: Operand; sym: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    Mn(t, "MOV");  MnOp(t, dest);  Mn (t, " imm32");
    ins.opcode := 16_B8 + dest.reg[0];
    ins.imm    := 0;
    ins.imsize := 4;
    writecode(t, ins);
    t.obj.relocate(t.textsym, t.obj.cursor(Seg.Text) - 4, sym);
  END movDummyReloc;

PROCEDURE movImmT (t: T; READONLY dest: Operand; imm: TIntN.T) =
  VAR ins: Instruction;
      zero, one: BOOLEAN;
      opsize: INTEGER;
  BEGIN
    IF NOT TIntN.ToHostInteger(imm, ins.imm) THEN
      Err(t, "movImmT: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(imm));
    END;
    IF dest.loc # OLoc.register THEN
      <* ASSERT dest.loc = OLoc.mem *>
      ins.opcode := 16_C6;
      get_op_size(dest.mvar.mvar_type, ins);
      build_modrm(t, dest, t.opcode[0], ins);
      Mn(t, "MOV");  MnOp(t, dest);  MnImmTInt(t, imm);
      ins.imsize := CG_Bytes[dest.mvar.mvar_type];
      writecode(t, ins);
      log_global_var(t, dest.mvar, -4 - CG_Bytes[dest.mvar.mvar_type]);
    ELSE
      (* Several cases can be size-optimized.
       * 0: xor: 2 bytes
       * 1: xor/inc: 3 bytes
       * -1: or with -1: 3 bytes
       * -128 to 127: push/pop: 3 bytes
       * For everything else: use a 5 byte move.
       *)
      opsize := GetOperandSize(dest);
      zero := TIntN.EQ(imm, TZero);
      one := (NOT zero) AND (opsize = 1) AND TIntN.EQ(imm, TOne);
      IF zero OR one THEN
        binOp(t, Op.oXOR, dest, dest);
        IF one THEN
          incOp(t, dest);
        END;
      ELSIF TWordN.EQ(imm, TIntN.T{n := 4 * GetOperandSize(dest), x := TInt.MOne}) THEN
        immOp(t, Op.oOR, dest, imm);
      ELSIF (opsize = 1) AND (dest.reg[0] # ESP) AND TIntN.GE(imm, TIntN.Min8) AND TIntN.LE(imm, TIntN.Max8) THEN
        pushOp(t, Operand {loc := OLoc.imm, imm := imm, optype := dest.optype});
        popOp(t, dest);
      ELSE
        ins.opcode := 16_B8 + dest.reg[0];
        ins.imsize := 4;
        Mn(t, "MOV");  MnOp(t, dest);  MnImmTInt(t, imm);
        writecode(t, ins);
      END;
    END;
  END movImmT;

PROCEDURE movImmI (t: T; READONLY dest: Operand; imm: INTEGER) =
  VAR immT: TIntN.T;
  BEGIN
    IF NOT TIntN.FromHostInteger(imm, BYTESIZE(imm), immT) THEN
      Err(t, "movImmI: unable to convert INTEGER to TIntN.T");
    END;
    t.movImmT(dest, immT);
  END movImmI;

PROCEDURE pushOp1 (t: T; READONLY src: Operand) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, "PUSH");  MnOp(t, src);
    CASE src.loc OF
    | OLoc.imm =>
        IF NOT TIntN.ToHostInteger(src.imm, ins.imm) THEN
          Err(t, "pushOp: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(src.imm));
        END;
        IF TIntN.GE(src.imm, TIntN.Min8) AND TIntN.LE(src.imm, TIntN.Max8) THEN
          ins.opcode := 16_6A;
          ins.imsize := 1;
        ELSE
          ins.opcode := 16_68;
          ins.imsize := 4;
        END;
        writecode(t, ins);
    | OLoc.register =>
        ins.opcode := 16_50 + src.reg[0];
        writecode(t, ins);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[src.mvar.mvar_type] = 4 *>
        build_modrm(t, src, t.opcode[6], ins);
        ins.opcode := 16_FF;
        writecode(t, ins);
        log_global_var(t, src.mvar, -4);
    ELSE
      Err(t, "Tried to push an fstack element to the integer stack");
    END
  END pushOp1;

PROCEDURE pushOp (t: T; READONLY src: Operand) =
  VAR a: ARRAY OperandPart OF Operand;
      size := SplitOperand(src, a);
  BEGIN

    <* ASSERT NOT TypeIs64(a[0].optype) *>
    <* ASSERT NOT TypeIs64(a[size - 1].optype) *>

    FOR i := size - 1 TO 0 BY -1 DO
      pushOp1(t, a[i]);
    END;
  END pushOp;

PROCEDURE popOp1 (t: T; READONLY dest: Operand) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, "POP");  MnOp(t, dest);
    CASE dest.loc OF
    | OLoc.imm =>
        Err(t, "Tried to pop into an immediate stack element");
    | OLoc.register =>
        ins.opcode := 16_58 + dest.reg[0];
        writecode(t, ins);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[dest.mvar.mvar_type] = 4 *>
        build_modrm(t, dest, t.opcode[6], ins);
        ins.opcode := 16_FF;
        writecode(t, ins);
        log_global_var(t, dest.mvar, -4);
    ELSE
      Err(t, "Tried to pop an fstack element from the integer stack");
    END
  END popOp1;

PROCEDURE popOp (t: T; READONLY dest: Operand) =
  VAR a: ARRAY OperandPart OF Operand;
      size := SplitOperand(dest, a);
  BEGIN

    <* ASSERT NOT TypeIs64(a[0].optype) *>
    <* ASSERT NOT TypeIs64(a[size - 1].optype) *>

    FOR i := 0 TO size - 1 DO
      popOp1(t, a[i]);
    END;
  END popOp;

TYPE BitOp = RECORD
  name: TEXT;
  bitIndexInImm8Opcode1: [16_80..16_FF];
  bitIndexInImm8Opcode2: [0..7];
  bitIndexInRegOpcode:   [16_80..16_FF];
END;

PROCEDURE bitOp (t: T; READONLY bits, index: Operand; op: BitOp) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, op.name);
    MnPtr(t, bits, 0, Type.Word32);
    MnOp(t, index);

    ins.escape := TRUE;

    (* Correct would be: *)
        <* ASSERT index.loc = OLoc.register OR index.loc = OLoc.imm *>
        <* ASSERT bits.loc = OLoc.register OR bits.loc = OLoc.mem *>

    (* Our caller only gives a subset that I could get to work: *)
        <* ASSERT index.loc = OLoc.register *>
        <* ASSERT bits.loc = OLoc.register *>

    IF index.loc = OLoc.imm THEN
      IF NOT TIntN.ToHostInteger(index.imm, ins.imm) THEN
        Err(t, "bitOp: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(index.imm));
      END;
      ins.opcode := op.bitIndexInImm8Opcode1;
      build_modrm(t, bits, t.opcode[op.bitIndexInImm8Opcode2], ins);
      Err(t, "bitOp: untested, non-working code reached #1");
    ELSE
      ins.opcode := op.bitIndexInRegOpcode;
      IF bits.loc = OLoc.mem THEN
        Err(t, "bitOp: untested, non-working code reached #2");
        <*NOWARN*>build_modrm(t, bits, index, ins);
      ELSE
        load_like_helper(t, index.reg[0], bits, 0, ins);
      END;
    END;
    writecode(t, ins);
    IF bits.loc = OLoc.mem THEN
      log_global_var(t, bits.mvar, -4);
    END;
  END bitOp;

PROCEDURE bitTestAndSetOp (t: T; READONLY bits, index: Operand) =
  BEGIN
    bitOp(t, bits, index, BitOp{"BTS", 16_BA, 5, 16_AB});
  END bitTestAndSetOp;

PROCEDURE bitTestOp (t: T; READONLY bits, index: Operand) =
  BEGIN
    bitOp(t, bits, index, BitOp{"BT", 16_BA, 4, 16_A3});
  END bitTestOp;

PROCEDURE incDecOp (t: T; READONLY op: Operand; isDec: [0..1]) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, ARRAY [0..1] OF TEXT{"INC", "DEC"}[isDec]); MnOp(t, op);
    <* ASSERT op.loc = OLoc.mem OR op.loc = OLoc.register *>
    IF op.loc = OLoc.register THEN
      ins.opcode := isDec * 8 + 16_40 + op.reg[0];
      writecode(t, ins);
    ELSE
      <* ASSERT op.loc = OLoc.mem AND CG_Bytes[op.mvar.mvar_type] = 4 *>
      build_modrm(t, op, t.opcode[isDec], ins);
      ins.opcode := 16_FF;
      writecode(t, ins);
      log_global_var(t, op.mvar, -4);
    END
  END incDecOp;

PROCEDURE incOp (t: T; READONLY op: Operand) =
  BEGIN
    incDecOp(t, op, 0);
  END incOp;

PROCEDURE decOp (t: T; READONLY op: Operand) =
  BEGIN
    incDecOp(t, op, 1);
  END decOp;

PROCEDURE unOp1 (t: T; op: Op; READONLY dest: Operand) =
  VAR ins: Instruction;
  BEGIN
    ins.opcode := opcode[op].imm32;
    IF dest.loc = OLoc.mem THEN
      get_op_size(dest.mvar.mvar_type, ins);
    ELSE
      <* ASSERT dest.loc = OLoc.register *>
      INC(ins.opcode);
    END;

    build_modrm(t, dest, t.opcode[opcode[op].immop], ins);

    Mn(t, opcode[op].name);  MnOp(t, dest);
    writecode(t, ins);

    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    END
  END unOp1;

PROCEDURE unOp (t: T; op: Op; READONLY dest: Operand) =
  VAR destA: ARRAY OperandPart OF Operand;
      destSize := SplitOperand(dest, destA);
  BEGIN

    IF destSize > 1 THEN
      CASE op OF
        | Op.oNOT => unOp_double(t, op, destA);
        | Op.oNEG => negate_double(t, destA);
        | Op.oSHL,
          Op.oSHR,
          Op.oSAR => shift_double_ecx(t, op, destA);
        ELSE
          <* ASSERT FALSE *>
      END
    ELSE
      <* ASSERT NOT TypeIs64(destA[0].optype) *>
      <* ASSERT NOT TypeIs64(destA[destSize - 1].optype) *>
      <* ASSERT destSize = 1 *>

      unOp1(t, op, destA[0]);
    END;
  END unOp;

PROCEDURE mulOp (t: T; READONLY src: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT src.loc = OLoc.register OR (src.loc = OLoc.mem AND
              CG_Bytes[src.mvar.mvar_type] = 4) *>
    build_modrm(t, src, t.opcode[4], ins);
    Mn(t, "MUL EAX");  MnOp(t, src);
    ins.opcode := 16_F7;
    writecode(t, ins);
    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END
  END mulOp;

PROCEDURE imulOp (t: T; READONLY dest, src: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.mvar_type] = 4 *>
    Mn(t, "IMUL");  MnOp(t, dest);  MnOp(t, src);
    IF src.loc = OLoc.imm THEN
      build_modrm(t, t.reg[dest.reg[0]], dest, ins);
      ins.opcode := 16_69;
      IF NOT TIntN.ToHostInteger(src.imm, ins.imm) THEN
        Err(t, "imulOp: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(src.imm));
      END;
      ins.imsize := 4;
      writecode(t, ins);
    ELSE
      build_modrm(t, src, dest, ins);
      ins.escape := TRUE;
      ins.opcode := 16_AF;
      writecode(t, ins);
    END;

    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4 - ins.imsize);
    END
  END imulOp;

PROCEDURE imulImm (t: T; READONLY dest, src: Operand; imm: INTEGER; imsize: CARDINAL) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.mvar_type] = 4 *>
    build_modrm(t, src, dest, ins);
    Mn(t, "IMUL");  MnOp(t, dest);  MnOp(t, src);  MnImmInt(t, imm);
    IF imsize = 1 THEN
      ins.opcode := 16_6B;
    ELSE
      ins.opcode := 16_69;
    END;
    ins.imm := imm;
    ins.imsize := imsize;
    writecode(t, ins);
    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4 - imsize);
    END
  END imulImm;

PROCEDURE divOp (t: T; READONLY divisor: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register OR (divisor.loc = OLoc.mem
              AND CG_Bytes[divisor.mvar.mvar_type] = 4) *>
    build_modrm(t, divisor, t.opcode[6], ins);
    Mn(t, "DIV EAX");  MnOp(t, divisor);
    ins.opcode := 16_F7;
    writecode(t, ins);
    IF divisor.loc = OLoc.mem THEN
      log_global_var(t, divisor.mvar, -4);
    END
  END divOp;

PROCEDURE idivOp (t: T; READONLY divisor: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register OR (divisor.loc = OLoc.mem
              AND CG_Bytes[divisor.mvar.mvar_type] = 4) *>
    build_modrm(t, divisor, t.opcode[7], ins);
    Mn(t, "IDIV EAX");  MnOp(t, divisor);
    ins.opcode := 16_F7;
    writecode(t, ins);
    IF divisor.loc = OLoc.mem THEN
      log_global_var(t, divisor.mvar, -4);
    END
  END idivOp;

PROCEDURE diffdivOp (t: T; READONLY divisor: Operand; apos: BOOLEAN) =
  VAR
    diffsignlab := reserve_labels(t, 1, TRUE);
    endlab := reserve_labels(t, 1, TRUE);
  BEGIN
    <* ASSERT divisor.loc = OLoc.register *>
    movOp(t, t.reg[EDX], t.reg[EAX]);                 (*   MOV EDX, EAX      *)
    binOp(t, Op.oXOR, t.reg[EDX], divisor);           (*   XOR EDX, divisor  *)
    brOp(t, Cond.L, diffsignlab);                     (*   JL  diffsignlab   *)
    IF apos THEN
      binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]);      (*   XOR EDX, EDX      *)
    ELSE
      noargOp(t, Op.oCDQ);                            (*   CDQ               *)
    END;
    idivOp(t, divisor);                               (*   IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);                     (*   JMP endlab        *)
    set_label(t, diffsignlab);                        (* .diffsignlab        *)
    noargOp(t, Op.oCDQ);                              (*   CDQ               *)
    idivOp(t, divisor);                               (*   IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], TZero);             (*   CMP EDX, #0       *)
    brOp(t, Cond.E, endlab);                          (*   JE  endlab        *)
    decOp(t, t.reg[EAX]);                             (*   DEC EAX           *)
    set_label(t, endlab);                             (* .endlab             *)
  END diffdivOp;

PROCEDURE diffmodOp (t: T; READONLY divisor: Operand; apos: BOOLEAN) =
  VAR
    diffsignlab := reserve_labels(t, 1, TRUE);
    endlab := reserve_labels(t, 1, TRUE);
  BEGIN
    <* ASSERT divisor.loc = OLoc.register *>

    movOp(t, t.reg[EDX], t.reg[EAX]);                 (*    MOV EDX, EAX      *)
    binOp(t, Op.oXOR, t.reg[EDX], divisor);           (*    XOR EDX, divisor  *)
    brOp(t, Cond.L, diffsignlab);                     (*    JL  diffsignlab   *)
    IF apos THEN
      binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]);      (*    XOR EDX, EDX      *)
    ELSE
      noargOp(t, Op.oCDQ);                            (*    CDQ               *)
    END;
    idivOp(t, divisor);                               (*    IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);                     (*    JMP endlab        *)
    set_label(t, diffsignlab);                        (* .diffsignlab         *)
    noargOp(t, Op.oCDQ);                              (*    CDQ               *)
    idivOp(t, divisor);                               (*    IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], TZero);             (*    CMP EDX, #0       *)
    brOp(t, Cond.E, endlab);                          (*    JE  endlab        *)
    binOp(t, Op.oADD, t.reg[EDX], divisor);           (*    ADD EDX, divisor  *)
    set_label(t, endlab);                             (* .endlab              *)
  END diffmodOp;

TYPE
  Instruction = RECORD
    escape  : BOOLEAN := FALSE;
    prefix  : BOOLEAN := FALSE;
    mrm_present : BOOLEAN := FALSE;
    sib_present : BOOLEAN := FALSE;
    opcode  : INTEGER  := 0;
    modrm   : INTEGER  := 0;
    sib     : INTEGER  := 0;
    disp    : INTEGER  := 0;
    dsize   : CARDINAL := 0;
    imm     : INTEGER  := 0;
    imsize  : CARDINAL := 0;
  END;

PROCEDURE get_op_size (type: MType;  VAR ins: Instruction) =
  BEGIN
    <* ASSERT ins.opcode # 0 *> (* add byte ptr[reg], al *)
    <* ASSERT ins.opcode # -1 *>
    CASE type OF
    | Type.Int8, Type.Word8 =>
        ins.prefix := FALSE;
    | Type.Int16, Type.Word16 =>
        INC (ins.opcode);
        ins.prefix := TRUE;
    ELSE
        INC (ins.opcode);
        ins.prefix := FALSE;
    END
  END get_op_size;

PROCEDURE build_modrm (t: T; READONLY mem, reg: Operand;  VAR ins: Instruction) =
  VAR offset: ByteOffset;
      fully_known := FALSE;
  BEGIN

    <* ASSERT ins.disp = 0 *>
    <* ASSERT ins.dsize = 0 *>

    ins.mrm_present := TRUE;

    <* ASSERT reg.loc = OLoc.register *>

    IF mem.loc = OLoc.register THEN
      ins.modrm := 16_C0 + reg.reg[0] * 8 + mem.reg[0];
      RETURN;
    END;

    <* ASSERT mem.loc = OLoc.mem *>

    <* ASSERT CG_Bytes[mem.mvar.mvar_type] # 1 OR reg.opcode OR reg.reg[0] IN RegistersForByteOperations *>

    offset := mem.mvar.mvar_offset;
    IF mem.mvar.var.loc = VLoc.temp THEN
      <* ASSERT mem.mvar.var.parent = t.current_proc *>
      INC(offset, mem.mvar.var.offset);
      fully_known := TRUE;
    END;
    ins.modrm := reg.reg[0] * 8 + EBP;
    IF (NOT fully_known) OR (offset # 0) THEN
      ins.disp := offset;
      IF (NOT fully_known) OR (offset > 16_7F) OR (offset < -16_80) THEN
        ins.dsize := 4;
        IF fully_known THEN
          INC (ins.modrm, 16_80);
        END;
      ELSE
        ins.dsize := 1;
        INC (ins.modrm, 16_40);
      END
    END;
  END build_modrm;

PROCEDURE debugcode (t: T;  READONLY ins: Instruction) =
  VAR len := 0;
  BEGIN

    IF NOT t.debug THEN
      RETURN;
    END;

    (* generate the PC label *)
    t.wr.OutC(' ');
    HexBE(t, t.obj.cursor(Seg.Text), 4);
    t.wr.OutT(": ");

    (* generate the instruction bytes *)
    IF ins.escape     THEN  Byte(t, 16_0F);       INC(len); END;
    IF ins.prefix     THEN  Byte(t, 16_66);       INC(len); END;
                            Byte(t, ins.opcode);  INC(len);
    IF ins.mrm_present THEN  Byte(t, ins.modrm);  INC(len); END;
    IF ins.sib_present THEN  Byte(t, ins.sib);    INC(len); END;
    IF ins.dsize # 0  THEN  HexLE(t, ins.disp, ins.dsize); INC(len, ins.dsize); END;
    IF ins.imsize # 0 THEN  HexLE(t, ins.imm, ins.imsize); INC(len, ins.imsize); END;

    (* finally, generate the instruction mnemonic info *)
    WHILE (len < 9) DO  t.wr.OutT("  ");  INC(len); END;
    t.wr.OutT ("  ");
    FOR i := 0 TO t.n_tags-1 DO
      t.wr.OutT (t.tags[i]);
      t.tags[i] := NIL;
    END;
    t.n_tags := 0;

    t.wr.NL();
  END debugcode;

PROCEDURE writecode (t: T; READONLY ins: Instruction) =
  BEGIN

    <* ASSERT ins.opcode # 0 *> (* add byte ptr[reg], al *)
    <* ASSERT ins.opcode # -1 *>

    IF t.debug THEN
      debugcode (t, ins);
    END;

    IF ins.escape THEN
      t.obj.append(Seg.Text, 16_0F, 1);
    END;

    IF ins.prefix THEN
      t.obj.append(Seg.Text, 16_66, 1);
    END;

    <* ASSERT ins.opcode >= 0 AND ins.opcode <= 255 *>
    t.obj.append(Seg.Text, ins.opcode, 1);

    IF ins.mrm_present THEN
      t.obj.append(Seg.Text, ins.modrm, 1);
    END;

    IF ins.sib_present THEN
      t.obj.append(Seg.Text, ins.sib, 1);
    END;

    IF ins.dsize # 0 THEN
      <* ASSERT ins.dsize = 1 OR ins.dsize = 4 *>
      t.obj.append(Seg.Text, ins.disp, ins.dsize);
    END;

    IF ins.imsize # 0 THEN
      t.obj.append(Seg.Text, ins.imm, ins.imsize);
    END;
  END writecode;

(*--------------------------------------------------------- jump routines ---*)

PROCEDURE case_jump (t: T; READONLY index: Operand; READONLY labels: ARRAY OF Label) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT index.loc = OLoc.register *>
    WITH curs = t.obj.cursor(Seg.Text) DO
      ins.opcode  := 16_FF;
      ins.modrm   := 16_24;                   ins.mrm_present := TRUE;
      ins.sib     := 16_85 + index.reg[0] * 8;ins.sib_present := TRUE;
      ins.disp    := curs + 7;                ins.dsize   := 4;
      writecode(t, ins); (* Jump to abs address indexed by register 'index'*4 *)
      t.obj.relocate(t.textsym, curs + 3, t.textsym);
      FOR i := 0 TO NUMBER(labels) - 1 DO
        check_label(t, labels[i], "case_jump");
        WITH lab = t.labarr[labels[i]] DO
          IF lab.no_address THEN
            t.obj.append(Seg.Text, 0, 4);
            log_unknown_label(t, labels[i], curs + 7 + i * 4, TRUE);
          ELSE
            t.obj.append(Seg.Text, lab.offset, 4);
            t.obj.relocate(t.textsym, curs + 7 + i * 4, t.textsym);
          END
        END
      END
    END
  END case_jump;

PROCEDURE load_ind (t: T; r: Regno; READONLY ind: Operand; offset: ByteOffset; type: MType) =
  VAR ins: Instruction;
      mnemonic := "MOV";
  BEGIN

    t.parent.proc_reguse[r] := TRUE;

    <* ASSERT ind.loc = OLoc.register *>
    ins.opcode := 16_8B;

    IF CG_Bytes[type] < 4 THEN
      CASE type OF
      | Type.Word8  => ins.opcode := 16_8A;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Word16 => ins.opcode := 16_8B;
                       ins.prefix := TRUE;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Int8   => ins.opcode := 16_BE;
                       ins.escape := TRUE;
                       mnemonic := "MOVSX";
      | Type.Int16  => ins.opcode := 16_BF;
                       ins.escape := TRUE;
                       mnemonic := "MOVSX";
      ELSE
        Err(t, "Unknown type of size other than dword in load_ind");
      END;
    END;
    Mn(t, mnemonic, " ", RegName[r]);  MnPtr(t, ind, offset, type);
    ins.mrm_present := TRUE;
    ins.modrm := r * 8 + ind.reg[0];
    IF offset # 0 THEN
      ins.disp := offset;
      IF offset > -16_81 AND offset < 16_80 THEN
        ins.dsize := 1;
        INC(ins.modrm, 16_40);
      ELSE
        INC(ins.modrm, 16_80);
        ins.dsize := 4;
      END;
    END;
    IF ind.reg[0] = ESP THEN
      ins.sib := 16_24;
      ins.sib_present := TRUE;
    END;
    writecode (t, ins);
  END load_ind;

PROCEDURE fast_load_ind (t: T; r: Regno; READONLY ind: Operand; offset: ByteOffset; size: INTEGER) =
  VAR ins: Instruction;
      type := Type.Int32;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    ins.opcode := 16_8B;
    CASE size OF
    | 1 => ins.opcode := 16_8A;
           type := Type.Int8;
    | 2 => ins.opcode := 16_8B;
           ins.prefix := TRUE;
           type := Type.Int16;
    | 4 => ins.opcode := 16_8B;
           type := Type.Int32;
    ELSE
      Err(t, "Illegal size in fast_load_ind");
    END;

    Mn(t, "MOV ", RegName[r]);  MnPtr(t, ind, offset, type);
    ins.mrm_present := TRUE;
    ins.disp := offset;
    ins.modrm := r * 8 + ind.reg[0];
    IF offset > -16_81 AND offset < 16_80 THEN
      INC(ins.modrm, 16_40);
      ins.dsize := 1;
    ELSE
      INC(ins.modrm, 16_80);
      ins.dsize := 4;
    END;
    IF ind.reg[0] = ESP THEN
      ins.sib := 16_24;
      ins.sib_present := TRUE;
    END;
    writecode (t, ins);
  END fast_load_ind;

PROCEDURE load_like_helper (t: T; r: Regno; READONLY ind: Operand; offset: ByteOffset; VAR ins: Instruction) =
(* This helps with bts, but can surely help with others.
 * It is based on load_ind, but there are similarities to others.
 *)
  BEGIN
    ins.mrm_present := TRUE;
    ins.modrm := r * 8 + ind.reg[0];
    IF offset # 0 THEN
      Err(t, "load_like_helper: untested path #1 (offset # 0)");
      ins.disp := offset; <* NOWARN *>
      IF offset > -16_81 AND offset < 16_80 THEN
        ins.dsize := 1;
        INC(ins.modrm, 16_40);
      ELSE
        INC(ins.modrm, 16_80);
        ins.dsize := 4;
      END;
    END;
    IF ind.reg[0] = ESP THEN
      Err(t, "load_like_helper: untested path #2 (reg # ESP)");
      ins.sib := 16_24; <* NOWARN *>
      ins.sib_present := TRUE;
    END;
  END load_like_helper;

PROCEDURE store_ind1 (t: T; READONLY val, ind: Operand; offset: ByteOffset; type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register AND val.loc # OLoc.mem *>

    ins.opcode := 16_88;
    IF val.loc = OLoc.imm THEN
      ins.opcode := 16_C6;
      IF NOT TIntN.ToHostInteger(val.imm, ins.imm) THEN
        Err(t, "store_ind1: unable to convert immediate to INTEGER:" & TIntN.ToDiagnosticText(val.imm));
      END;
      ins.imsize := CG_Bytes[type];
    END;

    get_op_size(type, ins);
    Mn(t, "MOV");  MnPtr(t, ind, offset, type);  MnOp(t, val);

    ins.mrm_present := TRUE;
    ins.modrm := ind.reg[0];
    IF offset # 0 THEN
      ins.disp := offset;
      IF offset >= -16_80 AND offset <= 16_7F THEN
        ins.dsize := 1;
        INC(ins.modrm, 16_40);
      ELSE
        ins.dsize := 4;
        INC(ins.modrm, 16_80);
      END;
    END;

    IF val.loc # OLoc.imm THEN
      INC(ins.modrm, val.reg[0] * 8);
    END;
    IF ind.reg[0] = ESP THEN
      ins.sib := 16_24;
      ins.sib_present := TRUE;
    END;
    writecode (t, ins);
  END store_ind1;

PROCEDURE store_ind (t: T; READONLY val, ind: Operand; offset: ByteOffset; type: MType) =
  VAR valA: ARRAY OperandPart OF Operand;
      size: OperandSize;
  BEGIN
    <* ASSERT ind.loc = OLoc.register AND val.loc # OLoc.mem *>
    <* ASSERT GetOperandSize(ind) = 1 *>

    size := SplitOperand(val, valA);
    <* ASSERT (size = 1) OR (GetOperandSize(val) = GetTypeSize(type)) *>
    IF size = 1 THEN
      store_ind1(t, val, ind, offset, type);
    ELSE
      FOR i := 0 TO size - 1 DO
        store_ind1(t, valA[i], ind, offset + i * 4, valA[i].optype);
      END;
    END;
  END store_ind;

PROCEDURE f_loadind (t: T; READONLY ind: Operand; offset: ByteOffset; type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    prepare_stack(t, FOp.fLD, TRUE);
    Mn(t, "FLD");  MnPtr(t, ind, offset, type);
    IF type = Type.Reel THEN
      ins.opcode := fopcode[FOp.fLD].m32;
    ELSE
      ins.opcode := fopcode[FOp.fLD].m64;
    END;
    ins.modrm := fopcode[FOp.fLD].memop * 8 + ind.reg[0];
    ins.mrm_present := TRUE;
    IF offset # 0 THEN
      ins.disp := offset;
      IF offset >= -16_80 AND offset <= 16_7F THEN
        ins.dsize := 1;
        INC(ins.modrm, 16_40);
      ELSE
        ins.dsize := 4;
        INC(ins.modrm, 16_80);
      END;
    END;
    IF ind.reg[0] = ESP THEN
      ins.sib := 16_24;
      ins.sib_present := TRUE;
    END;
    writecode (t, ins);
    INC(t.fstacksize);
    INC(t.fstackloaded);
  END f_loadind;

PROCEDURE f_storeind (t: T; READONLY ind: Operand; offset: ByteOffset; type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    fstack_check(t, 1, "f_storeind");
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;
    Mn(t, "FSTP");  MnPtr(t, ind, offset, type);
    IF type = Type.Reel THEN
      ins.opcode := fopcode[FOp.fSTP].m32;
    ELSE
      ins.opcode := fopcode[FOp.fSTP].m64;
    END;
    ins.modrm := fopcode[FOp.fSTP].memop * 8 + ind.reg[0];
    ins.mrm_present := TRUE;
    IF offset # 0 THEN
      ins.disp := offset;
      IF offset >= -16_80 AND offset <= 16_7F THEN
        ins.dsize := 1;
        INC (ins.modrm, 16_40);
      ELSE
        ins.dsize := 4;
        INC (ins.modrm, 16_80);
      END;
    END;
    IF ind.reg[0] = ESP THEN
      ins.sib := 16_24;
      ins.sib_present := TRUE;
    END;
    writecode (t, ins);
    DEC(t.fstacksize);
    DEC(t.fstackloaded);
  END f_storeind;

(*----------------------------------------------------------- label stuff ---*)

TYPE
  x86Label = RECORD
    offset: ByteOffset := 0;
    no_address := TRUE;
    usage: LabList := NIL;
    short := FALSE;
  END;

TYPE
  LabList = OBJECT
    seg: Seg;
    offs: INTEGER;
    abs: BOOLEAN;
    link: LabList;
  END;

PROCEDURE reserve_labels (t: T; n: INTEGER; short := FALSE): Label =
  VAR lab := t.next_label_id;
  BEGIN
    IF t.next_label_id+n >= t.lablimit THEN
      expand_labels(t);
    END;
    FOR i := lab TO lab + n - 1 DO
      t.labarr[i].no_address := TRUE;
      t.labarr[i].usage := NIL;
      t.labarr[i].short := short;
    END;
    INC(t.next_label_id, n);
    RETURN lab;
  END reserve_labels;

PROCEDURE expand_labels(t: T) =
  VAR newarr := NEW(REF ARRAY OF x86Label, t.lablimit * 2);
  BEGIN
    FOR i := 0 TO t.lablimit - 1 DO
      newarr[i] := t.labarr[i];
    END;
    t.labarr := newarr;
    t.lablimit := t.lablimit * 2;
  END expand_labels;

PROCEDURE log_unknown_label (t: T; label: Label; loc: ByteOffset; abs: BOOLEAN) =
  BEGIN
    check_label(t, label, "log_unknown_label");
    t.labarr[label].usage := NEW(LabList, seg := Seg.Text,
                             offs := loc, abs := abs,
                             link := t.labarr[label].usage);
  END log_unknown_label;

PROCEDURE log_label_init (t: T; var: x86Var; offset: ByteOffset; lab: Label) =
  BEGIN
    check_label(t, lab, "log_label_init");

    t.obj.relocate(var.symbol, offset, t.textsym);

    IF t.labarr[lab].no_address THEN
      t.labarr[lab].usage := NEW(LabList, seg := var.seg,
                                 offs := t.obj.cursor(var.seg), abs := TRUE,
                                 link := t.labarr[lab].usage);
      t.obj.append(var.seg, 0, 4);
    ELSE
      t.obj.append(var.seg, t.labarr[lab].offset, 4);
    END;
  END log_label_init;

PROCEDURE get_frame (t: T; r: Regno; target, current: x86Proc) =
  BEGIN
    IF current = target THEN
      movOp(t, t.reg[r], t.reg[EBP]);
      RETURN;
    END;

    load_ind(t, r, t.reg[EBP], -4, Type.Addr);

    current := current.parent;

    WHILE current # target DO
      load_ind(t, r, t.reg[r], -4, Type.Addr);
      current := current.parent;
    END
  END get_frame;

PROCEDURE set_label (t: T; label: Label; offset := 0) =
  BEGIN
    t.parent.debug_set_label(label);
    check_label(t, label, "set_label");
    WITH lab = t.labarr[label] DO
      IF NOT lab.no_address THEN
        Err(t, "Duplicate label definition");
      END;
      lab.offset := t.obj.cursor(Seg.Text) + offset;
      lab.no_address := FALSE;
      IF lab.usage # NIL THEN
        fill_in_label_thread(t, lab.usage, lab.offset, lab.short);
        lab.usage := NIL;
      END
    END
  END set_label;

PROCEDURE check_label(t: T; label: Label; place: TEXT) =
  BEGIN
    IF label >= t.next_label_id THEN
      Err(t, "Tried to reference unknown label in " & place);
    END
  END check_label;

PROCEDURE fill_in_label_thread (t: T; ptr: LabList; val: INTEGER; short: BOOLEAN) =
  BEGIN
    WHILE ptr # NIL DO
      IF ptr.abs THEN
        t.obj.relocate(t.textsym, ptr.offs, t.textsym);
        t.obj.patch(ptr.seg, ptr.offs, val, 4);
      ELSE
        <* ASSERT ptr.seg = Seg.Text *>

        IF short THEN
          <* ASSERT val - (ptr.offs + 1) <= 16_7F AND
                    val - (ptr.offs + 1) >= -16_80 *>
          t.obj.patch(ptr.seg, ptr.offs, val - (ptr.offs + 1), 1);
        ELSE
          t.obj.patch(ptr.seg, ptr.offs, val - (ptr.offs + 4), 4);
        END
      END;
      ptr := ptr.link;
    END;
  END fill_in_label_thread;

(*-------------------------------------------------- floating stack stuff ---*)

PROCEDURE fstack_loadtop (t: T) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT t.ftop_inmem *>
    fstack_ensure(t, 0); (* ensure will allow an extra space for the item
                            in memory, so height can be 0 not 1 *)
    Mn(t, "FLD ST");  MnMVar(t, t.ftop_mem);
    IF t.ftop_mem.mvar_type = Type.Reel THEN
      ins.opcode := fopcode[FOp.fLD].m32;
    ELSE
      ins.opcode := fopcode[FOp.fLD].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem, optype := t.ftop_mem.mvar_type},
                t.opcode[fopcode[FOp.fLD].memop], ins);
    writecode(t, ins);
    log_global_var(t, t.ftop_mem, -4);
    t.ftop_inmem := FALSE;
    INC(t.fstackloaded);
  END fstack_loadtop;

PROCEDURE assert_fstack (t: T; count: INTEGER) =
  BEGIN
    <* ASSERT t.fstacksize = count *>
  END assert_fstack;

PROCEDURE f_ensureloaded (t: T) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END
  END f_ensureloaded;

PROCEDURE f_exitproc (t: T) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    <* ASSERT t.fstacksize = 1 *>
    <* ASSERT t.fstackloaded = 1 *>

    t.fstacksize := 0;
    t.fstackloaded := 0;
  END f_exitproc;

PROCEDURE f_pushnew (t: T) =
  BEGIN
    INC(t.fstacksize);
    INC(t.fstackloaded);
  END f_pushnew;

PROCEDURE fstack_push (t: T; READONLY mvar: MVar; nomem := FALSE) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    t.ftop_inmem := TRUE;
    t.ftop_mem := mvar;
    INC(t.fstacksize);

    IF nomem THEN
      fstack_loadtop(t);
    END
  END fstack_push;

PROCEDURE fstack_pop (t: T; READONLY mvar: MVar) =
  VAR ins: Instruction;
  BEGIN
    IF t.ftop_inmem THEN
      IF mvar = t.ftop_mem THEN
        t.ftop_inmem := FALSE;
        DEC(t.fstacksize);
        RETURN;
      END;
      fstack_loadtop(t);
    END;
    Mn(t, "FSTP ST");  MnMVar(t, mvar);
    IF mvar.mvar_type = Type.Reel THEN
      ins.opcode := fopcode[FOp.fSTP].m32;
    ELSE
      ins.opcode := fopcode[FOp.fSTP].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar:= mvar, optype := t.ftop_mem.mvar_type},
                t.opcode[fopcode[FOp.fSTP].memop], ins);
    writecode(t, ins);
    log_global_var(t, mvar, -4);
    DEC(t.fstacksize);
    DEC(t.fstackloaded);
    t.ftop_inmem := FALSE;
  END fstack_pop;

PROCEDURE fstack_swap (t: T) =
  VAR ins: Instruction;
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    get_temp(t);
    get_temp(t);

    Mn(t, "FLD ST, m80real");
    build_modrm(t, t.fstackspill[t.fspilltop - 2], t.opcode[5], ins);
    ins.opcode := 16_DB;
    writecode(t, ins);

    Mn(t, "FLD ST, m80real");
    ins := Instruction{};
    build_modrm(t, t.fstackspill[t.fspilltop - 1], t.opcode[5], ins);
    ins.opcode := 16_DB;
    writecode(t, ins);

    DEC(t.fspilltop, 2);
  END fstack_swap;

PROCEDURE fstack_discard (t: T) =
  BEGIN
    fstack_check(t, 1, "fstack_discard");
    IF t.ftop_inmem THEN
      t.ftop_inmem := FALSE;
    ELSE
      binFOp(t, FOp.fFREE, 0);
      noargFOp(t, FOp.fINCSTP);
      DEC(t.fstackloaded);
    END;
    DEC(t.fstacksize);
  END fstack_discard;

PROCEDURE f_loadlit (t: T; READONLY flarr: FloatBytes; type: MType) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    t.ftop_inmem := TRUE;
    WITH mvar = t.ftop_mem DO
      mvar.var := t.flitvar;
      mvar.mvar_type := type;
      mvar.mvar_offset := 0;
    END;

    INC(t.fstacksize);

    t.f_litlist := NEW(FLiteral, arr := flarr, flit_size := CG_Bytes[type],
                       loc := 0, link := t.f_litlist);
  END f_loadlit;

PROCEDURE fstack_check (t: T; depth: INTEGER; place: TEXT) =
  BEGIN
    IF t.fstacksize < depth THEN
      Err(t, "Floating stack underflow in " & place);
    END;
    IF t.ftop_inmem THEN
      IF t.fstackloaded + 1 < depth THEN
        fstack_wipeup(t, depth-t.fstackloaded-1);
      END
    ELSE
      IF t.fstackloaded < depth THEN
        fstack_wipeup(t, depth-t.fstackloaded);
      END
    END
  END fstack_check;

PROCEDURE fstack_ensure (t: T; height: INTEGER) =
  VAR spill: INTEGER;
  BEGIN
    spill := t.fstackloaded + height - 8;
    IF t.ftop_inmem THEN
      INC(spill);
    END;
    IF spill > 0 THEN
      FOR i := 1 TO spill DO
        noargFOp(t, FOp.fDECSTP);
      END;
      FOR i := 1 TO spill DO
        get_temp(t);
      END;
      t.fstackloaded := t.fstackloaded - spill;
    END
  END fstack_ensure;

PROCEDURE fstack_wipeup(t: T; wipeup: INTEGER) =
  BEGIN
    IF wipeup + t.fstackloaded > 8 THEN
      Err(t, "Stack overflow in fstack_wipeup");
    END;
    IF wipeup > t.fspilltop THEN
      Err(t, "Not enough spilled fstack elements to replace in fstack_wipeup");
    END;
    FOR i := 1 TO wipeup DO
      retrieve_temp(t);
    END;
    FOR i := 1 TO wipeup DO
      noargFOp(t, FOp.fINCSTP);
    END;
    t.fstackloaded := t.fstackloaded + wipeup;
  END fstack_wipeup;

(*------------------------------------------------------- code writing i/o---*)

PROCEDURE MnPtr(t: T;  READONLY op: Operand;  disp: INTEGER;  type: Type) =
  BEGIN
    IF t.debug THEN
      MnOp (t, op);
      Mn (t, "^[", Fmt.Int (disp));
      IF (type # Type.Int32) AND (type # Type.Word32) THEN
        Mn (t, ":", Target.TypeNames[type]);
      END;
      Mn (t, "]");
    END;
  END MnPtr;

PROCEDURE MnOp(t: T; READONLY op: Operand) =
  BEGIN
    IF t.debug THEN
      CASE op.loc OF
        OLoc.fstack   => Mn (t, " FST");
      | OLoc.register => Mn (t, " ", RegName[op.reg[0]]);
      | OLoc.imm      => Mn (t, " $", TIntN.ToText (op.imm));
      | OLoc.mem      => MnMVar (t, op.mvar);
      END
    END
  END MnOp;

PROCEDURE MnMVar(t: T;  READONLY mvar: MVar) =
  BEGIN
    IF t.debug THEN
      MnVar (t, mvar.var);
      IF mvar.mvar_offset # 0 THEN  Mn(t, "+", Fmt.Int(mvar.mvar_offset)); END;
      IF (mvar.mvar_type # Type.Int32) AND (mvar.mvar_type # Type.Word32) THEN
        Mn (t, ":", Target.TypeNames[mvar.mvar_type]);
      END;
    END;
  END MnMVar;

PROCEDURE MnVar(t: T;  READONLY v: x86Var) =
  CONST VTag = ARRAY VLoc OF TEXT { " gv.", " tv." };
  BEGIN
    IF t.debug THEN
      IF v = NIL THEN
        Mn(t, " *");
      ELSE
        Mn (t, VTag[v.loc], Fmt.Int(v.tag));
        IF v.name # M3ID.NoID THEN
          Mn (t, "[", M3ID.ToText(v.name), "]");
        END;
      END;
    END;
  END MnVar;

PROCEDURE MnLabel(t: T;  label: Label) =
  BEGIN
    IF t.debug THEN
      IF (label = No_label) THEN
        Mn(t, " *");
      ELSE
        Mn(t, " L.", Fmt.Int (label));
      END;
    END;
  END MnLabel;

PROCEDURE MnProc(t: T;  p: x86Proc) =
  BEGIN
    IF t.debug THEN
      IF (p = NIL) THEN
        Mn(t, " *");
      ELSE
        Mn(t, " p.", Fmt.Int(p.tag));
        IF p.name # M3ID.NoID THEN
          Mn (t, "[", M3ID.ToText(p.name), "]");
        END;
      END;
    END;
  END MnProc;

PROCEDURE MnImmTInt(t: T; READONLY imm: TIntN.T) =
  BEGIN
    IF t.debug THEN
      Mn(t, " $", TIntN.ToText (imm));
    END;
  END MnImmTInt;

PROCEDURE MnImmInt(t: T;  imm: INTEGER) =
  BEGIN
    IF t.debug THEN
      Mn(t, " $", Fmt.Int (imm));
    END;
  END MnImmInt;

PROCEDURE Mn (t: T; mn1, mn2, mn3: TEXT := NIL) =
  BEGIN
    IF t.debug THEN
      IF (mn1 # NIL) AND (t.n_tags < NUMBER (t.tags)) THEN
        t.tags[t.n_tags] := mn1;  INC (t.n_tags);
      END;
      IF (mn2 # NIL) AND (t.n_tags < NUMBER (t.tags)) THEN
        t.tags[t.n_tags] := mn2;  INC (t.n_tags);
      END;
      IF (mn3 # NIL) AND (t.n_tags < NUMBER (t.tags)) THEN
        t.tags[t.n_tags] := mn3;  INC (t.n_tags);
      END;
    END;
  END Mn;

PROCEDURE HexBE (t: T; val: INTEGER; size: INTEGER) =
  BEGIN
    FOR i := size - 1 TO 0 BY -1 DO
      Byte(t, Word.Extract(val, i * 8, 8));
    END;
  END HexBE;

PROCEDURE HexLE (t: T; val: INTEGER; size: INTEGER) =
  BEGIN
    FOR i := 0 TO size - 1 DO
      Byte(t, Word.Extract(val, i * 8, 8));
    END;
  END HexLE;

PROCEDURE Byte (t: T; val: INTEGER) =
  CONST Digits = ARRAY [0 .. 15] OF CHAR
      {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A',
       'B', 'C', 'D', 'E', 'F'};
  BEGIN
    t.wr.OutC(Digits[Word.Extract(val, 4, 4)]);
    t.wr.OutC(Digits[Word.And(val,16_f)]);
  END Byte;

(*--------------------------------------------------- temporary var stuff ---*)

PROCEDURE get_temp (t: T) =
  VAR ins: Instruction;
  BEGIN
    IF t.fspilltop = t.fspilllimit THEN
      expand_spill(t);
    END;
    WITH spill = t.fstackspill[t.fspilltop] DO
      IF t.fspilltop = t.fspillhigh THEN
        spill.loc := OLoc.mem;
        spill.mvar.var := t.parent.declare_temp(10, 4, Type.Void, FALSE);
        INC (t.fspillhigh);
      END;
      Mn(t, "FSTP ST, m80real");
      build_modrm(t, spill, t.opcode[7], ins);
      ins.opcode := 16_DB;
      writecode(t, ins);
    END;
    INC(t.fspilltop);
  END get_temp;

PROCEDURE retrieve_temp (t: T) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT t.fspilltop > 0 *>
    DEC(t.fspilltop);
    WITH spill = t.fstackspill[t.fspilltop] DO
      Mn(t, "FLD ST, m80real");
      build_modrm(t, spill, t.opcode[5], ins);
      ins.opcode := 16_DB;
      writecode(t, ins);
    END;
  END retrieve_temp;

PROCEDURE expand_spill (t: T) =
  VAR newspill := NEW(REF ARRAY OF Operand, t.fspilllimit * 2);
  BEGIN
    FOR i := 0 TO t.fspilllimit DO
      newspill[i] := t.fstackspill[i];
    END;
    t.fstackspill := newspill;
    t.fspilllimit := t.fspilllimit * 2;
  END expand_spill;

(*---------------------------------------------- future update list stuff ---*)

PROCEDURE log_global_var (t: T; mvar: MVar; reltocurs: INTEGER) =
  VAR patch_loc: INTEGER;
  BEGIN
    IF mvar.var.loc # VLoc.global THEN
      RETURN;
    END;

    patch_loc := t.obj.cursor(Seg.Text) + reltocurs;
    IF mvar.var = t.flitvar THEN
      <* ASSERT t.f_litlist # NIL AND t.f_litlist.loc = 0 *>
      <* ASSERT t.f_litlist.flit_size = CG_Bytes[mvar.mvar_type] *>
      <* ASSERT mvar.mvar_type = Type.Reel OR mvar.mvar_type = Type.LReel OR mvar.mvar_type = Type.XReel *>
      t.f_litlist.loc := patch_loc;
    ELSE
      t.obj.patch(Seg.Text, patch_loc, mvar.mvar_offset + mvar.var.offset, 4);
      t.obj.relocate(t.textsym, patch_loc, mvar.var.symbol);
    END
  END log_global_var;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE set_error_handler (t: T; err: ErrorHandler) =
  BEGIN
    t.Err := err;
  END set_error_handler;

(*---------------------------------------------------------------------------*)

PROCEDURE init (t: T) =
  BEGIN
    t.tempsize := 0;

    t.fspilltop := 0;

    t.fstacksize := 0;
    t.fstackloaded := 0;

    t.ftop_inmem := FALSE;

    t.next_label_id := 0;

    t.f_litlist := NIL;
    t.abscall_list := NIL;

    t.flitvar := t.parent.NewVar(Type.Struct, 0, 0, 4);
    t.flitvar.loc := VLoc.global;

    t.current_proc := NIL;

    t.textsym := 0;
  END init;

PROCEDURE end (t: T) =
  BEGIN
    tidy_internals(t);
  END end;

TYPE LocList = REF RECORD
  loc: ByteOffset;
  link: LocList;
END;

PROCEDURE find_flit (<*UNUSED*> t: T;  READONLY flarr: FloatBytes;
                     size: INTEGER;
                     used: FLiteral; VAR loc: ByteOffset): BOOLEAN =
  VAR i: CARDINAL;
  BEGIN
    WHILE used # NIL DO
      i := 0;
      WHILE (i < size) AND (flarr[i] = used.arr[i]) DO INC (i); END;
      IF (i = size) THEN
        loc := used.loc;
        RETURN TRUE;
      END;

      used := used.link;
    END;

    RETURN FALSE;
  END find_flit;

PROCEDURE find_abscall (<*UNUSED *> t: T; internal: INTEGER;
                        used: AbsCall; VAR loc: ByteOffset): BOOLEAN =
  BEGIN
    WHILE used # NIL DO
      IF internal = used.sym THEN
        loc := used.loc;
        RETURN TRUE;
      END;

      used := used.link;
    END;

    RETURN FALSE;
  END find_abscall;

PROCEDURE tidy_internals (t: T) =
  VAR internal_size := 0;
      fl_used: FLiteral;
      abscall_used: AbsCall;
      fl_locs, abscall_locs: LocList;
      intvar: x86Var;
      flptr := t.f_litlist;
      abscallptr := t.abscall_list;
  BEGIN
    fl_used := log_flit_use(t, internal_size, fl_locs);
    abscall_used := log_abscall_use(t, internal_size, abscall_locs);

    IF internal_size # 0 THEN
      intvar := init_intvar(t, internal_size, fl_used, abscall_used);

      WHILE flptr # NIL DO
        t.obj.patch(Seg.Text, flptr.loc, fl_locs.loc, 4);
        t.obj.relocate(t.textsym, flptr.loc, intvar.symbol);
        fl_locs := fl_locs.link;
        flptr := flptr.link;
      END;

      <* ASSERT fl_locs = NIL *>

      WHILE abscallptr # NIL DO
        t.obj.patch(Seg.Text, abscallptr.loc, abscall_locs.loc, 4);
        t.obj.relocate(t.textsym, abscallptr.loc, intvar.symbol);
        abscall_locs := abscall_locs.link;
        abscallptr := abscallptr.link;
      END;

      <* ASSERT abscall_locs = NIL *>
    END
  END tidy_internals;

PROCEDURE log_flit_use (t: T; VAR internal_size: INTEGER; VAR flloc: LocList):
            FLiteral =
  VAR flptr := t.f_litlist;
      f_lit, f_littail: FLiteral := NIL;
      flloctail: LocList := NIL;
      f_litloc: ByteOffset;
  BEGIN
    WHILE flptr # NIL DO
      IF NOT find_flit(t, flptr.arr, flptr.flit_size, f_lit, f_litloc) THEN
        f_litloc := internal_size;
        IF f_littail = NIL THEN
          f_littail := NEW(FLiteral, arr := flptr.arr, flit_size := flptr.flit_size,
                           loc := f_litloc, link := NIL);
          f_lit := f_littail;
        ELSE
          f_littail.link := NEW(FLiteral, arr := flptr.arr, flit_size := flptr.flit_size,
                                loc := f_litloc, link := NIL);
          f_littail := f_littail.link;
        END;

        INC(internal_size, flptr.flit_size);
      END;

      IF flloctail = NIL THEN
        flloctail := NEW(LocList, loc := f_litloc, link := NIL);
        flloc := flloctail;
      ELSE
        flloctail.link := NEW(LocList, loc := f_litloc, link := NIL);
        flloctail := flloctail.link;
      END;

      flptr := flptr.link;
    END;

    RETURN f_lit;
  END log_flit_use;

PROCEDURE log_abscall_use (t: T; VAR internal_size: INTEGER;
                           VAR abscallloc: LocList):
            AbsCall =
  VAR abscallptr := t.abscall_list;
      abscall, abscalltail: AbsCall := NIL;
      absloctail: LocList := NIL;
      abcloc: ByteOffset;
  BEGIN
    WHILE abscallptr # NIL DO
      IF NOT find_abscall(t, abscallptr.sym, abscall, abcloc) THEN
        abcloc := internal_size;
        IF abscalltail = NIL THEN
          abscalltail := NEW(AbsCall, sym := abscallptr.sym,
                             loc := abcloc, link := NIL);
          abscall := abscalltail;
        ELSE
          abscalltail.link := NEW(AbsCall, sym := abscallptr.sym,
                                loc := abcloc, link := NIL);
          abscalltail := abscalltail.link;
        END;

        INC(internal_size, 4);
      END;

      IF absloctail = NIL THEN
        absloctail := NEW(LocList, loc := abcloc, link := NIL);
        abscallloc := absloctail;
      ELSE
        absloctail.link := NEW(LocList, loc := abcloc, link := NIL);
        absloctail := absloctail.link;
      END;

      abscallptr := abscallptr.link;
    END;

    RETURN abscall;
  END log_abscall_use;

PROCEDURE init_intvar (t: T; size: ByteSize; f_lit: FLiteral; abscall: AbsCall):
            x86Var =
  VAR intvar: x86Var;
      tint: Target.Int;
  BEGIN
    intvar := t.parent.declare_global(M3ID.NoID, size, 4,
                                      Type.Struct, 0, FALSE, TRUE);
    t.parent.begin_init(intvar);

    WHILE f_lit # NIL DO
      FOR i := 0 TO f_lit.flit_size - 1 DO
        EVAL TInt.FromInt(f_lit.arr[i], tint);
        t.parent.init_int(f_lit.loc + i, tint, Type.Word8);
      END;

      f_lit := f_lit.link;
    END;

    WHILE abscall # NIL DO
      t.parent.init_int(abscall.loc, TInt.Zero, Type.Int32);
      t.obj.relocate(intvar.symbol, abscall.loc, abscall.sym);
      abscall := abscall.link;
    END;

    t.parent.end_init(intvar);
    RETURN intvar;
  END init_intvar;

PROCEDURE set_current_proc (t: T; p: x86Proc) =
  BEGIN
    t.current_proc := p;

    <* ASSERT t.fspilltop = 0 *>
    t.fspillhigh := 0;
  END set_current_proc;

PROCEDURE set_textsym (t: T; sym: INTEGER) =
  BEGIN
    t.textsym := sym;
  END set_textsym;

PROCEDURE set_obj (t: T; obj: M3ObjFile.T) =
  BEGIN
    t.obj := obj;
  END set_obj;

PROCEDURE wrFlush (t: T) =
  BEGIN
    IF t.debug THEN
      t.wr.Flush();
    END
  END wrFlush;

PROCEDURE New (parent: M3x86Rep.U; wr: Wrx86.T): T =
  VAR code := NEW(T, parent := parent, wr := wr);
  BEGIN
    IF wr # NIL THEN
      code.debug := TRUE;
    END;

    code.templimit := 32;
    code.temparr := NEW(REF ARRAY OF MVar, code.templimit);
    code.fspilllimit := 16;
    code.fstackspill := NEW(REF ARRAY OF Operand, code.fspilllimit);
    code.lablimit := 256;
    code.labarr := NEW(REF ARRAY OF x86Label, code.lablimit);

    FOR i := 0 TO NRegs DO
      code.reg[i].loc := OLoc.register;
      code.reg[i].reg[0] := i;
      code.reg[i].opcode := FALSE;
      code.opcode[i].loc := OLoc.register;
      code.opcode[i].reg[0] := i;
      code.opcode[i].opcode := TRUE;
    END;

    RETURN code;
  END New;

PROCEDURE Err(t: T; err: TEXT) =
  BEGIN
    t.Err(err);
    <* ASSERT FALSE *>
  END Err;

BEGIN
END Codex86.
