(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Oct 31 11:00:15 PST 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

MODULE Codex86;

IMPORT Fmt, TargetMap, M3x86Rep, M3ID, M3CG_Ops, Word, M3ObjFile, Wrx86, Target;
IMPORT TInt AS TargetInt;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT ByteOffset, ByteSize, No_label;
FROM M3CG IMPORT Type, MType, Label, Alignment;
FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3x86Rep IMPORT Operand, MVar, Regno, OLoc, VLoc, x86Var, x86Proc, NRegs;
FROM M3x86Rep IMPORT RegSet, RegName;

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
        movImm := movImm;
        MOVSWOp := MOVSWOp;
        STOSWOp := STOSWOp;
        CBWOp := CBWOp;
        pushOp := pushOp;
        popOp := popOp;
        decOp := decOp;
        unOp := unOp;
        mulOp := mulOp;
        imulOp := imulOp;
        imulImm := imulImm;
        divOp := divOp;
        idivOp := idivOp;
        diffdivOp := diffdivOp;
        diffmodOp := diffmodOp;
        must_extend := must_extend;
        get_addsize := get_addsize;
        aligned := aligned;
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
  size: INTEGER;
  loc: ByteOffset;
  link: FLiteral;
END;

PROCEDURE intCall (t: T; l: Label) =
  VAR ins: Instruction;
  BEGIN
    check_label(t, l, "intCall");
    WITH lab = t.labarr[l], curs = t.obj.cursor(Seg.Text) DO

      ins.opcode := 16_E8;
      ins.dsize := 4;
      IF NOT lab.no_address THEN
        ins.disp := lab.offset - (curs + 5);
      END;

      Mn(t, "CALL");  MnLabel(t, l);
      writecode(t, ins);

      IF lab.no_address THEN
        log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 4, FALSE);
      END
    END
  END intCall;

PROCEDURE relCall (t: T; rel: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    ins.opcode := 16_E8;
    ins.disp   := rel;
    ins.dsize  := 4;
    Mn(t, "CALL PC +");  MnImm(t, rel);
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
    ins.mrmpres := TRUE;
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
    Mn(t, "RET");  MnImm(t, psize);
    ins.opcode := 16_C2;
    ins.imm    := psize;
    ins.imsize := 2;
    writecode(t, ins);
  END cleanretOp;

PROCEDURE brOp (t: T; br: Cond; l: Label) =
  VAR ins: Instruction;
  BEGIN
    check_label(t, l, "brOp");
    WITH lab = t.labarr[l], curs = t.obj.cursor(Seg.Text) DO
      IF NOT lab.no_address THEN
        ins.disp := lab.offset - (curs + 2);
      END;

      IF ins.disp > 16_7F OR ins.disp < -16_80
        OR lab.no_address AND NOT lab.short THEN
        IF lab.no_address
          THEN ins.disp := 0;
          ELSE ins.disp := lab.offset - (curs + 5);
        END;

        Mn(t, bropcode[br].name);  MnLabel(t, l);

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
        Mn(t, bropcode[br].name, " rel8");  MnLabel(t, l);
        IF br # Cond.Always
          THEN ins.opcode := bropcode[br].rel8;
          ELSE ins.opcode := 16_EB;
        END;
        ins.dsize  := 1;
        writecode (t, ins);
      END;

      IF lab.no_address THEN
        IF lab.short THEN
          log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 1, FALSE);
        ELSE
          log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 4, FALSE);
        END
      END
    END
  END brOp;

PROCEDURE setccOp (t: T; READONLY op: Operand; cond: Cond) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT (op.loc = OLoc.register AND
               op.reg IN RegSet { EAX, EBX, ECX, EDX } ) OR
              (op.loc = OLoc.mem AND CG_Bytes[op.mvar.t] = 1) *>
    IF op.loc = OLoc.register THEN
      movImm(t, op, 0);
    END;
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
    ins.mrmpres := TRUE;
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
    ins.modrm  := fopcode[op].stmodrm+1;
    ins.mrmpres := TRUE;
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
      IF t.ftop_mem.t = Type.Reel
        THEN mem.opcode := fopcode[op].m32;
        ELSE mem.opcode := fopcode[op].m64;
      END;
      build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem},
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
    ins.mrmpres := TRUE;
    writecode(t, ins);

    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END binFOp;

PROCEDURE memFOp (t: T; op: FOp; mvar: MVar) =
  VAR ins: Instruction;
  BEGIN
    prepare_stack(t, op);

    Mn(t, fopcode[op].name, " m");  MnMVar(t, mvar);
    build_modrm(t, Operand {loc := OLoc.mem, mvar := mvar},
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

PROCEDURE immOp (t: T; op: Op; READONLY dest: Operand; imm: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    ins.imm := imm;
    IF imm < 16_80 AND imm > -16_81
      THEN ins.imsize := 1;
      ELSE ins.imsize := 4;
    END;

    Mn(t, opcode[op].name);  MnOp(t, dest);  MnImm(t, imm);

    IF dest.loc = OLoc.register AND dest.reg = EAX
       AND ins.imsize = 4 THEN
      ins.opcode := opcode[op].Aimm32;
      writecode(t, ins);
    ELSE
      build_modrm(t, dest, t.opcode[opcode[op].immop], ins);
      IF ins.imsize = 1 THEN
        IF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.t] = 1 THEN
          ins.opcode := opcode[op].imm32 - 1;
          writecode(t, ins);
          log_global_var(t, dest.mvar, -5);
        ELSIF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.t] = 2 THEN
          ins.prefix := TRUE;
          ins.opcode := opcode[op].imm8;
          writecode(t, ins);
          log_global_var(t, dest.mvar, -5);
        ELSE
          ins.opcode := opcode[op].imm8;
          writecode(t, ins);
          IF dest.loc = OLoc.mem THEN
            log_global_var(t, dest.mvar, -5);
          END
        END
      ELSE
        <* ASSERT dest.loc # OLoc.mem OR CG_Bytes[dest.mvar.t] = 4 *>
        ins.opcode := opcode[op].imm32;
        writecode(t, ins);
        IF dest.loc = OLoc.mem THEN
          log_global_var(t, dest.mvar, -8);
        END
      END
    END
  END immOp;

PROCEDURE binOp (t: T; op: Op; READONLY dest, src: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF src.loc = OLoc.imm THEN
      immOp(t, op, dest, src.imm);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      build_modrm(t, src, dest, ins);
      ins.opcode := opcode[op].rrm + 1;
      IF src.loc = OLoc.mem THEN <* ASSERT CG_Bytes[src.mvar.t] = 4 *> END
    ELSE
      <* ASSERT src.loc = OLoc.register AND CG_Bytes[src.mvar.t] = 4 *>
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
  END binOp;

PROCEDURE tableOp (t: T; op: Op; READONLY dest, index: Operand;
                   scale: INTEGER; table: MVar) =
  VAR ins: Instruction;  fully_known := FALSE;
  BEGIN
    <* ASSERT dest.loc = OLoc.register AND index.loc = OLoc.register *>

    ins.disp := table.o;
    IF table.var.loc = VLoc.temp THEN
      <* ASSERT table.var.parent = t.current_proc *>
      INC(ins.disp, table.var.offset);
      fully_known := TRUE;
    END;

    ins.mrmpres := TRUE;
    IF (NOT fully_known) OR (ins.disp > 16_7f) OR (ins.disp < -16_80) THEN
      ins.dsize := 4;
      ins.modrm := dest.reg*8 + 4;
      IF fully_known THEN INC (ins.modrm, 16_80); END;
    ELSE
      ins.dsize := 1;
      ins.modrm := 16_40 + dest.reg*8 + 4;
    END;

    ins.sibpres := TRUE;
    CASE scale OF
    | 1 => ins.sib := 0;
    | 2 => ins.sib := 16_40;
    | 4 => ins.sib := 16_80;
    | 8 => ins.sib := 16_C0;
    ELSE t.Err("tableOp called with invalid scale parameter");
    END;
    INC(ins.sib, index.reg*8);
    INC(ins.sib, 5);

    Mn(t, opcode[op].name);  MnOp(t, dest); MnMVar(t, table);
    Mn(t, "::["); MnOp(t, index); Mn(t, " *"); MnImm (t, scale);
    Mn(t, " +");  MnImm(t, ins.disp);  Mn(t, " ]");

    ins.opcode := opcode[op].rrm+1;
    writecode(t, ins);
    log_global_var(t, table, -4);
  END tableOp;

PROCEDURE swapOp (t: T; READONLY dest, src: Operand) =
  VAR xchg, ins: Instruction;  otherreg: Regno;
  BEGIN
    <* ASSERT (dest.loc = OLoc.register OR dest.loc = OLoc.mem) AND
              (src.loc = OLoc.register OR src.loc = OLoc.mem) *>

    IF dest.loc = OLoc.register AND src.loc = OLoc.register
      AND (dest.reg = EAX OR src.reg = EAX) THEN
      IF dest.reg = EAX
        THEN otherreg := src.reg;
        ELSE otherreg := dest.reg;
      END;
      Mn(t, "XCHG ");  MnOp(t, dest);   MnOp(t, src);
      xchg.opcode := 16_90 + otherreg;
      writecode (t, xchg);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      <* ASSERT src.loc = OLoc.register OR CG_Bytes[src.mvar.t] = 4 *>
      build_modrm(t, src, dest, ins);
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      <* ASSERT dest.loc = OLoc.register OR CG_Bytes[dest.mvar.t] = 4 *>
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

PROCEDURE movOp (t: T; READONLY dest, src: Operand) =
  VAR ins: Instruction;  mnemonic: TEXT := NIL;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF src.loc = OLoc.imm THEN
      movImm(t, dest, src.imm);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND dest.reg = EAX AND
       src.loc = OLoc.mem AND CG_Bytes[src.mvar.t] = 4 AND
       src.mvar.var.loc = VLoc.global THEN
      Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
      ins.opcode := 16_A1;
      ins.disp   := src.mvar.o;
      ins.dsize  := 4;
      writecode (t, ins);
      log_global_var(t, src.mvar, -4);
      RETURN;
    END;

    IF src.loc = OLoc.register AND src.reg = EAX AND
       dest.loc = OLoc.mem AND dest.mvar.var.loc = VLoc.global THEN
      Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
      ins.opcode := 16_A2;
      get_op_size(dest.mvar.t, ins);
      ins.disp := dest.mvar.o;
      ins.dsize := 4;
      writecode(t, ins);
      log_global_var(t, dest.mvar, -4);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND src.loc = OLoc.mem AND
       CG_Bytes[src.mvar.t] # 4 THEN
      CASE src.mvar.t OF
      | Type.Word8  => ins.opcode := 16_8A;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[dest.reg], t.reg[dest.reg]);
      | Type.Word16 => ins.opcode := 16_8B;  ins.prefix := TRUE;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[dest.reg], t.reg[dest.reg]);
      | Type.Int8   => ins.opcode := 16_BE;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      | Type.Int16  => ins.opcode := 16_BF;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      ELSE
        t.Err("Unknown type of size other than dword in movOp");
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
        get_op_size(src.mvar.t, ins);
      ELSE
        INC(ins.opcode);
      END
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      build_modrm(t, dest, src, ins);
      ins.opcode := 16_88;
      get_op_size(dest.mvar.t, ins);
    END;

    Mn(t, "MOV");  MnOp(t, dest);  MnOp(t, src);
    writecode(t, ins);

    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END movOp;

PROCEDURE movDummyReloc(t: T; READONLY dest: Operand; sym: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    Mn(t, "MOV");  MnOp(t, dest);  Mn (t, " imm32");
    ins.opcode := 16_B8 + dest.reg;
    ins.imm    := 0;
    ins.imsize := 4;
    writecode(t, ins);
    t.obj.relocate(t.textsym, t.obj.cursor(Seg.Text) - 4, sym);
  END movDummyReloc;

PROCEDURE movImm (t: T; READONLY dest: Operand; imm: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    IF dest.loc # OLoc.register THEN
      <* ASSERT dest.loc = OLoc.mem *>
      ins.opcode := 16_C6;
      get_op_size(dest.mvar.t, ins);
      build_modrm(t, dest, t.opcode[0], ins);
      Mn(t, "MOV");  MnOp(t, dest);  MnImm(t, imm);
      ins.imm    := imm;
      ins.imsize := CG_Bytes[dest.mvar.t];
      writecode(t, ins);
      log_global_var(t, dest.mvar, -4 - CG_Bytes[dest.mvar.t]);
    ELSIF imm = 0 THEN
      binOp(t, Op.oXOR, dest, dest);
    ELSE
      ins.opcode := 16_B8 + dest.reg;
      ins.imm    := imm;
      ins.imsize := 4;
      Mn(t, "MOV");  MnOp(t, dest);  MnImm(t, imm);
      writecode(t, ins);
    END;
  END movImm;

PROCEDURE pushOp (t: T; READONLY src: Operand) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, "PUSH");  MnOp(t, src);
    CASE src.loc OF
    | OLoc.imm =>
        ins.opcode := 16_68;
        ins.imm    := src.imm;
        ins.imsize := 4;
        writecode(t, ins);
    | OLoc.register =>
        ins.opcode := 16_50 + src.reg;
        writecode(t, ins);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[src.mvar.t] = 4 *>
        build_modrm(t, src, t.opcode[6], ins);
        ins.opcode := 16_FF;
        writecode(t, ins);
        log_global_var(t, src.mvar, -4);
    ELSE
      t.Err("Tried to push an fstack element to the integer stack");
    END
  END pushOp;

PROCEDURE popOp (t: T; READONLY dest: Operand) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, "POP");  MnOp(t, dest);
    CASE dest.loc OF
    | OLoc.imm =>
        t.Err("Tried to pop into an immediate stack element");
    | OLoc.register =>
        ins.opcode := 16_58 + dest.reg;
        writecode(t, ins);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[dest.mvar.t] = 4 *>
        build_modrm(t, dest, t.opcode[6], ins);
        ins.opcode := 16_FF;
        writecode(t, ins);
        log_global_var(t, dest.mvar, -4);
    ELSE
      t.Err("Tried to pop an fstack element from the integer stack");
    END
  END popOp;

PROCEDURE decOp (t: T; READONLY op: Operand) =
  VAR ins: Instruction;
  BEGIN
    Mn(t, "DEC");  MnOp(t, op);
    <* ASSERT op.loc = OLoc.mem OR op.loc = OLoc.register *>
    IF op.loc = OLoc.register THEN
      ins.opcode := 16_48 + op.reg;
      writecode(t, ins);
    ELSE
      <* ASSERT op.loc = OLoc.mem AND CG_Bytes[op.mvar.t] = 4 *>
      build_modrm(t, op, t.opcode[1], ins);
      ins.opcode := 16_FF;
      writecode(t, ins);
      log_global_var(t, op.mvar, -4);
    END
  END decOp;

PROCEDURE unOp (t: T; op: Op; READONLY dest: Operand) =
  VAR ins: Instruction;
  BEGIN
    ins.opcode := opcode[op].imm32;
    IF dest.loc = OLoc.mem THEN
      get_op_size(dest.mvar.t, ins);
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
  END unOp;

PROCEDURE mulOp (t: T; READONLY src: Operand) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT src.loc = OLoc.register OR (src.loc = OLoc.mem AND
              CG_Bytes[src.mvar.t] = 4) *>
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
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.t] = 4 *>
    Mn(t, "IMUL");  MnOp(t, dest);  MnOp(t, src);
    IF src.loc = OLoc.imm THEN
      build_modrm(t, t.reg[dest.reg], dest, ins);
      ins.opcode := 16_69;
      ins.imm := src.imm;
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

PROCEDURE imulImm (t: T; READONLY dest, src: Operand; imm, imsize: INTEGER) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.t] = 4 *>
    build_modrm(t, src, dest, ins);
    Mn(t, "IMUL");  MnOp(t, dest);  MnOp(t, src);  MnImm(t, imm);
    IF imsize = 1
      THEN ins.opcode := 16_6B;
      ELSE ins.opcode := 16_69;
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
              AND CG_Bytes[divisor.mvar.t] = 4) *>
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
              AND CG_Bytes[divisor.mvar.t] = 4) *>
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
    IF apos
      THEN binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]); (*   XOR EDX, EDX      *)
      ELSE noargOp(t, Op.oCDQ);                       (*   CDQ               *)
    END;
    idivOp(t, divisor);                               (*   IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);                     (*   JMP endlab        *)
    set_label(t, diffsignlab);                        (* .diffsignlab        *)
    noargOp(t, Op.oCDQ);                              (*   CDQ               *)
    idivOp(t, divisor);                               (*   IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], 0);                 (*   CMP EDX, #0       *)
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
    IF apos
      THEN binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]); (*    XOR EDX, EDX      *)
      ELSE noargOp(t, Op.oCDQ);                       (*    CDQ               *)
    END;
    idivOp(t, divisor);                               (*    IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);                     (*    JMP endlab        *)
    set_label(t, diffsignlab);                        (* .diffsignlab         *)
    noargOp(t, Op.oCDQ);                              (*    CDQ               *)
    idivOp(t, divisor);                               (*    IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], 0);                 (*    CMP EDX, #0       *)
    brOp(t, Cond.E, endlab);                          (*    JE  endlab        *)
    binOp(t, Op.oADD, t.reg[EDX], divisor);           (*    ADD EDX, divisor  *)
    set_label(t, endlab);                             (* .endlab              *)
  END diffmodOp;

PROCEDURE must_extend (<*UNUSED*> t: T; READONLY src: Operand): BOOLEAN =
  BEGIN
    IF src.loc # OLoc.mem THEN
      RETURN FALSE;
    END;
    IF src.mvar.t = Type.Word8 OR src.mvar.t = Type.Word16 OR
       src.mvar.t = Type.Int8 OR src.mvar.t = Type.Int16 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END
  END must_extend;

PROCEDURE get_addsize (<*UNUSED*> t: T; READONLY op: Operand): INTEGER =
  BEGIN
    IF op.loc # OLoc.mem THEN
      RETURN 0;
    END;

    IF op.mvar.var.loc = VLoc.global THEN
      RETURN 4;
    END;

    WITH offset = op.mvar.o + op.mvar.var.offset DO
      IF offset > 16_7F OR offset < -16_80 THEN
        RETURN 4;
      ELSE
        RETURN 1;
      END
    END
  END get_addsize;

TYPE
  Instruction = RECORD
    escape  : BOOLEAN := FALSE;
    prefix  : BOOLEAN := FALSE;
    mrmpres : BOOLEAN := FALSE;
    sibpres : BOOLEAN := FALSE;
    opcode  : INTEGER := 0;
    modrm   : INTEGER := 0;
    sib     : INTEGER := 0;
    disp    : INTEGER := 0;
    dsize   : INTEGER := 0;
    imm     : INTEGER := 0;
    imsize  : INTEGER := 0;
  END;

PROCEDURE get_op_size (type: MType;  VAR ins: Instruction) =
  BEGIN
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
    ins.mrmpres := TRUE;

    <* ASSERT reg.loc = OLoc.register *>
    IF mem.loc = OLoc.register THEN
      ins.disp := 0;
      ins.dsize := 0;
      ins.modrm := 16_C0 + reg.reg*8 + mem.reg;
      RETURN;
    END;

    <* ASSERT mem.loc = OLoc.mem *>

    <* ASSERT CG_Bytes[mem.mvar.t] # 1 OR reg.opcode OR
              reg.reg IN RegSet { EAX, EBX, ECX, EDX } *>

    offset := mem.mvar.o;
    IF mem.mvar.var.loc = VLoc.temp THEN
      <* ASSERT mem.mvar.var.parent = t.current_proc *>
      INC(offset, mem.mvar.var.offset);
      fully_known := TRUE;
    END;
    IF (NOT fully_known) OR (offset > 16_7f) OR (offset < -16_80) THEN
      ins.disp := offset;
      ins.dsize := 4;
      IF NOT fully_known
        THEN ins.modrm := reg.reg*8 + 5;
        ELSE ins.modrm := 16_80 + reg.reg*8 + EBP;
      END;
    ELSE
      ins.disp := offset;
      ins.dsize := 1;
      ins.modrm := 16_40 + reg.reg*8 + EBP;
    END;
  END build_modrm;

PROCEDURE debugcode (t: T;  READONLY ins: Instruction) =
  VAR len := 0;
  BEGIN
    (* generate the PC label *)
    t.wr.OutC(' ');
    HexBE(t, t.obj.cursor(Seg.Text), 4);
    t.wr.OutT(": ");

    (* generate the instruction bytes *)
    IF ins.escape     THEN  Byte(t, 16_0F);       INC(len); END;
    IF ins.prefix     THEN  Byte(t, 16_66);       INC(len); END;
                            Byte(t, ins.opcode);  INC(len);
    IF ins.mrmpres    THEN  Byte(t, ins.modrm);   INC(len); END;
    IF ins.sibpres    THEN  Byte(t, ins.sib);     INC(len); END;
    IF ins.dsize # 0  THEN  HexLE(t, ins.disp, ins.dsize); INC(len,ins.dsize); END;
    IF ins.imsize # 0 THEN  HexLE(t, ins.imm, ins.imsize); INC(len,ins.imsize); END;

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
    IF t.debug THEN debugcode (t, ins); END;

    IF ins.escape THEN
      t.obj.append(Seg.Text, 16_0F, 1);
    END;

    IF ins.prefix THEN
      t.obj.append(Seg.Text, 16_66, 1);
    END;

    <* ASSERT ins.opcode >= 0 AND ins.opcode <= 255 *>
    t.obj.append(Seg.Text, ins.opcode, 1);

    IF ins.mrmpres THEN
      t.obj.append(Seg.Text, ins.modrm, 1);
    END;

    IF ins.sibpres THEN
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

PROCEDURE case_jump (t: T; index: Operand; READONLY labels: ARRAY OF Label) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT index.loc = OLoc.register *>
    WITH curs = t.obj.cursor(Seg.Text) DO
      ins.opcode  := 16_FF;
      ins.modrm   := 16_24;                   ins.mrmpres := TRUE;
      ins.sib     := 16_85 + index.reg * 8;   ins.sibpres := TRUE;
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

PROCEDURE load_ind (t: T; r: Regno; READONLY ind: Operand; o: ByteOffset;
                    type: MType) =
  VAR ins: Instruction;
      mnemonic := "MOV";
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    ins.opcode := 16_8B;
    IF CG_Bytes[type] # 4 THEN
      CASE type OF
      | Type.Word8  => ins.opcode := 16_8A;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Word16 => ins.opcode := 16_8B;  ins.prefix := TRUE;
                       mnemonic := "MOV";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Int8   => ins.opcode := 16_BE;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      | Type.Int16  => ins.opcode := 16_BF;  ins.escape := TRUE;
                       mnemonic := "MOVSX";
      ELSE
        t.Err("Unknown type of size other than dword in load_ind");
      END;
    END;
    Mn(t, mnemonic, " ", RegName[r]);  MnPtr(t, ind, o, type);
    ins.mrmpres := TRUE;
    ins.disp := o;
    IF o > -16_81 AND o < 16_80 THEN
      ins.modrm := 16_40 + r * 8 + ind.reg;
      ins.dsize := 1;
    ELSE
      ins.modrm := 16_80 + r * 8 + ind.reg;
      ins.dsize := 4;
    END;
    IF ind.reg = ESP THEN  ins.sib := 16_24; ins.sibpres := TRUE;  END;
    writecode (t, ins);
  END load_ind;

PROCEDURE fast_load_ind (t: T; r: Regno; READONLY ind: Operand; o: ByteOffset;
                    size: INTEGER) =
  VAR ins: Instruction;  type := Type.Int32;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    ins.opcode := 16_8B;
    CASE size OF
    | 1 => ins.opcode := 16_8A;  type := Type.Int8;
    | 2 => ins.opcode := 16_8B;  ins.prefix := TRUE;  type := Type.Int16;
    | 4 => ins.opcode := 16_8B;  type := Type.Int32;
    ELSE   t.Err("Illegal size in fast_load_ind");
    END;

    Mn(t, "MOV ", RegName[r]);  MnPtr(t, ind, o, type);
    ins.mrmpres := TRUE;
    ins.disp := o;
    IF o > -16_81 AND o < 16_80 THEN
      ins.modrm := 16_40 + r * 8 + ind.reg;
      ins.dsize := 1;
    ELSE
      ins.modrm := 16_80 + r * 8 + ind.reg;
      ins.dsize := 4;
    END;
    IF ind.reg = ESP THEN  ins.sib := 16_24;  ins.sibpres := TRUE;  END;
    writecode (t, ins);
  END fast_load_ind;

PROCEDURE store_ind (t: T; READONLY val, ind: Operand; o: ByteOffset;
                     type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register AND val.loc # OLoc.mem *>

    ins.opcode := 16_88;
    IF val.loc = OLoc.imm THEN
      ins.opcode := 16_C6;
      ins.imm := val.imm;
      ins.imsize := CG_Bytes[type];
    END;

    get_op_size(type, ins);
    Mn(t, "MOV");  MnPtr(t, ind, o, type);  MnOp(t, val);

    ins.mrmpres := TRUE;
    ins.disp := o;
    IF o >= -16_80 AND o <= 16_7F THEN
      ins.dsize := 1;
      ins.modrm := 16_40 + ind.reg;
    ELSE
      ins.dsize := 4;
      ins.modrm := 16_80 + ind.reg;
    END;

    IF val.loc # OLoc.imm THEN
      INC(ins.modrm, val.reg * 8);
    END;
    IF ind.reg = ESP THEN  ins.sib := 16_24;  ins.sibpres := TRUE;  END;
    writecode (t, ins);
  END store_ind;

PROCEDURE f_loadind (t: T; READONLY ind: Operand; o: ByteOffset; type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    prepare_stack(t, FOp.fLD, TRUE);
    Mn(t, "FLD");  MnPtr(t, ind, o, type);
    IF type = Type.Reel
      THEN ins.opcode := fopcode[FOp.fLD].m32;
      ELSE ins.opcode := fopcode[FOp.fLD].m64;
    END;
    ins.modrm := 16_40 + fopcode[FOp.fLD].memop * 8 + ind.reg;
    ins.mrmpres := TRUE;
    ins.disp := o;
    IF o >= -16_80 AND o <= 16_7F
      THEN ins.dsize := 1;
      ELSE ins.dsize := 4;  INC (ins.modrm, 16_40);
    END;
    IF ind.reg = ESP THEN  ins.sib := 16_24;  ins.sibpres := TRUE;  END;
    writecode (t, ins);
    INC(t.fstacksize);
    INC(t.fstackloaded);
  END f_loadind;

PROCEDURE f_storeind (t: T; READONLY ind: Operand; o: ByteOffset;
                      type: MType) =
  VAR ins: Instruction;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    fstack_check(t, 1, "f_storeind");
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;
    Mn(t, "FSTP");  MnPtr(t, ind, o, type);
    IF type = Type.Reel
      THEN ins.opcode := fopcode[FOp.fSTP].m32;
      ELSE ins.opcode := fopcode[FOp.fSTP].m64;
    END;
    ins.modrm := 16_40 + fopcode[FOp.fSTP].memop * 8 + ind.reg;
    ins.mrmpres := TRUE;
    ins.disp := o;
    IF o >= -16_80 AND o <= 16_7F
      THEN ins.dsize := 1;
      ELSE ins.dsize := 4;  INC (ins.modrm, 16_40);
    END;
    IF ind.reg = ESP THEN  ins.sib := 16_24;  ins.sibpres := TRUE;  END;
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

PROCEDURE log_unknown_label (t: T; l: Label; loc: ByteOffset; abs: BOOLEAN) =
  BEGIN
    check_label(t, l, "log_unknown_label");
    t.labarr[l].usage := NEW(LabList, seg := Seg.Text,
                             offs := loc, abs := abs,
                             link := t.labarr[l].usage);
  END log_unknown_label;

PROCEDURE log_label_init (t: T; var: x86Var; o: ByteOffset; lab: Label) =
  BEGIN
    check_label(t, lab, "log_label_init");

    t.obj.relocate(var.symbol, o, t.textsym);

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

PROCEDURE set_label (t: T; l: Label; offset := 0) =
  BEGIN
    check_label(t, l, "set_label");
    WITH lab = t.labarr[l] DO
      IF NOT lab.no_address THEN
        t.Err("Duplicate label definition");
      END;
      lab.offset := t.obj.cursor(Seg.Text) + offset;
      lab.no_address := FALSE;
      IF lab.usage # NIL THEN
        fill_in_label_thread(t, lab.usage, lab.offset, lab.short);
        lab.usage := NIL;
      END
    END
  END set_label;

PROCEDURE check_label(t: T; l: Label; place: TEXT) =
  BEGIN
    IF l >= t.next_label_id THEN
      t.Err("Tried to reference unknown label in " & place);
    END
  END check_label;

PROCEDURE fill_in_label_thread (t: T; ptr: LabList; val: INTEGER;
                                short: BOOLEAN) =
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
    IF t.ftop_mem.t = Type.Reel
      THEN ins.opcode := fopcode[FOp.fLD].m32;
      ELSE ins.opcode := fopcode[FOp.fLD].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem},
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
    IF mvar.t = Type.Reel
      THEN ins.opcode := fopcode[FOp.fSTP].m32;
      ELSE ins.opcode := fopcode[FOp.fSTP].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar:= mvar},
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
    IF t.ftop_inmem THEN fstack_loadtop(t); END;

    get_temp(t);
    get_temp(t);

    Mn(t, "FLD ST, m80real");
    build_modrm(t, t.fstackspill[t.fspilltop-2], t.opcode[5], ins);
    ins.opcode := 16_DB;
    writecode(t, ins);

    Mn(t, "FLD ST, m80real");
    build_modrm(t, t.fstackspill[t.fspilltop-1], t.opcode[5], ins);
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
      mvar.t := type;
      mvar.o := 0;
    END;

    INC(t.fstacksize);

    t.f_litlist := NEW(FLiteral, arr := flarr, size := CG_Bytes[type],
                       loc := 0, link := t.f_litlist);
  END f_loadlit;

PROCEDURE fstack_check (t: T; depth: INTEGER; place: TEXT) =
  BEGIN
    IF t.fstacksize < depth THEN
      t.Err("Floating stack underflow in " & place);
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
      t.Err("Stack overflow in fstack_wipeup");
    END;
    IF wipeup > t.fspilltop THEN
      t.Err("Not enough spilled fstack elements to replace in fstack_wipeup");
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
      | OLoc.register => Mn (t, " ", RegName[op.reg]);
      | OLoc.imm      => Mn (t, " $", Fmt.Int (op.imm));
      | OLoc.mem      => MnMVar (t, op.mvar);
      END
    END
  END MnOp;

PROCEDURE MnMVar(t: T;  READONLY mvar: MVar) =
  BEGIN
    IF t.debug THEN
      MnVar (t, mvar.var);
      IF mvar.o # 0 THEN  Mn(t, "+", Fmt.Int(mvar.o)); END;
      IF (mvar.t # Type.Int32) AND (mvar.t # Type.Word32) THEN
        Mn (t, ":", Target.TypeNames[mvar.t]);
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

PROCEDURE MnLabel(t: T;  l: Label) =
  BEGIN
    IF t.debug THEN
      IF (l = No_label)
        THEN Mn(t, " *");
        ELSE Mn(t, " L.", Fmt.Int (l));
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

PROCEDURE MnImm(t: T;  imm: INTEGER) =
  BEGIN
    IF t.debug THEN
      Mn(t, " $", Fmt.Int (imm));
    END;
  END MnImm;

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
    FOR i := size-1 TO 0 BY -1 DO
      Byte(t, Word.Extract(val, i*8, 8));
    END;
  END HexBE;

PROCEDURE HexLE (t: T; val: INTEGER; size: INTEGER) =
  BEGIN
    FOR i := 0 TO size-1 DO
      Byte(t, Word.Extract(val, i*8, 8));
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
    
(*------------------------------------------------------- alignment stuff ---*)

PROCEDURE aligned (<*UNUSED*> t: T; READONLY var: MVar;
                   align: Alignment): BOOLEAN =
  BEGIN
    IF Word.And(var.o + var.var.offset, align - 1) = 0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END
  END aligned;

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
      <* ASSERT t.f_litlist.size = CG_Bytes[mvar.t] *>
      <* ASSERT mvar.t = Type.Reel OR mvar.t = Type.LReel OR mvar.t = Type.XReel *>
      t.f_litlist.loc := patch_loc;
    ELSE
      t.obj.patch(Seg.Text, patch_loc, mvar.o + mvar.var.offset, 4);
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
      IF NOT find_flit(t, flptr.arr, flptr.size, f_lit, f_litloc) THEN
        f_litloc := internal_size;
        IF f_littail = NIL THEN
          f_littail := NEW(FLiteral, arr := flptr.arr, size := flptr.size,
                           loc := f_litloc, link := NIL);
          f_lit := f_littail;
        ELSE
          f_littail.link := NEW(FLiteral, arr := flptr.arr, size := flptr.size,
                                loc := f_litloc, link := NIL);
          f_littail := f_littail.link;
        END;

        INC(internal_size, flptr.size);
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
      FOR i := 0 TO f_lit.size-1 DO
        EVAL TargetInt.FromInt(f_lit.arr[i], tint);
        t.parent.init_int(f_lit.loc + i, tint, Type.Word8);
      END;

      f_lit := f_lit.link;
    END;

    WHILE abscall # NIL DO
      t.parent.init_int(abscall.loc, TargetInt.Zero, Type.Int32);
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
      code.reg[i].reg := i;
      code.reg[i].opcode := FALSE;
      code.opcode[i].loc := OLoc.register;
      code.opcode[i].reg := i;
      code.opcode[i].opcode := TRUE;
    END;

    RETURN code;
  END New;

BEGIN
END Codex86.
