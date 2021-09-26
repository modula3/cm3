(*Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Mar 22 09:02:46 PST 1995 by kalsow     *)
(*      modified on Fri Nov 25 11:36:13 PST 1994 by isard      *)

MODULE Stackx86;

IMPORT M3CG, TargetMap, M3CG_Ops, M3x86Rep, Codex86, Wrx86;

IMPORT TIntN, TWordN;
IMPORT Target, Fmt;
FROM Target IMPORT FloatType;
FROM TargetMap IMPORT CG_Bytes, CG_Align_bytes, CG_Size;

FROM M3CG IMPORT Type, MType, ZType, IType, Sign, Label, ByteOffset;
FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3x86Rep IMPORT Operand, MVar, Regno, OLoc, VLoc, NRegs, Force, TypeIs64, OperandPart, RegName, OperandSize, TZero, AllRegisters;
FROM M3x86Rep IMPORT RegistersForByteOperations, RegSet, FlToInt, x86Var, x86Proc, NoStore, SplitOperand, SplitMVar, GetTypeSize, GetOperandSize;
FROM M3x86Rep IMPORT TypeIsSigned, TypeIsUnsigned, EAX, ECX, EDX, EBX, UnsignedType, MaximumShift, MinimumShift, BitCountMask, IntType;

FROM Codex86 IMPORT Op, FOp, Cond, revcond;

IMPORT RTIO;

REVEAL T = Public BRANDED "Stackx86.T" OBJECT
        cg            : Codex86.T := NIL;
        parent        : M3x86Rep.U := NIL;
        Err           : ErrorHandler := NIL;
        debug         := FALSE;
        stacktop      := 0;
        vstack        : REF ARRAY OF Operand := NIL;
        vstacklimit   := 0;
        reguse        : ARRAY [0 .. NRegs] OF Register;
        current_proc  : x86Proc;
        rmode         : ARRAY FlToInt OF TIntN.T;
      OVERRIDES
        init := init;
        end := end;
        set_current_proc := set_current_proc;
        unlock := unlock;
        lock := lock;
        clearall := clearall;
        releaseall := releaseall;
        all_to_mem := all_to_mem;
        find := find;
        freereg := freereg;
        set_reg := set_reg;
        set_type := set_type;
        dealloc_reg := dealloc_reg;
        corrupt := corrupt;
        set_fstack := set_fstack;
        set_mvar := set_mvar;
        set_imm := set_imm;
        loc := get_loc;
        op := get_op;
        pos := pos;
        discard := discard;
        set_error_handler := set_error_handler;
        push := push;
        pushnew := pushnew;
        pushimmI := pushimmI;
        pushimmT := pushimmT;
        pop := pop;
        doloadaddress := doloadaddress;
        dobin := dobin;
        dostoreind := dostoreind;
        doumul := doumul;
        doimul := doimul;
        dodiv := dodiv;
        domod := domod;
        doimm := doimm;
        doneg := doneg;
        doabs := doabs;
        domaxmin := domaxmin;
        fltoint := fltoint;
        inttoflt := inttoflt;
        doshift := doshift;
        dorotate := dorotate;
        doextract := doextract;
        doextract_n := doextract_n;
        doextract_mn := doextract_mn;
        doinsert := doinsert;
        doinsert_n := doinsert_n;
        doinsert_mn := doinsert_mn;
        swap := swap;
        doloophole := doloophole;
        doindex_address := doindex_address;
        newdest := newdest;
        reg := reg;
        lower := lower;
        set_lower := set_lower;
        upper := upper;
        set_upper := set_upper;
        non_nil := non_nil;
        set_non_nil := set_non_nil;
      END;

TYPE
  Register = RECORD
    stackp     : INTEGER(*CARDINAL*) := -1;
    operandPart: INTEGER(*OperandPart*) := -1;
    last_store : MVar    := NoStore;
    last_imm   : TIntN.T := TZero;
    lowbound   : TIntN.T;
    upbound    : TIntN.T;
    imm        : BOOLEAN := FALSE;
    locked     : BOOLEAN := FALSE;
    non_nil    : BOOLEAN := FALSE;
  END;

PROCEDURE InitRegister(locked: BOOLEAN := FALSE):Register =
BEGIN
  RETURN Register { locked := locked,
                    (* BUG? Upbound/lowbound should adapt to the type? *)
                    lowbound := TIntN.TargetIntegerMin,
                     upbound := TIntN.TargetIntegerMax};
END InitRegister;

(*-------------------------------------------- register handling routines ---*)

CONST HighPrec: INTEGER = NRegs * 1000;

PROCEDURE check (t: T; where: TEXT) =
  VAR size: OperandSize;
  BEGIN
    t.cg.wrFlush();

    FOR i := 0 TO NRegs DO
      IF t.reguse[i].stackp # -1 THEN
        IF t.vstack[t.reguse[i].stackp].reg[t.reguse[i].operandPart] # i THEN
          Err(t, where
              & " i:" & RegName[i]
              & " t.reguse[i].stackp:" & Fmt.Int(t.reguse[i].stackp)
              & " t.vstack[t.reguse[i].stackp].reg[t.reguse[i].operandPart]:" & RegName[t.vstack[t.reguse[i].stackp].reg[t.reguse[i].operandPart]]);
        END;
      END
    END;

    FOR i := 0 TO t.stacktop - 1 DO
      IF t.vstack[i].loc = OLoc.register THEN
        size := GetOperandSize(t.vstack[i]);
        FOR j := 0 TO size - 1 DO
          IF t.reguse[t.vstack[i].reg[j]].stackp # i THEN
            Err(t, where
                & " i:" & Fmt.Int(i)
                & " j:" & Fmt.Int(j)
                & " t.vstack[i].reg[j]:" & RegName[t.vstack[i].reg[j]]
                & " t.reguse[t.vstack[i].reg[j]].stackp:" & Fmt.Int(t.reguse[t.vstack[i].reg[j]].stackp));
          END;
        END;
      END;
    END;
  END check;

PROCEDURE check_float (t: T) =
  VAR flcount := 0;
  BEGIN
    t.cg.wrFlush();

    FOR i := 0 TO t.stacktop - 1 DO
      IF t.vstack[i].loc = OLoc.fstack THEN
        INC(flcount);
      END
    END;
    t.cg.assert_fstack(flcount);
  END check_float;

PROCEDURE unlock (t: T) =
  BEGIN
    FOR i := 0 TO NRegs DO
      t.reguse[i].locked := FALSE;
    END;

    IF t.debug THEN
      check(t, "unlock");
      check_float(t);
    END
  END unlock;

PROCEDURE lock (t: T; r: Regno) =
  BEGIN
    t.reguse[r].locked := TRUE;
  END lock;

PROCEDURE loadreg (t: T; r: Regno; READONLY op: Operand; operandPart: OperandPart) =
  BEGIN
    t.cg.movOp(t.cg.reg[r], op);

    t.reguse[r] := InitRegister(locked := t.reguse[r].locked);
    t.reguse[r].stackp := op.stackp;
    t.reguse[r].operandPart := operandPart;

    IF op.loc = OLoc.mem THEN
      IF op.mvar.var.stack_temp THEN
        IF operandPart = 0 THEN
          t.parent.free_temp(op.mvar.var);
        END;
      ELSE
        t.reguse[r].last_store := op.mvar;
      END
    END;

    IF op.loc = OLoc.imm THEN
      t.reguse[r].last_imm := op.imm;
      t.reguse[r].imm := TRUE;
    END;

    set_reg(t, op.stackp, r, operandPart);
  END loadreg;

PROCEDURE loadphantom (t: T; r: Regno; stackp: CARDINAL; operandPart: OperandPart) =
  BEGIN
    t.reguse[r].stackp := stackp;
    t.reguse[r].operandPart := operandPart;
    t.vstack[stackp].loc := OLoc.register;
    t.vstack[stackp].reg[operandPart] := r;
  END loadphantom;

PROCEDURE copyreg (t: T; stackp: CARDINAL; to, from: Regno; operandPart: OperandPart) =
  BEGIN
    t.reguse[to] := t.reguse[from];
    set_reg(t, stackp, to, operandPart);
    t.cg.movOp(t.cg.reg[to], t.cg.reg[from]);
  END copyreg;

PROCEDURE movereg (t: T; to, from: Regno; operandPart: OperandPart) =
  BEGIN
    t.reguse[to] := t.reguse[from];
    t.reguse[from].stackp := -1;
    t.reguse[from].operandPart := -1;
    set_reg(t, t.reguse[to].stackp, to, operandPart);
    t.cg.movOp(t.cg.reg[to], t.cg.reg[from]);
  END movereg;

PROCEDURE swapreg (t: T; to, from: Regno; operandPart: OperandPart) =
  VAR tempstack := t.reguse[from].stackp;
      tempstore := t.reguse[from].last_store;
      temppart  := t.reguse[from].operandPart;
  BEGIN
    <* ASSERT to # from *>
    <* ASSERT t.reguse[from].stackp = -1 OR t.reguse[from].operandPart = operandPart *>
    t.reguse[from].stackp := t.reguse[to].stackp;
    t.reguse[to].stackp := tempstack;
    t.reguse[from].last_store := t.reguse[to].last_store;
    t.reguse[to].last_store := tempstore;
    t.reguse[from].operandPart := t.reguse[to].operandPart;
    t.reguse[to].operandPart := temppart;

    IF t.reguse[from].stackp # -1 THEN
      set_reg(t, t.reguse[from].stackp, from, t.reguse[from].operandPart);
    END;

    IF tempstack # -1 THEN
      set_reg(t, tempstack, to, temppart);
    END;

    t.cg.swapOp(t.cg.reg[to], t.cg.reg[from]);
  END swapreg;

PROCEDURE clearall (t: T) =
  BEGIN
    t.cg.wrFlush();

    FOR r := 0 TO NRegs DO
      IF t.reguse[r].stackp # -1 THEN
        Err(t, "r:" & RegName[r]
            & " t.reguse[r].stackp:" & Fmt.Int(t.reguse[r].stackp));
      END;
      t.reguse[r] := InitRegister();
    END
  END clearall;

PROCEDURE releaseall (t: T) =
  BEGIN
    t.cg.wrFlush();

    FOR r := 0 TO NRegs DO
      t.reguse[r] := InitRegister();
    END
  END releaseall;


PROCEDURE find (t: T; stackp: CARDINAL;
                force: Force := Force.any; set := AllRegisters;
                hintaddr := FALSE) =
(*
 * Find a suitable register to put a stack item in.
 *)
  VAR in: ARRAY OperandPart OF Regno; (* initialize to -1? *)
      to: ARRAY OperandPart OF Regno; (* initialize to -1? *)
      opA: ARRAY OperandPart OF Operand;
      done := ARRAY OperandPart OF BOOLEAN{FALSE,..};
      size: OperandSize := 1;
      ret64: BOOLEAN := FALSE;
      compare_exchange_64: BOOLEAN := FALSE;
  BEGIN
    WITH op = t.vstack[stackp] DO

      <* ASSERT op.stackp = stackp *>

      size := SplitOperand(op, opA);
      <* ASSERT (size = 1) OR (size = 2) *>
      IF size = 2 THEN
        ret64 := (force = Force.regset AND set = RegSet{EAX, EDX});
        compare_exchange_64 := (force = Force.regset AND set = RegSet{ECX, EBX});
      END;

      FOR i := 0 TO size - 1 DO
        CASE op.loc OF
          OLoc.fstack =>
              Err(t, "Tried to put a float in an int register in 'find'");
          | OLoc.mem =>
              IF i = 0 THEN
                in[i] := inreg(t, opA[i].mvar, set);
              ELSE
                in[i] := inreg(t, opA[i].mvar, set - RegSet{in[0]});
              END;
          | OLoc.register =>
              in[i] := op.reg[i];
          | OLoc.imm =>
              IF i = 0 THEN
                in[i] := immreg(t, opA[i].imm, set);
              ELSE
                in[i] := immreg(t, opA[i].imm, set - RegSet{in[0]});
              END;
          END;
      END;

      IF op.mvar.mvar_type = Type.Addr THEN
        hintaddr := TRUE;
      END;

      (* If it is in a register and shouldn't be, move it *)

      IF force = Force.mem AND op.loc = OLoc.register THEN
        get_temp(t, stackp);
        RETURN;
      END;

      (* If it is an immediate value and should be in mem, do it *)

      IF force = Force.mem AND op.loc = OLoc.imm THEN
        get_temp(t, stackp);
        RETURN;
      END;

      (* If it is immediate and doesn't have to be in a register, then do nothing *)

      IF op.loc = OLoc.imm AND force # Force.anyreg AND force # Force.regset THEN
        RETURN;
      END;

      (* If it isn't in a register yet, and it doesn't have to be, do nothing *)

      IF force = Force.any AND in[0] = -1 AND in[size - 1] = -1 THEN
        RETURN;
      END;

      IF force = Force.anydword AND in[0] = -1 AND in[size - 1] = -1 AND op.loc = OLoc.mem AND CG_Bytes[op.mvar.mvar_type] = (size * 4) THEN
        RETURN;
      END;

      (* If it is in a temporary variable and can stay there, leave it *)

      IF force = Force.anytemp AND in[0] = -1 AND in[size - 1] = -1 AND op.loc = OLoc.mem AND op.mvar.var.stack_temp THEN
        RETURN;
      END;

      IF op.loc = OLoc.mem AND CG_Bytes[op.mvar.mvar_type] = 1 THEN
        force := Force.regset;
        set := set * RegistersForByteOperations;
      END;

      (* If for any reason it isn't in the right register, find the best
         candidate for a register to put it in. *)

      FOR i := 0 TO size - 1 DO
        IF (in[i] = -1) OR (force = Force.regset AND (NOT in[i] IN set))
            OR t.reguse[in[i]].locked

            (* For the case of a 64bit return value, be sure
             * EDX is high, EAX is low. Note that we
             * could get here in other cases, and
             * it could be that the registers are already
             * allocated and reversed and this will
             * unnecessarily swap them. This is easily
             * fixed by having actual return paths/values
             * identify themselves to find() instead of
             * just deducing it from aspects of the parameters.
             *)
            OR (ret64 AND i = 0 AND in[0] # EAX)
            OR (ret64 AND i = 1 AND in[1] # EDX)

            (* compare_exchange_64 similar to ret64 *)
            OR (compare_exchange_64 AND i = 0 AND in[0] # EBX)
            OR (compare_exchange_64 AND i = 1 AND in[1] # ECX)

            OR (t.reguse[in[i]].stackp # -1 AND t.reguse[in[i]].stackp # stackp)
            OR (t.reguse[in[i]].stackp = stackp AND t.reguse[in[i]].operandPart # i) THEN

          IF ret64 THEN
            to[0] := EAX;
            to[1] := EDX;
          ELSIF compare_exchange_64 THEN
            to[0] := EBX;
            to[1] := ECX;
            t.parent.proc_reguse[EBX] := TRUE;
          ELSIF i = 1 AND done[0] = FALSE THEN
            to[i] := pickreg(t, set - RegSet{to[0]}, hintaddr);
          ELSE
            to[i] := pickreg(t, set, hintaddr);
          END;
          done[i] := FALSE;
        ELSE

          (* Otherwise, it is in the right place, so leave it *)

          loadphantom(t, in[i], stackp, i);
          t.reguse[in[i]].locked := TRUE;
          done[i] := TRUE;
        END;
      END;

      IF done[0] AND done[size - 1] THEN
        RETURN;
      END;

      (* Favor putting EAX in the low part of 64bit operands, in
       * case this ends up being the return value.
       *)

      <* ASSERT size < 2 OR to[0] # to[1] *>
      IF size = 2 AND in[0] = -1 AND in[1] = -1 AND to[1] = EAX AND NOT done[0] AND NOT done[1] THEN
        to[1] := to[0];
        to[0] := EAX;
      END;

      (* If it doesn't have to be in a register, and there are no
         unused registers, do nothing *)

      IF force = Force.any AND t.reguse[to[0]].stackp # -1 AND t.reguse[to[size - 1]].stackp # -1 THEN
        RETURN;
      END;

      IF force = Force.anydword
            AND op.loc = OLoc.mem
            AND t.reguse[to[0       ]].stackp # -1
            AND t.reguse[to[size - 1]].stackp # -1
            AND CG_Bytes[op.mvar.mvar_type] = (size * 4) THEN
        RETURN;
      END;

      (* If it is in a temporary variable and can stay there, leave it *)

      IF force = Force.anytemp AND t.reguse[to[0]].stackp # -1 AND t.reguse[in[size - 1]].stackp # -1
            AND op.loc = OLoc.mem
            AND op.mvar.var.stack_temp THEN
        RETURN;
      END;

      (* Handle the case where one or both registers are
       * allocated to this operand, but the wrong operandPart.
       * This could be written without enumerating every case separately.
       *)

      IF size = 2 THEN
        VAR do_loads := FALSE;
        BEGIN
          IF NOT done[0] AND NOT done[1] AND in[0] = to[1] AND in[1] = to[0] THEN
            do_loads := TRUE;
            done[0] := TRUE;
            done[1] := TRUE;
          (* Some of these might be better as "copy" or "move" but should be ok. *)
          ELSIF NOT done[1] AND in[0] = to[1] THEN
            do_loads := TRUE;
            done[1] := TRUE;
           ELSIF NOT done[0] AND in[1] = to[0] THEN
            do_loads := TRUE;
            done[0] := TRUE;
          END;
          IF do_loads THEN
            swapreg(t, in[1], in[0], 0);
            loadphantom(t, in[0], stackp, 0);
            loadphantom(t, in[1], stackp, 1);
          END;
        END;
      END;


      (* Now we know that we want to put it into 'to' *)

      FOR i := 0 TO size - 1 DO

        IF NOT done[i] THEN

          (* If 'to' is unused, this is easy *)

          IF t.reguse[to[i]].stackp = -1 THEN
            IF in[i] = -1 THEN
              loadreg(t, to[i], opA[i], i);
            ELSE
              IF t.reguse[in[i]].stackp = stackp THEN
                movereg(t, to[i], in[i], i);
              ELSE
                copyreg(t, stackp, to[i], in[i], i);
              END
            END;
          ELSE

            (* Otherwise, see if 'in' is used for something other than stackp. If not,
               swap the registers over. If so, force 'to' out. If there is a free
               register, 'to' will be moved into it, otherwise it will be stored to
               memory *)

            IF in[i] = -1
                OR (t.reguse[in[i]].stackp # -1 AND t.reguse[in[i]].stackp # stackp)
                OR (t.reguse[in[i]].stackp = stackp AND t.reguse[in[i]].operandPart # i) THEN
              forceout(t, to[i], i);
              IF in[i] = -1 THEN
                loadreg(t, to[i], opA[i], i);
              ELSE
                copyreg(t, stackp, to[i], in[i], i);
              END
            ELSE
              swapreg(t, to[i], in[i], i);
              loadphantom(t, to[i], stackp, i);
            END;
          END;
        END;
      END;
    END;
    t.reguse[to[0]].locked := TRUE;
    t.reguse[to[size - 1]].locked := TRUE;
    t.parent.proc_reguse[to[0]] := TRUE;
    t.parent.proc_reguse[to[size - 1]] := TRUE;
  END find;

PROCEDURE freereg (t: T; set := AllRegisters; operandPart: OperandPart): Regno =
  VAR to: Regno;
  BEGIN
    to := pickreg(t, set);
    corrupt(t, to, operandPart);
    t.reguse[to].locked := TRUE;
    RETURN to;
  END freereg;

PROCEDURE forceout (t: T; r: Regno; operandPart: OperandPart) =
  VAR dead: Regno;
  BEGIN
    dead := finddead(t);
    IF dead = -1 THEN
      get_temp(t, t.reguse[r].stackp);
    ELSE
      movereg(t, dead, r, operandPart);
    END
  END forceout;

PROCEDURE finddead (t: T): Regno =
  VAR minprec := HighPrec;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF (t.reguse[i].stackp = -1) THEN
        WITH prec = precedence(t, i) DO
          IF prec < minprec THEN
            minprec := prec;
            bestreg := i;
          END
        END
      END
    END;
    RETURN bestreg;
  END finddead;

PROCEDURE pickreg (t: T; set: RegSet:= AllRegisters; hintaddr := FALSE): Regno =
  VAR minprec := HighPrec;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF i IN set THEN
        WITH prec = precedence(t, i, hintaddr) DO
          IF prec < minprec THEN
            minprec := prec;
            bestreg := i;
          END
        END
      END
    END;
    <* ASSERT minprec # HighPrec *>
    RETURN bestreg;
  END pickreg;

PROCEDURE inreg (t: T; READONLY v: MVar; set: RegSet:= AllRegisters): Regno =
  VAR minprec := HighPrec * HighPrec;
      prec := 0;
      bestreg: Regno := -1;
      hintaddr := FALSE;
  BEGIN
    IF v.mvar_type = Type.Addr THEN
      hintaddr := TRUE;
    END;

    FOR i := 0 TO NRegs DO
      IF t.reguse[i].last_store # NoStore AND v = t.reguse[i].last_store THEN
        prec := precedence(t, i);
        IF NOT i IN set THEN
          prec := prec * HighPrec;
        END;
        IF prec < minprec THEN
          minprec := prec;
          bestreg := i;
        END
      END
    END;
    RETURN bestreg;
  END inreg;

PROCEDURE immreg (t: T; READONLY imm: TIntN.T; set: RegSet:= AllRegisters): Regno =
  VAR minprec := HighPrec * HighPrec;
      prec := 0;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF t.reguse[i].imm AND TIntN.EQ(imm, t.reguse[i].last_imm) THEN
        prec := precedence(t, i);
        IF NOT i IN set THEN
          prec := prec * HighPrec;
        END;
        IF prec < minprec THEN
          minprec := prec;
          bestreg := i;
        END
      END
    END;
    RETURN bestreg;
  END immreg;

CONST baseprec = ARRAY BOOLEAN, [0 .. NRegs] OF INTEGER
                 { ARRAY [0 .. NRegs] OF INTEGER { 6, 5, 2, 1, HighPrec,
                                                   HighPrec, 3, 4 },
                   ARRAY [0 .. NRegs] OF INTEGER { 6, 5, 4, 3, HighPrec,
                                                   HighPrec, 1, 2 } };

PROCEDURE precedence (t: T; r: Regno; hintaddr := FALSE): INTEGER =
  VAR prec: INTEGER;
  BEGIN
    IF baseprec[hintaddr][r] = HighPrec THEN
      RETURN HighPrec;
    END;
    IF t.reguse[r].locked THEN
      RETURN HighPrec;
    END;
    IF t.reguse[r].stackp # -1 THEN
      prec := 4 * NRegs;
    ELSIF t.reguse[r].last_store # NoStore THEN
      prec := 3 * NRegs;
    ELSIF t.reguse[r].imm THEN
      prec := 2 * NRegs;
    ELSE
      prec := NRegs;
    END;
    prec := prec + baseprec[hintaddr][r];
    RETURN prec;
  END precedence;

(*-------------------------------------------------------- stack routines ---*)

PROCEDURE get_temp (t: T; stackp: CARDINAL) =
  VAR op := t.vstack[stackp]; (* *copy* this before changing it *)
      size := GetTypeSize(op.optype);
      mvar: MVar;
  BEGIN
    set_mvar(t, stackp, MVar { var := t.parent.declare_temp(size * 4, 4, op.optype,
                                                            FALSE),
                               mvar_offset := 0, mvar_type := op.optype } );
    t.vstack[stackp].mvar.var.stack_temp := TRUE;
    CASE op.loc OF
      | OLoc.imm =>
        t.cg.movImmT(t.vstack[stackp], op.imm);
      | OLoc.register =>
        mvar := t.vstack[stackp].mvar;                  (* temporarily save away *)
        t.vstack[stackp].mvar.mvar_type := Type.Word32; (* temporarily make it register sized *)
        t.vstack[stackp].optype := Type.Word32;
        FOR i := 0 TO size - 1 DO
          t.reguse[op.reg[i]].stackp := -1;
          t.reguse[op.reg[i]].operandPart := -1;
          t.vstack[stackp].mvar.mvar_offset := i * 4;   (* temporary offset to register sized chunk *)
          t.cg.movOp(t.vstack[stackp], t.cg.reg[op.reg[i]]);
        END;
        t.vstack[stackp].optype := op.optype;
        t.vstack[stackp].mvar := mvar;                  (* restore *)
      ELSE
        <* ASSERT FALSE *>
    END
  END get_temp;

PROCEDURE sweep (t: T; READONLY mvar: MVar) =
(* I do not understand the point of this function.
 * It searches the virtual stack looking for mvar and
 * anything it finds it moves to new machine stack temporaries.
 * It is only called from pop. Perhaps the point
 * is a sort of copy-on-write?
 *)
  VAR doneswap := FALSE;
      mvarA: ARRAY OperandPart OF MVar;
      size: OperandSize := 1;
      size2: OperandSize;
      stackOpA: ARRAY OperandPart OF Operand;
      type := Type.Word32;
  BEGIN
    FOR i := 0 TO t.stacktop - 1 DO
      IF t.vstack[i].loc = OLoc.mem AND
         t.vstack[i].mvar = mvar THEN
        IF NOT doneswap THEN
          doneswap := TRUE;
          size := SplitMVar(mvar, mvarA);
          t.cg.pushOp(t.cg.reg[EAX]);
          IF size = 2 THEN
            t.cg.pushOp(t.cg.reg[ECX]);
            type := Type.Word64;
          END;
        END;
        size2 := SplitOperand(t.vstack[i], stackOpA);
        <* ASSERT size = size2 *>
        IF size = 1 THEN
          t.cg.movOp(t.cg.reg[EAX], t.vstack[i]);
        ELSE
          t.cg.movOp(t.cg.reg[EAX], stackOpA[0]);
          t.cg.movOp(t.cg.reg[ECX], stackOpA[1]);
        END;
        set_mvar(t, i, MVar { var := t.parent.declare_temp(size * 4, 4, type, FALSE),
                              mvar_offset := 0, mvar_type := type } );
        t.vstack[i].mvar.var.stack_temp := TRUE;
        IF size = 1 THEN
          t.cg.movOp(t.vstack[i], t.cg.reg[EAX]);
        ELSE
          t.cg.movOp(stackOpA[0], t.cg.reg[EAX]);
          t.cg.movOp(stackOpA[1], t.cg.reg[ECX]);
        END;
      END
    END;
    IF doneswap THEN
      IF size = 2 THEN
        t.cg.popOp(t.cg.reg[ECX]);
      END;
      t.cg.popOp(t.cg.reg[EAX]);
    END
  END sweep;

PROCEDURE set_type (t: T; stackp: CARDINAL; type: Type) =
  BEGIN
    t.vstack[stackp].optype := type;
  END set_type;

PROCEDURE set_reg (t: T; stackp: CARDINAL; r: Regno; operandPart: OperandPart) =
  BEGIN
    t.vstack[stackp].loc := OLoc.register;
    t.vstack[stackp].reg[operandPart] := r;
    t.reguse[r].stackp := stackp;
    t.reguse[r].operandPart := operandPart;
    t.parent.proc_reguse[r] := TRUE;
  END set_reg;

PROCEDURE dealloc_reg (t: T; stackp: CARDINAL; operandPart: OperandPart) =
  BEGIN
    <* ASSERT t.vstack[stackp].loc = OLoc.register *>
    t.reguse[t.vstack[stackp].reg[operandPart]].stackp := -1;
    t.reguse[t.vstack[stackp].reg[operandPart]].operandPart := -1;
  END dealloc_reg;

PROCEDURE corrupt (t: T; reg: Regno; operandPart: OperandPart) =
  BEGIN
    IF t.reguse[reg].stackp # -1 THEN
      forceout(t, reg, operandPart);
    END;
    t.parent.proc_reguse[reg] := TRUE;
    t.reguse[reg] := InitRegister(locked := t.reguse[reg].locked);
  END corrupt;

PROCEDURE all_to_mem(t: T) =
  BEGIN
    FOR i := 0 TO t.stacktop - 1 DO
      IF t.vstack[i].loc = OLoc.register THEN
        find(t, i, Force.mem);
      END;
    END;
  END all_to_mem;

PROCEDURE set_fstack (t: T; stackp: CARDINAL) =
  BEGIN
    t.vstack[stackp].loc := OLoc.fstack;
  END set_fstack;

PROCEDURE set_mvar (t: T; stackp: CARDINAL; READONLY mvar: MVar) =
  BEGIN
    t.vstack[stackp].loc := OLoc.mem;
    t.vstack[stackp].mvar := mvar;
  END set_mvar;

PROCEDURE set_imm (t: T; stackp: CARDINAL; READONLY imm: TIntN.T) =
  BEGIN
    t.vstack[stackp].loc := OLoc.imm;
    t.vstack[stackp].imm := imm;
  END set_imm;

PROCEDURE get_loc (t: T; stackp: CARDINAL): OLoc =
  BEGIN
    RETURN t.vstack[stackp].loc;
  END get_loc;

PROCEDURE get_op (t: T; stackp: CARDINAL): Operand =
  BEGIN
    RETURN t.vstack[stackp];
  END get_op;

PROCEDURE pos (t: T; depth: CARDINAL; place: TEXT): CARDINAL =
  VAR pos: CARDINAL;
  BEGIN
    pos := t.stacktop - 1 - depth;
    IF pos >= 0 THEN
      RETURN pos;
    ELSE
      Err(t, "Stack underflow in " & place);
    END;
    RETURN -1;<*NOWARN*>
  END pos;

PROCEDURE pushimmT (t: T; imm: TIntN.T; type: Type) =
  BEGIN
    expand_stack(t);

    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.loc := OLoc.imm;
      stack0.imm := imm;
      stack0.stackp := t.stacktop;
      stack0.optype := type;
    END;

    INC(t.stacktop);
  END pushimmT;

PROCEDURE pushimmI (t: T; immI: INTEGER; type: Type) =
  VAR immT: TIntN.T;
  BEGIN
    IF NOT TIntN.FromHostInteger(immI, Target.Integer.bytes, immT) THEN
      Err(t, "pushimmI: unable to convert to target integer");
    END;
    t.pushimmT(immT, type);
  END pushimmI;

PROCEDURE pushnew (t: T; type: MType; force: Force; set := AllRegisters) =
  VAR hintaddr := (type = Type.Addr);
      reg: ARRAY OperandPart OF Regno;
      r: Regno;
      size := GetTypeSize(type);
      any_reg_in_use := FALSE;
  BEGIN
    expand_stack(t);
    FOR i := 0 TO size - 1 DO

      (* Be sure 64 bit values have low in EAX, high in EDX (or ECX, EBX for lock compare exchange). *)

      IF size = 2 AND i = 0 AND force = Force.regset AND set = RegSet{EAX, EDX} THEN
        r := pickreg(t, RegSet{EAX}, hintaddr);
      ELSIF size = 2 AND i = 0 AND force = Force.regset AND set = RegSet{ECX, EBX} THEN
        r := pickreg(t, RegSet{EBX}, hintaddr);
      ELSE
        r := pickreg(t, set, hintaddr);
      END;
      reg[i] := r;
      set := (set - RegSet{r});
      IF t.reguse[r].stackp # -1 THEN
        any_reg_in_use := TRUE;
      END;
    END;
    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.optype := type;
      stack0.stackp := t.stacktop;
      IF FloatType [type] THEN
        stack0.loc := OLoc.fstack;
      ELSE
        IF force = Force.mem OR (any_reg_in_use AND force = Force.any) THEN
          set_mvar(t, t.stacktop,
                   MVar { var := t.parent.declare_temp(CG_Bytes[type],
                                                       CG_Align_bytes[type],
                                                       type, FALSE),
                          mvar_offset := 0, mvar_type := type } );
          stack0.mvar.var.stack_temp := TRUE;
        ELSE
          FOR i := 0 TO size - 1 DO
            corrupt(t, reg[i], operandPart := i);
            set_reg(t, t.stacktop, reg[i], operandPart := i);
          END;
        END
      END
    END;
    INC(t.stacktop);
  END pushnew;

PROCEDURE push (t: T; READONLY src_mvar: MVar) =
  VAR indreg: Regno;
      destreg: ARRAY OperandPart OF Regno;
      srcSize := GetTypeSize(src_mvar.mvar_type);
  BEGIN
    expand_stack(t);

    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.stackp := t.stacktop;
      stack0.optype := src_mvar.mvar_type;
      IF FloatType [src_mvar.mvar_type] THEN
        IF src_mvar.var.loc = VLoc.temp AND src_mvar.var.parent # t.current_proc THEN
          unlock(t);
          indreg := pickreg(t, AllRegisters, TRUE);
          corrupt(t, indreg, operandPart := 0);

          t.cg.get_frame(indreg, src_mvar.var.parent, t.current_proc);
          t.cg.f_loadind(t.cg.reg[indreg], src_mvar.mvar_offset + src_mvar.var.offset, src_mvar.mvar_type);
          stack0.loc := OLoc.fstack;
        ELSE
          stack0.loc := OLoc.fstack;
          t.cg.fstack_push(src_mvar);
        END
      ELSE
        IF src_mvar.var.loc = VLoc.temp AND src_mvar.var.parent # t.current_proc THEN
          unlock(t);
          FOR i := 0 TO srcSize - 1 DO
            IF CG_Bytes[src_mvar.mvar_type] = 1 THEN
              <* ASSERT srcSize = 1 AND i = 0 *>
              destreg[i] := pickreg(t, RegistersForByteOperations);
            ELSE
              destreg[i] := pickreg(t, AllRegisters, src_mvar.mvar_type = Type.Addr);
            END;
            corrupt(t, destreg[i], operandPart := i);
            t.reguse[destreg[i]].locked := TRUE;
          END;

          indreg := pickreg(t, AllRegisters, TRUE);
          corrupt(t, indreg, operandPart := 0);

          t.cg.get_frame(indreg, src_mvar.var.parent, t.current_proc);
          FOR i := 0 TO srcSize - 1 DO
            t.cg.load_ind(destreg[i], t.cg.reg[indreg], src_mvar.mvar_offset + src_mvar.var.offset + i * 4,
                          src_mvar.mvar_type);
            set_reg(t, t.stacktop, destreg[i], operandPart := i);
          END;
          newdest(t, stack0);
        ELSE
          stack0.loc := OLoc.mem;
          stack0.mvar := src_mvar;
        END
      END
    END;

    INC(t.stacktop);
  END push;

PROCEDURE pop (t: T; READONLY dest_mvar: MVar) =
  VAR indreg: Regno;
      dest_mvarA: ARRAY OperandPart OF MVar;
      destSize := SplitMVar(dest_mvar, dest_mvarA);
      src_opA: ARRAY OperandPart OF Operand;
      srcSize: OperandSize := 1;
  BEGIN
    IF t.stacktop < 1 THEN
      Err(t, "Stack underflow in pop");
    END;

    WITH src_stack0 = t.vstack[t.stacktop - 1] DO
      IF src_stack0.loc = OLoc.fstack THEN
        IF dest_mvar.var.loc = VLoc.temp AND dest_mvar.var.parent # t.current_proc THEN
          unlock(t);
          indreg := pickreg(t, AllRegisters, TRUE);
          corrupt(t, indreg, operandPart := 0);
          t.cg.get_frame(indreg, dest_mvar.var.parent, t.current_proc);
          t.cg.f_storeind(t.cg.reg[indreg], dest_mvar.mvar_offset + dest_mvar.var.offset, dest_mvar.mvar_type);

        ELSE
          t.cg.fstack_pop(dest_mvar);
        END
      ELSE
        unlock(t);
        IF CG_Bytes[dest_mvar.mvar_type] = 1 AND src_stack0.loc # OLoc.imm THEN
          find(t, t.stacktop - 1, Force.regset, RegSet { EAX, EBX, ECX, EDX } );
        ELSE
          find(t, t.stacktop - 1, Force.anyregimm);
        END;

        srcSize := SplitOperand(src_stack0, src_opA);
        IF srcSize # destSize THEN
          Err(t, " srcSize:" & Fmt.Int(srcSize)
              & " destSize:" & Fmt.Int(destSize)
              & " dest_mvar.mvar_type:" & Target.TypeNames[dest_mvar.mvar_type]
              & " src_stack0.optype:" & Target.TypeNames[src_stack0.optype]);
        END;

        IF dest_mvar.var.loc = VLoc.temp AND dest_mvar.var.parent # t.current_proc THEN
          indreg := pickreg(t, AllRegisters, TRUE);
          corrupt(t, indreg, operandPart := 0);
          t.cg.get_frame(indreg, dest_mvar.var.parent, t.current_proc);
          FOR i := 0 TO destSize - 1 DO
            t.cg.store_ind(src_opA[i],
                           t.cg.reg[indreg],
                           dest_mvarA[i].mvar_offset + dest_mvarA[i].var.offset,
                           dest_mvarA[i].mvar_type);
            t.reguse[src_stack0.reg[i]].stackp := -1;
            t.reguse[src_stack0.reg[i]].operandPart := -1;
            corrupt(t, src_stack0.reg[i], i);
          END;

        ELSE
          sweep(t, dest_mvar);

          FOR i := 0 TO NRegs DO
            IF t.reguse[i].last_store = dest_mvar
                OR t.reguse[i].last_store = dest_mvarA[0]
                OR t.reguse[i].last_store = dest_mvarA[destSize - 1] THEN
              t.reguse[i].last_store := NoStore;
            END;
          END;

          IF src_stack0.loc = OLoc.register THEN
            FOR i := 0 TO destSize - 1 DO
              t.reguse[src_stack0.reg[i]].stackp := -1;
              t.reguse[src_stack0.reg[i]].operandPart := -1;
              t.reguse[src_stack0.reg[i]].last_store := dest_mvarA[i];
            END;
          END;

          FOR i := 0 TO destSize - 1 DO
            t.cg.movOp(Operand { loc := OLoc.mem, mvar := dest_mvarA[i], optype := dest_mvarA[i].mvar_type }, src_opA[i]);
          END;
          set_mvar(t, t.stacktop - 1, dest_mvar);
        END
      END
    END;

    DEC(t.stacktop);
  END pop;

PROCEDURE doloadaddress (t: T; v: x86Var; o: ByteOffset) =
  VAR to, tvoffset, ti: TIntN.T;
  BEGIN
    unlock(t);
    pushnew(t, Type.Addr, Force.anyreg);

    WITH stop0 = t.vstack[pos(t, 0, "doloadaddress")] DO
      IF v.loc = VLoc.temp AND v.parent # t.current_proc THEN
        t.cg.get_frame(stop0.reg[0], v.parent, t.current_proc);
        IF NOT TIntN.FromHostInteger(o, Target.Integer.bytes, to) THEN
          Err(t, "doloadaddress: unable to convert o");
        END;
        IF NOT TIntN.FromHostInteger(v.offset, Target.Integer.bytes, tvoffset) THEN
          Err(t, "doloadaddress: unable to convert v.offset");
        END;
        IF NOT TIntN.Add(to, tvoffset, ti) THEN
          Err(t, "dloadaddress: Add overflowed");
        END;
        t.cg.immOp(Op.oADD, t.cg.reg[stop0.reg[0]], ti);

      ELSE
        t.cg.binOp(Op.oLEA, stop0, Operand {loc := OLoc.mem, optype := Type.Word32,
                                            mvar := MVar {var := v, mvar_offset := o,
                                                          mvar_type := Type.Word32} } );
      END
    END
  END doloadaddress;

PROCEDURE findbin (t: T; symmetric, overwritesdest: BOOLEAN;
                   VAR dest, src: INTEGER): BOOLEAN =
  VAR reversed := FALSE;
  BEGIN
    WITH stack0 = pos(t, 0, "findbin"),
         stack1 = pos(t, 1, "findbin") DO

      find(t, stack0, Force.any);
      find(t, stack1, Force.any);

      WITH stop0 = t.vstack[stack0],
           stop1 = t.vstack[stack1] DO

        IF GetTypeSize(stop0.optype) # GetTypeSize(stop1.optype) THEN
          Err(t, "findbin: stop0.optype:" & Target.TypeNames[stop0.optype] & " stop1.optype:" & Target.TypeNames[stop1.optype]);
         END;

        IF symmetric THEN
          IF stop0.loc = OLoc.register OR stop1.loc = OLoc.imm OR
             (stop0.loc = OLoc.mem AND stop0.mvar.var.stack_temp AND
              stop1.loc # OLoc.register) THEN
            dest := stack0;
            src := stack1;
            reversed := TRUE;
          ELSE
            dest := stack1;
            src := stack0;
          END
        ELSE
          dest := stack1;
          src := stack0;
        END
      END
    END;

    WITH destop = t.vstack[dest],
         srcop = t.vstack[src] DO

      <* ASSERT GetTypeSize(destop.optype) = GetTypeSize(srcop.optype) *>

      IF destop.loc = OLoc.mem AND NOT destop.mvar.var.stack_temp AND overwritesdest THEN
        find(t, dest, Force.anyreg);
      END;

      IF destop.loc = OLoc.imm THEN
        find(t, dest, Force.anyreg);
      END;

      IF destop.loc = OLoc.mem AND (CG_Bytes[destop.mvar.mvar_type] < 4 OR srcop.loc = OLoc.mem) THEN
        find(t, dest, Force.anyreg);
      END;

      IF srcop.loc = OLoc.mem AND CG_Bytes[srcop.mvar.mvar_type] < 4 THEN
        find(t, src, Force.anyreg);
      END
    END;

    RETURN reversed;
  END findbin;

PROCEDURE dobin (t: T; op: Op; symmetric, overwritesdest: BOOLEAN; type: Type): BOOLEAN =
  VAR src, dest: INTEGER;
      reversed: BOOLEAN;
      size: OperandSize;
      srcA: ARRAY OperandPart OF Operand;
      destA: ARRAY OperandPart OF Operand;
  BEGIN

    reversed := findbin(t, symmetric, overwritesdest, dest, src);
    <* ASSERT reversed = (dest > src) *>

    WITH destop = t.vstack[dest],
          srcop = t.vstack[src] DO

      IF (GetTypeSize(destop.optype) # GetTypeSize(srcop.optype))
         OR (GetTypeSize(destop.optype) # GetTypeSize(type)) THEN
        Err(t, " GetTypeSize(destop.optype):" & Fmt.Int(GetTypeSize(destop.optype))
            & " GetTypeSize(srcop.optype):" & Fmt.Int(GetTypeSize(srcop.optype))
            & " GetTypeSize(type):" & Fmt.Int(GetTypeSize(type))
            & " destop.optype:" & Target.TypeNames[destop.optype]
            & " srcop.optype:" & Target.TypeNames[srcop.optype]
            & " type:" & Target.TypeNames[type]);
      END;

      size := SplitOperand(srcop, srcA);
      EVAL SplitOperand(destop, destA);

      t.cg.binOp(op, destop, srcop);

      IF overwritesdest THEN
        newdest(t, destop);
        IF reversed THEN
          swap(t);
        END;
        discard(t, 1);
      ELSE
        discard(t, 2);
      END
    END;

    RETURN reversed;
  END dobin;

PROCEDURE dostoreind (t: T; o: ByteOffset; type: MType) =
  BEGIN
    WITH stack0 = pos(t, 0, "store_indirect"),
         stack1 = pos(t, 1, "store_indirect") DO
      find(t, stack1, Force.any, AllRegisters, TRUE);
      IF CG_Bytes[type] = 1 AND t.vstack[stack0].loc # OLoc.imm THEN
        find(t, stack0, Force.regset, RegistersForByteOperations);
      ELSE
        find(t, stack0, Force.anyregimm);
      END;

      IF t.vstack[stack1].loc # OLoc.register THEN
        find(t, stack1, Force.anyreg, AllRegisters, TRUE);
      END;

      t.cg.store_ind(t.vstack[stack0], t.vstack[stack1], o, type);
    END;

    discard(t, 2);
  END dostoreind;

PROCEDURE doumul (t: T) =
  VAR otherop: Operand;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doumul"),
         stack1 = pos(t, 1, "doumul") DO
      WITH stop0 = t.vstack[stack0],
           stop1 = t.vstack[stack1] DO
        IF stop0.loc = OLoc.register AND stop0.reg[0] = EAX THEN
          lock(t, EAX);
          find(t, stack1, Force.anydword);
          otherop := stop1;
        ELSIF stop1.loc = OLoc.register AND stop1.reg[0] = EAX THEN
          lock(t, EAX);
          find(t, stack0, Force.anydword);
          otherop := stop0;
        ELSIF stop0.loc = OLoc.register THEN
          find(t, stack0, Force.regset, RegSet {EAX});
          find(t, stack1, Force.anydword);
          otherop := stop1;
        ELSE
          find(t, stack1, Force.regset, RegSet {EAX});
          find(t, stack0, Force.anydword);
          otherop := stop0;
        END
      END;

      IF otherop.loc = OLoc.imm THEN
        IF otherop = t.vstack[stack1] THEN
          find(t, stack1, Force.anyreg);
          otherop := t.vstack[stack1];
        ELSE
          find(t, stack0, Force.anyreg);
          otherop := t.vstack[stack0];
        END
      END;

      IF otherop.loc # OLoc.register OR otherop.reg[0] # EDX THEN
        corrupt(t, EDX, operandPart := 0);
      END;

      t.cg.mulOp(otherop);
      IF otherop = t.vstack[stack1] THEN
        swap(t);
      END;

      newdest(t, t.cg.reg[EDX]);
      newdest(t, t.cg.reg[EAX]);
      discard(t, 1);
    END
  END doumul;

PROCEDURE doimul (t: T) =
  VAR dest, src: Operand;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doimul"),
         stack1 = pos(t, 1, "doimul") DO
      WITH stop0 = t.vstack[stack0],
           stop1 = t.vstack[stack1] DO
        find(t, stack1, Force.any);
        IF stop1.loc = OLoc.register THEN
          find(t, stack0, Force.anydword);
          dest := stop1;
          src := stop0;
        ELSE
          find(t, stack0, Force.anyreg);
          find(t, stack1, Force.anydword);
          dest := stop0;
          src := stop1;
          swap(t);
        END
      END;

      t.cg.imulOp(dest, src);
      newdest(t, dest);
      discard(t, 1);
    END
  END doimul;

PROCEDURE dodiv (t: T; a, b: Sign) =
  VAR neglabel: Label;
  BEGIN
    unlock(t);

    corrupt(t, EDX, operandPart := 0);
    lock(t, EDX);

    WITH stack0 = pos(t, 0, "dodiv"),
         stack1 = pos(t, 1, "dodiv") DO
      find(t, stack1, Force.regset, RegSet {EAX});

      IF a # Sign.Unknown AND b # Sign.Unknown THEN
        find(t, stack0, Force.anydword);
        IF t.vstack[stack0].loc = OLoc.imm THEN
          find(t, stack0, Force.anyreg);
        END;

        IF a = Sign.Positive THEN
          t.cg.binOp(Op.oXOR, t.cg.reg[EDX], t.cg.reg[EDX]);
        ELSE
          t.cg.noargOp(Op.oCDQ);
        END;

        IF a = Sign.Positive AND b = Sign.Positive THEN
          t.cg.divOp(t.vstack[stack0]);
        ELSE
          t.cg.idivOp(t.vstack[stack0]);
        END;

        IF (a = Sign.Positive AND b = Sign.Negative) OR
           (a = Sign.Negative AND b = Sign.Positive) THEN
          t.cg.immOp(Op.oCMP, t.cg.reg[EDX], TZero);

          neglabel := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.E, neglabel);
          t.cg.decOp(t.cg.reg[EAX]);

          t.cg.set_label(neglabel);
        END
      ELSE
        find(t, stack0, Force.anyreg);
        t.cg.diffdivOp(t.vstack[stack0], a = Sign.Positive);
      END;

      newdest(t, t.vstack[stack1]);
      discard(t, 1);
    END
  END dodiv;

PROCEDURE domod (t: T; a, b: Sign) =
  VAR neglabel: Label;
  BEGIN
    unlock(t);

    corrupt(t, EDX, operandPart := 0);
    lock(t, EDX);

    WITH stack0 = pos(t, 0, "domod"),
         stack1 = pos(t, 1, "domod"),
         stop0 = t.vstack[stack0],
         stop1 = t.vstack[stack1] DO

      <* ASSERT GetTypeSize(stop0.optype) = GetTypeSize(stop1.optype) *>

      find(t, stack1, Force.regset, RegSet {EAX});
      IF (a = Sign.Positive AND b = Sign.Positive) OR
         (a = Sign.Negative AND b = Sign.Negative) THEN
        find(t, stack0, Force.anydword);
        IF t.vstack[stack0].loc = OLoc.imm THEN
          find(t, stack0, Force.anyreg);
        END;
      ELSE
        find(t, stack0, Force.anyreg);
      END;

      IF a # Sign.Unknown AND b # Sign.Unknown THEN
        IF a = Sign.Positive THEN
          t.cg.binOp(Op.oXOR, t.cg.reg[EDX], t.cg.reg[EDX]);
        ELSE
          t.cg.noargOp(Op.oCDQ);
        END;

        IF a = Sign.Positive AND b = Sign.Positive THEN
          t.cg.divOp(t.vstack[stack0]);
        ELSE
          t.cg.idivOp(t.vstack[stack0]);
        END;

        IF (a = Sign.Positive AND b = Sign.Negative) OR
           (a = Sign.Negative AND b = Sign.Positive) THEN
          t.cg.immOp(Op.oCMP, t.cg.reg[EDX], TZero);

          neglabel := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.E, neglabel);
          t.cg.binOp(Op.oADD, t.cg.reg[EDX], t.vstack[stack0]);

          t.cg.set_label(neglabel);
        END
      ELSE
        t.cg.diffmodOp(t.vstack[stack0], a = Sign.Positive);
      END;

      newdest(t, t.vstack[stack1]);
      dealloc_reg(t, stack1, operandPart := 0);
      set_reg(t, stack1, EDX, operandPart := 0);
      discard(t, 1);
    END
  END domod;

PROCEDURE doimm (t: T; op: Op; READONLY imm: TIntN.T; overwritesdest: BOOLEAN) =
  BEGIN
    unlock(t);

    WITH stack0 = pos(t, 0, "doimm"),
         stop0 = t.vstack[stack0] DO
      IF (stop0.loc = OLoc.mem AND
         ((overwritesdest AND NOT stop0.mvar.var.stack_temp) OR
          CG_Bytes[stop0.mvar.mvar_type] = 2 OR
          (CG_Bytes[stop0.mvar.mvar_type] = 1 AND (TIntN.GT(imm, TIntN.Max8) OR TIntN.LT(imm, TIntN.Min8)))))
         OR stop0.loc = OLoc.imm THEN
        find(t, stack0, Force.anyreg);
      ELSE
        find(t, stack0, Force.any);
      END;

      t.cg.immOp(op, stop0, imm);

      IF overwritesdest THEN
        newdest(t, stop0);
      ELSE
        discard(t, 1);
      END
    END
  END doimm;

PROCEDURE doneg (t: T; stack_position := 0) =
  VAR neg: TIntN.T;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, stack_position, "doneg"),
         stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.imm THEN
        IF NOT TIntN.Negate(stop0.imm, neg) THEN
          Err(t, "doneg: Negate overflowed");
        END;
        stop0.imm := neg;
      ELSE
        find(t, stack0, Force.anytemp);
        t.cg.unOp(Op.oNEG, stop0);

        newdest(t, stop0);
      END
    END
  END doneg;

PROCEDURE doabs (t: T) =
  VAR lab: Label;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doabs"),
         stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.imm THEN
        IF NOT TIntN.Abs(stop0.imm, stop0.imm) THEN
          Err(t, "doabs: Abs overflowed");
        END;
      ELSE
        find(t, stack0, Force.anytemp);

        IF (stop0.loc = OLoc.mem) OR (GetOperandSize(stop0) > 1) THEN
          t.cg.immOp(Op.oCMP, stop0, TZero);

          lab := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.GE, lab);
          t.cg.unOp(Op.oNEG, stop0);

          t.cg.set_label(lab);
        ELSE
          t.cg.unOp(Op.oNEG, stop0);

          lab := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.G, lab);
          t.cg.unOp(Op.oNEG, stop0);

          t.cg.set_label(lab);
        END;

        newdest(t, stop0);
      END
    END
  END doabs;

PROCEDURE doshift (t: T; type: IType; shiftType: ShiftType): BOOLEAN =
  VAR ovflshift, leftlab, endlab: Label;
      tShiftCount: TIntN.T;
      shiftResult: TIntN.T;
      shiftCount: INTEGER;
      typeBitSize := TIntN.T{x := Target.Int{IntType[type].size, 0, ..}};
  BEGIN

    unlock(t);
    WITH stack0 = pos(t, 0, "doshift"),
         stack1 = pos(t, 1, "doshift"),
         stop0 = t.vstack[stack0],
         stop1 = t.vstack[stack1] DO

      <* ASSERT TypeIs64(type) = TypeIs64(stop1.optype) *>

      IF stop0.loc = OLoc.imm AND TWordN.LT(stop0.imm, TIntN.SixtyFour) THEN
        IF stop1.loc = OLoc.imm THEN
          IF NOT TIntN.ToHostInteger(stop0.imm, shiftCount) THEN
            Err(t, "doshift: unable to convert target integer to host integer");
          END;

          (* shift constant by a constant *)

          CASE shiftType OF
            | ShiftType.UnboundedPositiveIsLeft =>
                TWordN.Shift(stop1.imm, shiftCount, shiftResult);
            | ShiftType.LeftAlreadyBounded =>
                TWordN.LeftShift(stop1.imm, shiftCount, shiftResult);
            | ShiftType.RightAlreadyBounded =>
                TWordN.RightShift(stop1.imm, shiftCount, shiftResult);
          END;

          stop1.imm := shiftResult;
        ELSE

          (* shift non-constant by a constant *)
          (* NOTE: binOp/immOp know how to do double precision shifts *)

          IF TIntN.NE(stop0.imm, TZero) THEN

            (* shift non-constant by a non-zero constant *)

            find(t, stack1, Force.anytemp);

            CASE shiftType OF
              | ShiftType.UnboundedPositiveIsLeft =>
                IF     TIntN.GT(stop0.imm, MaximumShift[type])
                    OR TIntN.LT(stop0.imm, MinimumShift[type]) THEN
                  t.cg.binOp(Op.oXOR, stop1, stop1);        (* shifting "too far" just yields zero *)
                ELSIF TIntN.GT(stop0.imm, TZero) THEN
                  t.cg.immOp(Op.oSHL, stop1, stop0.imm);    (* positive shift is left shift *)
                ELSE
                  IF NOT TIntN.Negate(stop0.imm, tShiftCount) THEN
                    Err(t, "doshift: Negate overflowed");
                  END;
                  t.cg.immOp(Op.oSHR, stop1, tShiftCount);  (* negative shift is right shift *)
                END;
              | ShiftType.LeftAlreadyBounded,
                 ShiftType.RightAlreadyBounded =>
                  TWordN.And(stop0.imm, MaximumShift[type], tShiftCount);
                  stop0.imm := tShiftCount;
                  IF shiftType = ShiftType.LeftAlreadyBounded THEN
                    t.cg.immOp(Op.oSHL, stop1, stop0.imm);
                  ELSE
                    t.cg.immOp(Op.oSHR, stop1, stop0.imm);
                  END;
            END;

            newdest(t, stop1);
          END
        END
      ELSE

        IF ((stop1.loc # OLoc.imm) OR (TIntN.NE(stop1.imm, TZero))) THEN

          (* shift by a non-constant
           * NOTE: binOp/immOp know how to do double precision compares/xor/shift
           *)

          find(t, stack0, Force.regset, RegSet {ECX});
          find(t, stack1, Force.anytemp);
          IF stop1.loc = OLoc.imm THEN
            find(t, stack1, Force.anyreg);
          END;

          CASE shiftType OF
            | ShiftType.UnboundedPositiveIsLeft =>
              t.cg.immOp(Op.oCMP, stop0, TZero);

              leftlab := t.cg.reserve_labels(1, TRUE);
              ovflshift := t.cg.reserve_labels(1, TRUE);
              endlab := t.cg.reserve_labels(1, TRUE);

              t.cg.brOp(Cond.GE, leftlab);
              t.cg.unOp(Op.oNEG, stop0);
              t.cg.immOp(Op.oCMP, stop0, typeBitSize);
              t.cg.brOp(Cond.GE, ovflshift);
              t.cg.unOp(Op.oSHR, stop1);
              t.cg.brOp(Cond.Always, endlab);
              t.cg.set_label(ovflshift);
              (* .ovflshift *)
              t.cg.binOp(Op.oXOR, stop1, stop1);
              t.cg.brOp(Cond.Always, endlab);
              t.cg.set_label(leftlab);
              (* .leftlab *)
              t.cg.immOp(Op.oCMP, stop0, typeBitSize);
              t.cg.brOp(Cond.GE, ovflshift);
              t.cg.unOp(Op.oSHL, stop1);
              t.cg.set_label(endlab);
              (* .endlab  *)

            | ShiftType.LeftAlreadyBounded => t.cg.unOp(Op.oSHL, stop1); (* shift count in ECX *)
            | ShiftType.RightAlreadyBounded => t.cg.unOp(Op.oSHR, stop1); (* shift count in ECX *)
          END;

          newdest(t, stop1);
          newdest(t, stop0);
        END;
      END;

      discard(t, 1);
    END;

    RETURN TRUE;
  END doshift;

PROCEDURE dorotate (t: T; type: IType): BOOLEAN =
  VAR leftlab, endlab: Label;
      rotateCount: INTEGER;
      is64 := TypeIs64(type);
  BEGIN

    unlock(t);
    WITH stack0 = pos(t, 0, "dorotate"),
         stack1 = pos(t, 1, "dorotate"),
         stop0 = t.vstack[stack0],
         stop1 = t.vstack[stack1] DO

      IF stop0.loc = OLoc.imm THEN
        IF stop1.loc = OLoc.imm THEN
          IF NOT TIntN.ToHostInteger(stop0.imm, rotateCount) THEN
            Err(t, "dorotate: failed to convert rotateCount to host integer");
          END;
          TWordN.Rotate(stop1.imm, rotateCount, stop1.imm);
        ELSE
          IF TIntN.NE(stop0.imm, TZero) THEN

            IF is64 THEN (* needs work to be more efficient, but ok *)
              RETURN FALSE;
            END;

            find(t, stack1, Force.anytemp);

            IF TIntN.GT(stop0.imm, TZero) THEN
              TWordN.And(stop0.imm, BitCountMask[type], stop0.imm);
              t.cg.immOp(Op.oROL, stop1, stop0.imm);
            ELSE
              IF NOT TIntN.Negate(stop0.imm, stop0.imm) THEN
                Err(t, "dorotate: negate overflowed");
              END;
              TWordN.And(stop0.imm, BitCountMask[type], stop0.imm);
              t.cg.immOp(Op.oROR, stop1, stop0.imm);
            END;

            newdest(t, stop1);
          END
        END
      ELSE

        IF ((stop0.loc # OLoc.imm) OR (TIntN.NE(stop0.imm, TZero))) THEN

          IF is64 THEN (* needs work to be more efficient, but ok *)
            RETURN FALSE;
          END;

          find(t, stack0, Force.regset, RegSet {ECX});

          find(t, stack1, Force.anytemp);
          IF stop1.loc = OLoc.imm THEN
            find(t, stack1, Force.anyreg);
          END;

          t.cg.immOp(Op.oCMP, stop0, TZero);

          leftlab := t.cg.reserve_labels(1, TRUE);
          endlab := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.GE, leftlab);
          t.cg.unOp(Op.oNEG, stop0);
          t.cg.unOp(Op.oROR, stop1);
          t.cg.brOp(Cond.Always, endlab);
          t.cg.set_label(leftlab);
          (* .leftlab *)
          t.cg.unOp(Op.oROL, stop1);
          t.cg.set_label(endlab);
          (* .endlab  *)
        END;

        newdest(t, stop1);
        newdest(t, stop0);
      END;

      discard(t, 1);
    END;
    RETURN TRUE;
  END dorotate;

PROCEDURE doextract (t: T; type: IType; sign_extend: BOOLEAN) =
(*
 *  T extract(T value, UINT32 offset, UINT32 count) 
 *  {
 *      return ((value >> offset) & ~((~(T)0) << count));
 *  }
 *  for T = UINT32 or UINT64
 *
 * in this order, so that we can pop "count" first:
 *
 *  T extract(T value, UINT32 offset, UINT32 count) 
 *  {
 *      T mask = ((~(T)0) << count);
 *      value >>= offset;
 *      value &= ~mask;
 *      return value;
 *  }
 *  for T = UINT32 or UINT64
 *
 *)
  VAR count_value: INTEGER;
      utype := UnsignedType[type];
      mask, count, offset, value: CARDINAL;
      get_mask := FALSE;
      get_offset := TRUE;
      get_count := TRUE;

  (* There is quite a dance here, in order to discard stack items
   * as we finish with them, in order to avoid preserving dead values (count).
   * There should be an easier way but I couldn't find it and this
   * at least clearly "plays by the rules".
   *)

  PROCEDURE GetOperands() =
    VAR n: CARDINAL := 0;
    BEGIN
      IF get_mask THEN 
        mask := pos(t, 0, "extract");
        INC(n);
      END;
      IF get_count THEN
        count := pos(t, n, "extract");
        INC(n);
      END;
      IF get_offset THEN
        offset := pos(t, n, "extract");
        INC(n);
      END;
      value := pos(t, n, "extract");
    END GetOperands;

  BEGIN

    (* See if count is a constant, in which case
     * call the more optimal doextract_n.
     *)

    unlock(t);
    GetOperands();

    IF sign_extend AND t.vstack[count].loc # OLoc.imm THEN
      Err(t, "doextract: sign_extend requires constant offset/count");
    END;

    IF t.vstack[count].loc = OLoc.imm THEN
      IF NOT TIntN.ToHostInteger(t.vstack[count].imm, count_value) THEN
        Err(t, "doextract: failed to convert to host integer");
      END;
      discard(t, 1);
      doextract_n(t, type, sign_extend, count_value);
      RETURN;
    END;

    (* Push the mask on the virtual stack,
     * with an initial value of ~0.
     * This could be done using lower level
     * primitives, but this works just as well
     * and is easier to code.
     *)

    t.pushimmT(TIntN.T{x := IntType[utype].max}, utype);
    get_mask := TRUE;
    unlock(t);
    GetOperands();

    (* Get count into ECX if it is not immediate. *)

    IF t.vstack[count].loc = OLoc.imm THEN
      (* Mask count to 31 or 63 -- should be redundant but is safe. *)
      TWordN.And(t.vstack[count].imm, BitCountMask[type], t.vstack[count].imm);
    ELSE
      find(t, count, Force.regset, RegSet{ECX});
    END;

    (* Get mask into registers. Do this after count possibly takes ECX,
     * so that mask doesn't needlessly do so and then we'd swap them.
     *)

    find(t, mask, Force.anyreg);

    (* Shift mask left by count. *)

    IF t.vstack[count].loc = OLoc.imm THEN
      t.cg.immOp(Op.oSHL, t.vstack[mask], t.vstack[count].imm);
    ELSE
      t.cg.unOp(Op.oSHL, t.vstack[mask]); (* shift by ECX *)
    END;

    (* done with count *)

    newdest(t, t.vstack[count]); (* Is this needed? *)
    swap(t);
    discard(t, 1);
    unlock(t);
    get_count := FALSE;
    GetOperands();

    (* Get offset into ECX if is not immediate. *)

    IF t.vstack[offset].loc = OLoc.imm THEN
      (* Mask count to 31 or 63 -- should be redundant but is safe. *)
      TWordN.And(t.vstack[offset].imm, BitCountMask[type], t.vstack[offset].imm);
    ELSE
      find(t, offset, Force.regset, RegSet{ECX});
    END;

    (* Get value into registers. Do this after offset
     * so that value doesn't needlessly go into ECX
     * and have to be swapped.
     *)

    find(t, value, Force.anyreg);

    (* Shift value right by offset. *)

    IF t.vstack[offset].loc = OLoc.imm THEN
      t.cg.immOp(Op.oSHR, t.vstack[value], t.vstack[offset].imm);
    ELSE
      t.cg.unOp(Op.oSHR, t.vstack[value]); (* shift by ECX *)
    END;

    (* done with offset *)

    newdest(t, t.vstack[offset]); (* Is this needed? *)
    swap(t);
    discard(t, 1);
    unlock(t);
    get_offset := FALSE;
    GetOperands();

    (* Ensure mask and value are in registers. *)

    find(t, mask, Force.anyreg);
    find(t, value, Force.anyreg);

    t.cg.unOp(Op.oNOT, t.vstack[mask]);
    t.cg.binOp(Op.oAND, t.vstack[value], t.vstack[mask]);

    newdest(t, t.vstack[mask]); (* Is this needed? *)
    newdest(t, t.vstack[value]);
    discard(t, 1);

  END doextract;

PROCEDURE doextract_n (t: T; type: IType; sign_extend: BOOLEAN; count: CARDINAL) =
  VAR andval: TIntN.T;
      offset: INTEGER;
      uint_type := IntType[UnsignedType[type]];
      max := TIntN.T{x := uint_type.max};
      typeBitSize := uint_type.size;
  BEGIN

    IF count < 0 THEN
      Err(t, "doextract_n: count must be positive");
    END;

    IF sign_extend AND (count < 1) THEN
      Err(t, "doextract_n: count must at least 1 if sign extending");
    END;

    unlock(t);
    WITH stack_offset = pos(t, 0, "extract_n"),
         stack_value = pos(t, 1, "extract_n"),
         op_offset = t.vstack[stack_offset],
         op_value = t.vstack[stack_value] DO

      IF sign_extend AND op_offset.loc # OLoc.imm THEN
        Err(t, "doextract: sign_extend requires constant offset/count");
      END;

      IF op_offset.loc = OLoc.imm THEN
        IF NOT TIntN.ToHostInteger(op_offset.imm, offset) THEN
          Err(t, "doextract_n: failed to convert to host integer");
        END;
        discard(t, 1);
        doextract_mn(t, type, sign_extend, offset, count);
        RETURN;
      END;

      find(t, stack_offset, Force.regset, RegSet { ECX });
      find(t, stack_value, Force.anyreg);

      t.cg.unOp(Op.oSHR, op_value); (* shift by ECX *)

      IF count < typeBitSize THEN
        TWordN.Shift(max, count - typeBitSize, andval);
        t.cg.immOp(Op.oAND, op_value, andval);
      END;

      newdest(t, op_value);
      discard(t, 1);
    END;
  END doextract_n;

PROCEDURE doextract_mn (t: T; type: IType; sign_extend: BOOLEAN; offset, count: CARDINAL) =
  VAR andval, tint: TIntN.T;
      uint_type := IntType[UnsignedType[type]];
      max := TIntN.T{x := uint_type.max};
      typeBitSize := uint_type.size;
  BEGIN

    IF offset < 0 THEN
      Err(t, "doextract_mn: offset must be positive");
    END;
    IF count < 0 THEN
      Err(t, "doextract_mn: count must be positive");
    END;
    IF sign_extend AND (count < 1) THEN
      Err(t, "doextract_mn: count must at least 1 if sign extending");
    END;

    unlock(t);
    WITH stack0 = pos(t, 0, "extract_mn"),
         stop0 = t.vstack[stack0] DO

      IF stop0.loc = OLoc.imm THEN
        TWordN.Shift(stop0.imm, -offset, stop0.imm);
        TWordN.Shift(max, count - typeBitSize, tint);
        TWordN.And(stop0.imm, tint, stop0.imm);
        IF sign_extend THEN
          TWordN.Shift(TIntN.One, count - 1, tint);
          TWordN.And(stop0.imm, tint, tint);
          IF TIntN.NE(tint, TZero) THEN
            TWordN.Shift(max, count, tint);
            TWordN.Or(stop0.imm, tint, stop0.imm);
          END;
        END;
        RETURN;
      END;

      IF sign_extend THEN
        find(t, stack0, Force.anyreg);
        IF (offset + count) < typeBitSize THEN
          IF NOT TIntN.FromHostInteger(typeBitSize - (offset + count), Target.Integer.bytes, tint) THEN
            Err(t, "doextract_mn: failed to convert " & Fmt.Int(typeBitSize) & " - (offset + count) to target integer");
          END;
          t.cg.immOp(Op.oSHL, stop0, tint);
        END;

        IF count < typeBitSize THEN
          IF NOT TIntN.FromHostInteger(typeBitSize - count, Target.Integer.bytes, tint) THEN
            Err(t, "doextract_mn: failed to convert " & Fmt.Int(typeBitSize) & " - count to target integer");
          END;
          t.cg.immOp(Op.oSAR, stop0, tint);
        END
      ELSE
        find(t, stack0, Force.anyreg);
        IF (offset + count) < typeBitSize THEN
          TWordN.Shift(max, offset + count - typeBitSize, andval);
          t.cg.immOp(Op.oAND, stop0, andval);
        END;

        IF offset > 0 THEN
          IF NOT TIntN.FromHostInteger(offset, Target.Integer.bytes, tint) THEN
            Err(t, "doextract_mn: failed to offset to target integer");
          END;
          t.cg.immOp(Op.oSHR, stop0, tint);
        END
      END;

      newdest(t, stop0);
    END;
  END doextract_mn;

PROCEDURE doinsert (t: T; type: IType) =
(*
 *  T insert(T to, T from, UINT32 offset, UINT32 count)
 *  {
 *      T mask = ((~((~(T)0) << count)) << offset);
 *      return (to & ~mask) | ((from << offset) & mask);
 *  }
 *  for T = UINT32 or UINT64
 *
 *)
  VAR count_value: INTEGER;
      offset_value: INTEGER;
      utype := UnsignedType[type];
      mask, count, offset, from, to: CARDINAL;
      get_mask := FALSE;
      get_offset := TRUE;
      get_count := TRUE;

  (* There is quite a dance here, in order to discard stack items
   * as we finish with them, in order to avoid preserving dead values (count, offset).
   * There should be an easier way but I couldn't find it and this
   * at least clearly "plays by the rules".
   *)

  PROCEDURE GetOperands() =
    VAR n: CARDINAL := 0;
    BEGIN
      IF get_mask THEN 
        mask := pos(t, 0, "insert");
        INC(n);
      END;
      IF get_count THEN
        count := pos(t, n, "insert");
        INC(n);
      END;
      IF get_offset THEN
        offset := pos(t, n, "insert");
        INC(n);
      END;
      from := pos(t, n, "insert");
      to := pos(t, n + 1, "insert");
    END GetOperands;

  BEGIN

    unlock(t);
    GetOperands();

    (* If offset and count are constant, call the more efficient doinsert_mn.
     * We don't try to call doinsert_n here, because it might just
     * call us back and infinitely recurse.
     *)

    IF t.vstack[count].loc = OLoc.imm AND t.vstack[offset].loc = OLoc.imm THEN
      IF NOT TIntN.ToHostInteger(t.vstack[count].imm, count_value) THEN
        Err(t, "doinsert: failed to convert count to host integer");
      END;
      IF NOT TIntN.ToHostInteger(t.vstack[offset].imm, offset_value) THEN
        Err(t, "doinsert: failed to convert offset to host integer");
      END;
      discard(t, 2);
      doinsert_mn(t, type, offset_value, count_value);
      RETURN;
    END;

    t.pushimmT(TIntN.T{x := IntType[utype].max}, utype);
    get_mask := TRUE;

    unlock(t);
    GetOperands();

    IF t.vstack[count].loc = OLoc.imm THEN
      TWordN.And(t.vstack[count].imm, BitCountMask[type], t.vstack[count].imm); (* shouldn't be needed *)
    ELSE
      find(t, count, Force.regset, RegSet{ECX});
    END;

    find(t, mask, Force.anyreg);

    IF t.vstack[count].loc = OLoc.register THEN
      t.cg.unOp(Op.oSHL, t.vstack[mask]); (* shift by ECX *)
    ELSE
      t.cg.immOp(Op.oSHL, t.vstack[mask], t.vstack[count].imm);
    END;
    t.cg.unOp(Op.oNOT, t.vstack[mask]);

    (* done with count *)

    newdest(t, t.vstack[count]); (* Is this needed? *)
    swap(t);
    discard(t, 1);
    unlock(t);
    get_count := FALSE;
    GetOperands();
      
    IF t.vstack[offset].loc = OLoc.imm THEN
      TWordN.And(t.vstack[offset].imm, BitCountMask[type], t.vstack[offset].imm); (* shouldn't be needed *)
    ELSE
      find(t, offset, Force.regset, RegSet{ECX});
    END;
    find(t, mask, Force.anyreg);
    find(t, from, Force.anyreg);

    IF t.vstack[offset].loc = OLoc.register THEN
      t.cg.unOp(Op.oSHL, t.vstack[mask]); (* shift by ECX *)
      t.cg.unOp(Op.oSHL, t.vstack[from]); (* shift by ECX *)
    ELSE
      t.cg.immOp(Op.oSHL, t.vstack[mask], t.vstack[offset].imm);
      t.cg.immOp(Op.oSHL, t.vstack[from], t.vstack[offset].imm);
    END;

    (* done with offset *)

    newdest(t, t.vstack[offset]); (* Is this needed? *)
    swap(t);
    discard(t, 1);
    unlock(t);
    get_offset := FALSE;
    GetOperands();

    find(t, from, Force.anyreg);
    find(t, mask, Force.anyreg);

    t.cg.binOp(Op.oAND, t.vstack[from], t.vstack[mask]);
    t.cg.unOp(Op.oNOT, t.vstack[mask]);
    find(t, to, Force.anyreg);
    t.cg.binOp(Op.oAND, t.vstack[to], t.vstack[mask]);
    t.cg.binOp(Op.oOR, t.vstack[to], t.vstack[from]);

    newdest(t, t.vstack[to]);
    newdest(t, t.vstack[from]); (* Is this needed? *)
    newdest(t, t.vstack[mask]); (* Is this needed? *)
    discard(t, 2);

  END doinsert;

PROCEDURE doinsert_n (t: T; type: IType; count: CARDINAL) =
  VAR offset: INTEGER;
  BEGIN

    (* If offset is also a constant (count already is),
     * call the more efficient doinsert_mn.
     *)

    WITH stack_offset = pos(t, 0, "insert"),
         op_offset = t.vstack[stack_offset] DO

      IF op_offset.loc = OLoc.imm THEN
        IF NOT TIntN.ToHostInteger(op_offset.imm, offset) THEN
          Err(t, "doinsert_n: failed to convert to host integer");
        END;
        discard(t, 1);
        doinsert_mn(t, type, offset, count);
        RETURN;
      END;
    END;

    (* Just call the general doinsert. *)

    t.pushimmI(count, UnsignedType[type]);
    t.doinsert(type);

  END doinsert_n;
  
PROCEDURE doinsert_mn (t: T; type: IType; offset, count: CARDINAL) =
  VAR tint_m, mask_m, mask_m_n, mask: TIntN.T;
      uint_type := IntType[UnsignedType[type]];
      max := TIntN.T{x := uint_type.max};
      typeBitSize := uint_type.size;
  BEGIN

    unlock(t);
    WITH stack_from = pos(t, 0, "insert"),
         stack_to = pos(t, 1, "insert"),
         op_from = t.vstack[stack_from],
         op_to = t.vstack[stack_to] DO

      find(t, stack_to, Force.any);
      find(t, stack_from, Force.anyregimm);

      IF op_to.loc = OLoc.mem THEN
        find(t, stack_to, Force.anyreg);
      END;

      TWordN.Shift(max, count - typeBitSize, mask);

      IF op_from.loc = OLoc.imm THEN
        TWordN.And(op_from.imm, mask, op_from.imm);
        TWordN.Shift(op_from.imm, offset, op_from.imm);
      ELSE
        IF (count + offset) < typeBitSize THEN
          t.cg.immOp(Op.oAND, op_from, mask);
        END;

        IF offset # 0 THEN
          IF NOT TIntN.FromHostInteger(offset, Target.Integer.bytes, tint_m) THEN
            Err(t, "doinsert_mn: unable to convert offset to target integer");
          END;
          t.cg.immOp(Op.oSHL, op_from, tint_m);
        END
      END;

      TWordN.Shift(max, offset, mask_m);
      TWordN.Shift(max, offset + count - typeBitSize, mask_m_n);
      TWordN.Xor(mask_m, mask_m_n, mask);

      IF TWordN.NE(mask, max) THEN
        IF op_to.loc = OLoc.imm THEN
          TWordN.And(op_to.imm, mask, op_to.imm);
        ELSE
          t.cg.immOp(Op.oAND, op_to, mask);
        END
      END;

      IF op_to.loc = OLoc.imm THEN
        IF op_from.loc = OLoc.imm THEN
          TWordN.Or(op_to.imm, op_from.imm, op_to.imm);
        ELSE
          swap(t);
          IF op_from.loc # OLoc.imm OR TIntN.NE(op_from.imm, TZero) THEN
            t.cg.binOp(Op.oOR, op_to, op_from);
          END
        END
      ELSE
        IF op_from.loc # OLoc.imm OR TIntN.NE(op_from.imm, TZero) THEN
          t.cg.binOp(Op.oOR, op_to, op_from);
        END
      END;

      newdest(t, op_from);
      newdest(t, op_to);
      discard(t, 1);
    END;
  END doinsert_mn;

PROCEDURE swap (t: T) =
  VAR tmp: Operand;
  BEGIN
    WITH stack0 = pos(t, 0, "swap"),
         stack1 = pos(t, 1, "swap") DO

      tmp := t.vstack[stack0];
      t.vstack[stack0] := t.vstack[stack1];
      t.vstack[stack1] := tmp;

      t.vstack[stack0].stackp := stack0;
      t.vstack[stack1].stackp := stack1;

      IF t.vstack[stack0].loc = OLoc.register THEN
        FOR i := 0 TO GetOperandSize(t.vstack[stack0]) - 1 DO
          <* ASSERT t.reguse[t.vstack[stack0].reg[i]].stackp = stack1 *>
          t.reguse[t.vstack[stack0].reg[i]].stackp := stack0;
        END;
      END;

      IF t.vstack[stack1].loc = OLoc.register THEN
        FOR i := 0 TO GetOperandSize(t.vstack[stack1]) - 1 DO
          <* ASSERT t.reguse[t.vstack[stack1].reg[i]].stackp = stack0 *>
          t.reguse[t.vstack[stack1].reg[i]].stackp := stack1;
        END;
      END;

      IF t.vstack[stack0].loc = OLoc.fstack AND
        t.vstack[stack1].loc = OLoc.fstack THEN
        t.cg.fstack_swap();
      END
    END
  END swap;

PROCEDURE doloophole (t: T; from, to: ZType) =
  VAR fromSize := GetTypeSize(from);
      toSize := GetTypeSize(to);
      changeSize := fromSize # toSize;
      fromFloat := FloatType[from];
      toFloat := FloatType[to];

  PROCEDURE assert(expr: TEXT; value: BOOLEAN): BOOLEAN =
  BEGIN
    IF value THEN RETURN TRUE END;
    RTIO.PutText("from:"); RTIO.PutText(Target.TypeNames[from]);
    RTIO.PutText(" to:"); RTIO.PutText(Target.TypeNames[to]);
    RTIO.PutText(" ASSERT failed:");
    RTIO.PutText(expr);
    RTIO.Flush();
    RETURN value;
  END assert;

  BEGIN
      WITH stack0 = pos(t, 0, "doloophole"),
           stop0 = t.vstack[stack0] DO

        IF fromFloat AND toFloat THEN

          (* no code is needed *)

        ELSIF (NOT fromFloat) AND (NOT toFloat) THEN

          <* ASSERT fromSize = GetTypeSize(stop0.optype) *>

          (* If we are narrowing, free up a register.
           * If we are widening, allocate and zero a register
           * OR we should support a notion of an operand being
           * split reg+imm, such as imm=0
           * or sign extend?
           * The notion of split reg+imm could be met by a
           * design that used multiple stack positions
           * for larger operands.
           *)

          IF changeSize THEN
            unlock(t);
            CASE stop0.loc OF
              | OLoc.fstack => <* ASSERT FALSE *>
              | OLoc.mem,
                OLoc.imm,        
                OLoc.register =>
                  IF fromSize = 2 THEN
                    find(t, stack0, Force.anyreg);
                  ELSE
                    IF TypeIsSigned(from) THEN
                      find(t, stack0, Force.regset, RegSet{EAX});
                    ELSE
                      find(t, stack0, Force.anyreg);
                    END;
                  END;
            END;
            CASE stop0.loc OF
              | OLoc.fstack => <* ASSERT FALSE *>
              | OLoc.mem => <* ASSERT FALSE *>
              | OLoc.imm => <* ASSERT FALSE *>
              | OLoc.register =>
                IF toSize = 1 THEN
                  (* just free up the upper register *)
                  t.dealloc_reg(stack0, operandPart := 1);
                ELSIF toSize = 2 THEN
                  (* This should be better. We can run out of registers.
                   * We should favor dead, or else anything but
                   * the one that holds the other half of this operand.
                   *)
                  IF TypeIsUnsigned(from) THEN
                    (* zero extend by allocating another register and xoring *)
                    WITH reg = finddead(t) DO
                      <* ASSERT reg # -1 *>
                      <* ASSERT reg # stop0.reg[0] *>
                      t.set_reg(stack0, reg, operandPart := 1);
                      t.cg.binOp(Op.oXOR, t.cg.reg[reg], t.cg.reg[reg]);
                    END;
                  ELSE
                    (* sign extend EAX to EDX with CDQ *)
                    t.corrupt(EDX, operandPart := 1);
                    t.set_reg(stack0, EDX, operandPart := 1);
                    t.cg.noargOp(Op.oCDQ);
                  END;
                ELSE
                  <* ASSERT FALSE *>
                END;
            END;
            stop0.optype := to;
          END;

        ELSIF fromFloat THEN
          <* ASSERT assert("NOT toFloat", NOT toFloat) *>
          <* ASSERT assert("stop0.loc = OLoc.fstack", stop0.loc = OLoc.fstack) *>
          <* ASSERT assert("CG_Size[from] = CG_Size[to]", CG_Size[from] = CG_Size[to]) *>

          stop0.loc := OLoc.mem;
          stop0.mvar.var := t.parent.declare_temp(CG_Bytes[to],
                                                  CG_Align_bytes[to], to,
                                                  FALSE);
          stop0.mvar.var.stack_temp := TRUE;
          stop0.mvar.mvar_offset := 0;
          stop0.mvar.mvar_type := from;
          t.cg.fstack_pop(stop0.mvar);
          stop0.mvar.mvar_type := to;
          stop0.optype := to;

        ELSE
          <* ASSERT NOT fromFloat *>
          <* ASSERT toFloat *>
          <* ASSERT assert("CG_Size[from] = CG_Size[to]", CG_Size[from] = CG_Size[to]) *>

          IF stop0.loc = OLoc.mem AND CG_Bytes[stop0.mvar.mvar_type] < 4 THEN
            find(t, stack0, Force.anyreg);
          END;

          IF stop0.loc = OLoc.register OR stop0.loc = OLoc.imm THEN
            find(t, stack0, Force.mem);
          END;
          (******* BOGUS  - WKK 2/7/95 *****************
          find(t, stack0, Force.mem);
          **********************************************)

          stop0.mvar.mvar_type := to;

          t.cg.fstack_push(stop0.mvar, TRUE);
          IF stop0.mvar.var.stack_temp THEN
            t.parent.free_temp(stop0.mvar.var);
          END;
          stop0.loc := OLoc.fstack;
        END
      END
  END doloophole;

PROCEDURE doindex_address (t: T; shift, size: INTEGER; neg: BOOLEAN) =
  VAR imsize: CARDINAL;
      muldest: Regno;
      tsize: TIntN.T;
      tshift: TIntN.T;
      tint: TIntN.T;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doindex_address"),
         stack1 = pos(t, 1, "doindex_address"),
         stop0 = t.vstack[stack0],
         stop1 = t.vstack[stack1] DO

      find(t, stack0, Force.any);
      find(t, stack1, Force.anyreg, AllRegisters, TRUE);

      IF stop0.loc = OLoc.imm THEN
        IF NOT TIntN.FromHostInteger(size, Target.Integer.bytes, tsize) THEN
          Err(t, "doindex_address: failed to convert size to target integer");
        END;
        IF NOT TIntN.Multiply(stop0.imm, tsize, tint) THEN
          Err(t, "doindex_address: multiply overflowed");
        END;
        stop0.imm := tint;
      ELSE
        IF stop0.loc # OLoc.register AND shift >= 0 THEN
          find(t, stack0, Force.anyreg);
        END;
        IF stop0.loc = OLoc.mem AND shift < 0 AND
          CG_Bytes[stop0.mvar.mvar_type] < 4 THEN
          find(t, stack0, Force.anydword);
        END;

        IF shift < 0 THEN
          IF size < 16_80 AND size > -16_81 THEN
            imsize := 1;
          ELSE
            imsize := 4;
          END;

          IF stop0.loc # OLoc.register THEN
            muldest := pickreg(t);
            corrupt(t, muldest, operandPart := 0);
            t.cg.imulImm(t.cg.reg[muldest], stop0, size, imsize);
            set_reg(t, stack0, muldest, operandPart := 0);

          ELSE
            t.cg.imulImm(stop0, stop0, size, imsize);
            newdest(t, stop0);
          END

        ELSIF shift > 0 THEN
          IF NOT TIntN.FromHostInteger(shift, Target.Integer.bytes, tshift) THEN
            Err(t, "doindex_address: failed to convert size to target integer");
          END;
          t.cg.immOp(Op.oSHL, stop0, tshift);
          newdest(t, stop0);
        END
      END;

      IF neg THEN
        t.cg.binOp(Op.oSUB, stop1, stop0);
      ELSE
        t.cg.binOp(Op.oADD, stop1, stop0);
      END;

      newdest(t, stop1);
      discard(t, 1);
    END
  END doindex_address;

TYPE MaxMinRec = RECORD
  regreg, regmem, memreg: Cond;
END;

TYPE MaxMinCond = ARRAY [Type.Word32 .. Type.Reel] OF MaxMinRec;

CONST maxmincond = ARRAY MaxMin OF MaxMinCond {
  (* MAX *)
    MaxMinCond { MaxMinRec { Cond.A, Cond.AE, Cond.BE },  (* Word32 *)
                 MaxMinRec { Cond.G, Cond.GE, Cond.LE },  (* Int32  *)
                 MaxMinRec { Cond.A, Cond.AE, Cond.BE },  (* Word64 *)
                 MaxMinRec { Cond.G, Cond.GE, Cond.LE },  (* Int64  *)
                 MaxMinRec { Cond.AE, Cond.AE, Cond.AE }  (* Reel   *)
               },
  (* MIN *)
    MaxMinCond { MaxMinRec { Cond.B, Cond.BE, Cond.AE },  (* Word32 *)
                 MaxMinRec { Cond.L, Cond.LE, Cond.GE },  (* Int32  *)
                 MaxMinRec { Cond.B, Cond.BE, Cond.AE },  (* Word64 *)
                 MaxMinRec { Cond.L, Cond.LE, Cond.GE },  (* Int64  *)
                 MaxMinRec { Cond.BE, Cond.BE, Cond.BE }  (* Reel   *)
               } };

PROCEDURE domaxmin (t: T; type: ZType; maxmin: MaxMin) =
  VAR lab, end: Label;
      src, dest: INTEGER;
      ftop_inmem: BOOLEAN;
      cond: Cond;
      reversed: BOOLEAN;
  BEGIN
    IF FloatType [type] THEN
      t.cg.binFOp(FOp.fCOM, 1);

      ftop_inmem := t.cg.ftop_inmem;

      corrupt(t, EAX, operandPart := 0);
      t.cg.noargFOp(FOp.fNSTSWAX);
      t.cg.noargOp(Op.oSAHF);

      lab := t.cg.reserve_labels(1, TRUE);
      end := t.cg.reserve_labels(1, TRUE);

      cond := maxmincond[maxmin][Type.Reel].regreg;
      IF NOT ftop_inmem THEN
        cond := revcond[cond];
      END;

      t.cg.brOp(cond, lab);
      t.cg.binFOp(FOp.fSTP, 1);

      t.cg.brOp(Cond.Always, end);

      t.cg.set_label(lab);

      t.cg.ftop_inmem := FALSE;
      IF NOT ftop_inmem THEN
        t.cg.f_pushnew(); (* It thinks we have just discarded something
                             from the stack in the previous branch, so we
                             have to fool it into letting us discard it
                             again without getting its stack counts
                             mixed up *)
        t.cg.fstack_discard();
      END;

      t.cg.set_label(end);

    ELSE
      unlock(t);
      reversed := findbin(t, TRUE, TRUE, dest, src);
      <* ASSERT reversed = (dest > src) *>

      WITH destop = t.vstack[dest],
           srcop = t.vstack[src] DO

        t.cg.binOp(Op.oCMP, destop, srcop);
        lab := t.cg.reserve_labels(1, TRUE);
        IF destop.loc = OLoc.register OR srcop.loc = OLoc.imm THEN
          IF srcop.loc = OLoc.register OR srcop.loc = OLoc.imm THEN
            t.cg.brOp(maxmincond[maxmin][type].regreg, lab);
          ELSE
            t.cg.brOp(maxmincond[maxmin][type].regmem, lab);
          END;
          t.cg.movOp(destop, srcop);
        ELSE
          t.cg.brOp(maxmincond[maxmin][type].memreg, lab);
          t.cg.movOp(srcop, destop);
        END;
        t.cg.set_label(lab);

        newdest(t, destop);
        IF reversed THEN
          swap(t);
        END;
      END
    END;

    discard(t, 1);
  END domaxmin;

PROCEDURE fltoint (t: T; mode: FlToInt; <*UNUSED*>type: Type) =
  VAR status: x86Var;
      statusop, newstat: Operand;
      statreg: Regno;
  BEGIN
    status := t.parent.declare_temp(8, 4, Type.Int32, FALSE);

    unlock(t);
    statreg := pickreg(t);
    corrupt(t, statreg, operandPart := 0);

    t.cg.noargOp(Op.oWAIT);
    t.cg.noargFOp(FOp.fNCLEX);

    statusop := Operand { loc := OLoc.mem, optype := Type.Int32,
                          mvar := MVar { var := status, mvar_offset := 0,
                                         mvar_type := Type.Int32 } };
    newstat := Operand { loc := OLoc.mem, optype := Type.Int32,
                         mvar := MVar { var := status, mvar_offset := 4,
                                         mvar_type := Type.Int32 } };
    t.cg.memFOp(FOp.fSTCW, statusop.mvar);

    t.cg.movOp(t.cg.reg[statreg], statusop);
    t.cg.immOp(Op.oAND, t.cg.reg[statreg], TIntN.F3FF);

    IF TIntN.NE(t.rmode[mode], TZero) THEN
      t.cg.immOp(Op.oOR, t.cg.reg[statreg], t.rmode[mode]);
    END;

    t.cg.movOp(newstat, t.cg.reg[statreg]);

    t.cg.memFOp(FOp.fLDCW, newstat.mvar);

    discard(t, 1);
    pushnew(t, Type.Int32, Force.mem);

    t.cg.memFOp(FOp.fISTP, t.vstack[pos(t, 0, "fltoint")].mvar);

    t.cg.noargOp(Op.oWAIT);
    t.cg.noargFOp(FOp.fNCLEX);
    t.cg.memFOp(FOp.fLDCW, statusop.mvar);

    t.parent.free_temp(status);
  END fltoint;

PROCEDURE inttoflt (t: T) =
  BEGIN
    WITH stack0 = pos(t, 0, "inttoflt"),
         stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.mem AND CG_Bytes[stop0.mvar.mvar_type] < 4 THEN
        unlock(t);
        find(t, stack0, Force.anyreg);
      END;

      IF stop0.loc = OLoc.register OR stop0.loc = OLoc.imm THEN
        find(t, stack0, Force.mem);
      END;

      t.cg.memFOp(FOp.fILD, stop0.mvar);
      IF stop0.mvar.var.stack_temp THEN
        t.parent.free_temp(stop0.mvar.var);
      END;
      stop0.loc := OLoc.fstack;
    END
  END inttoflt;

PROCEDURE newdest (t: T; READONLY op: Operand) =
  BEGIN
    IF op.loc = OLoc.register THEN
      FOR i := 0 TO GetTypeSize(op.optype) - 1 DO
        WITH z = t.reguse[op.reg[i]] DO
          z.last_store := NoStore;
          (* BUG? Upbound/lowbound should adapt to the type? *)
          z.upbound    := TIntN.TargetIntegerMax;
          z.lowbound   := TIntN.TargetIntegerMin;
          z.imm        := FALSE;
          z.non_nil    := FALSE;
        END;
      END;
    END
  END newdest;

PROCEDURE expand_stack (t: T) =
  BEGIN
    IF t.stacktop = t.vstacklimit THEN
      WITH newarr = NEW(REF ARRAY OF Operand, t.vstacklimit * 2) DO
        FOR i := 0 TO (t.vstacklimit - 1) DO
          newarr[i] := t.vstack[i];
        END;
        t.vstacklimit := t.vstacklimit * 2;
        t.vstack := newarr;
      END;
    END;
  END expand_stack;

PROCEDURE discard (t: T; depth: CARDINAL) =
  BEGIN
    IF depth > t.stacktop THEN
      Err(t, "Stack underflow in stack_discard");
    END;
    FOR i := t.stacktop - depth TO t.stacktop - 1 DO
      WITH stackp = t.vstack[i] DO
        CASE stackp.loc OF
          OLoc.mem =>
            IF stackp.mvar.var.stack_temp THEN
              t.parent.free_temp(stackp.mvar.var);
            END
        | OLoc.register =>
            FOR j := 0 TO GetOperandSize(stackp) - 1 DO
              t.reguse[stackp.reg[j]].stackp := -1;
              t.reguse[stackp.reg[j]].operandPart := -1;
            END;
        | OLoc.fstack =>
            (* The discards will have been done elsewhere *)
        | OLoc.imm =>
            (* Nothing to do *)
        END
      END
    END;
    t.stacktop := t.stacktop - depth;
  END discard;

PROCEDURE reg (t: T; stackp: CARDINAL): Regno =
  BEGIN
    RETURN t.vstack[stackp].reg[0];
  END reg;

PROCEDURE lower (t: T; reg: Regno): TIntN.T =
  BEGIN
    RETURN t.reguse[reg].lowbound;
  END lower;

PROCEDURE upper (t: T; reg: Regno): TIntN.T =
  BEGIN
    RETURN t.reguse[reg].upbound;
  END upper;

PROCEDURE set_lower (t: T; reg: Regno; newlow: TIntN.T) =
  BEGIN
    t.reguse[reg].lowbound := newlow;
  END set_lower;

PROCEDURE set_upper (t: T; reg: Regno; newup: TIntN.T) =
  BEGIN
    t.reguse[reg].upbound := newup;
  END set_upper;

PROCEDURE non_nil (t: T; reg: Regno): BOOLEAN =
  BEGIN
    RETURN t.reguse[reg].non_nil;
  END non_nil;

PROCEDURE set_non_nil (t: T; reg: Regno) =
  BEGIN
    t.reguse[reg].non_nil := TRUE;
  END set_non_nil;

PROCEDURE set_error_handler (t: T; err: ErrorHandler) =
  BEGIN
    t.Err := err;
  END set_error_handler;

PROCEDURE init (t: T) =
  BEGIN
    t.stacktop := 0;
    t.current_proc := NIL;

    FOR i := 0 TO NRegs DO
      WITH z = t.reguse[i] DO
        z.stackp     := -1;
        z.last_store := NoStore;
        (* BUG? Upbound/lowbound should adapt to the type? *)
        z.upbound    := TIntN.TargetIntegerMax;
        z.lowbound   := TIntN.TargetIntegerMin;
        z.imm        := FALSE;
        z.non_nil    := FALSE;
        z.locked     := FALSE;
      END;
    END;

    t.rmode := ARRAY FlToInt OF TIntN.T
      { TZero, TIntN.x0400, TIntN.x0800, TIntN.x0F00 };
  END init;

PROCEDURE end (<*UNUSED*> t: T) =
  BEGIN
  END end;

PROCEDURE set_current_proc (t: T; p: x86Proc) =
  BEGIN
    t.current_proc := p;
  END set_current_proc;

PROCEDURE New (parent: M3x86Rep.U; cg: Codex86.T; debug: BOOLEAN): T =
  VAR stack := NEW(T, parent := parent, cg := cg, debug := debug);
  BEGIN
    stack.vstacklimit := 16;
    stack.vstack := NEW(REF ARRAY OF Operand, stack.vstacklimit);
    RETURN stack;
  END New;

CONST
  OLocName = ARRAY OLoc OF TEXT { "MEM", "REG", "FSTACK", "IMM" };

PROCEDURE Debug (t: T;  tag: TEXT;  wr: Wrx86.T) =
  VAR
    tos := t.stacktop - 1;
  BEGIN
    IF NOT t.debug THEN RETURN END;
    wr.OutT (tag);  wr.NL ();
    FOR i := 0 TO tos DO
      wr.OutT ("  S-");  wr.OutI (i);  wr.OutT (": ");
      DebugOp (t.vstack [tos-i], wr);
      wr.NL ();
    END;

    FOR i := 0 TO NRegs DO
      wr.OutT ("  ");
      wr.OutT (RegName[i]);
      wr.OutT (": ");
      DebugReg (t.reguse [i], wr);
      wr.NL ();
    END;
  END Debug;

PROCEDURE DebugOp (READONLY op: Operand;  wr: Wrx86.T) =
  BEGIN
    wr.OutT (OLocName [op.loc]);
    wr.OutT ("  mvar: ");  DebugMVar (op.mvar, wr);
    wr.OutT ("  reg: "); wr.OutT (RegName [op.reg[0]]);
    wr.OutT ("  imm: "); wr.OutT (TIntN.ToText (op.imm));
    wr.OutT ("  stackp: ");  wr.OutI (op.stackp);
    IF (op.opcode) THEN wr.OutT ("  OPCODE"); END;
  END DebugOp;

PROCEDURE DebugReg (READONLY r: Register;  wr: Wrx86.T) =
  BEGIN
    IF r.stackp # -1 THEN
      wr.OutT ("  stackp: ");  wr.OutI (r.stackp);
    END;
    IF r.last_store # NoStore THEN
      wr.OutT ("  mvar: ");  DebugMVar (r.last_store, wr);
    END;
    IF (NOT TIntN.EQ(r.last_imm, TZero)) THEN
      wr.OutT ("  imm: ");  wr.OutT (TIntN.ToText (r.last_imm));
    END;
    IF (NOT TIntN.EQ(r.lowbound, TIntN.TargetIntegerMin)) THEN
      wr.OutT ("  lo: ");  wr.OutT (TIntN.ToText (r.lowbound));
    END;
    IF (NOT TIntN.EQ(r.upbound, TIntN.TargetIntegerMax)) THEN
      wr.OutT ("  hi: ");  wr.OutT (TIntN.ToText (r.upbound));
    END;
    IF (r.imm # FALSE) THEN
      wr.OutT ("  IMMED");
    END;
    IF (r.locked # FALSE) THEN
      wr.OutT ("  LOCKED");
    END;
    IF (r.non_nil # FALSE) THEN
      wr.OutT ("  NON-NIL");
    END;
  END DebugReg;

PROCEDURE DebugMVar (READONLY v: MVar;  wr: Wrx86.T) =
  BEGIN
    wr.OutT ("{ ");  wr.VName (v.var);
    IF (v.mvar_offset # 0) THEN  wr.OutT ("  offset: ");  wr.OutI (v.mvar_offset);  END;
    wr.OutT ("  type: ");  wr.TName (v.mvar_type);
    wr.OutT (" }");
  END DebugMVar;

PROCEDURE Err(t: T; err: TEXT) =
  BEGIN
    t.Err(err);
    <* ASSERT FALSE *>
  END Err;

BEGIN
END Stackx86.
