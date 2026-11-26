MODULE MSIRChecks;

IMPORT MSIR, Fmt, Target, Word;
FROM MSIR IMPORT Op;

CONST
  LtPred = ARRAY BOOLEAN OF MSIR.CmpPred {MSIR.CmpPred.Slt, MSIR.CmpPred.Ult};
  GtPred = ARRAY BOOLEAN OF MSIR.CmpPred {MSIR.CmpPred.Sgt, MSIR.CmpPred.Ugt};

VAR seq := 0;  (* for unique block labels *)

PROCEDURE IsCheck (op: Op): BOOLEAN =
  BEGIN
    RETURN op IN SET OF Op {Op.NilCheck, Op.RangeCheck, Op.SubscriptCheck,
                            Op.Fault, Op.ShapeCheck};
  END IsCheck;

PROCEDURE HasCheck (b: MSIR.Block): BOOLEAN =
  BEGIN
    FOR j := 0 TO MSIR.BlockInsnCount (b) - 1 DO
      IF IsCheck (MSIR.InsnOp (MSIR.BlockInsn (b, j))) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END HasCheck;

PROCEDURE NewBlk (p: MSIR.Proc;  like: MSIR.Block;  tag: TEXT): MSIR.Block =
  VAR b := MSIR.NewBlock ("chk." & tag & "." & Fmt.Int (seq),
                          ARRAY OF MSIR.BlockParam {});
  BEGIN
    INC (seq);
    MSIR.ProcAddBlock (p, b);
    MSIR.BlockSetScope (b, MSIR.BlockScope (like));
    RETURN b;
  END NewBlk;

(* Is bound c the extreme (lowest if lo, else highest) of the w-bit
   integers, so that comparing against it can never fail?  A non-constant
   bound, or one represented otherwise, just costs a compare. *)
PROCEDURE Extreme (c: MSIR.Value;  lo, unsigned: BOOLEAN;  w: INTEGER): BOOLEAN =
  VAR x, e: INTEGER;
  BEGIN
    IF MSIR.GetValueKind (c) # MSIR.ValueKind.ConstInt
       OR w <= 0 OR w > BITSIZE (INTEGER) THEN RETURN FALSE END;
    x := MSIR.GetIntVal (c);
    IF unsigned THEN
      IF lo THEN e := 0
      ELSIF w = BITSIZE (INTEGER) THEN e := -1
      ELSE e := Word.Minus (Word.LeftShift (1, w), 1)
      END;
    ELSIF w = BITSIZE (INTEGER) THEN
      IF lo THEN e := FIRST (INTEGER) ELSE e := LAST (INTEGER) END;
    ELSE
      e := Word.LeftShift (1, w - 1);
      IF lo THEN e := -e ELSE e := e - 1 END;
    END;
    RETURN x = e;
  END Extreme;

(* The condition under which check i fails, built in block b. *)
PROCEDURE FailCond (b: MSIR.Block;  i: MSIR.Insn): MSIR.Value =
  VAR v := MSIR.InsnOperand (i, 0);
  BEGIN
    CASE MSIR.InsnOp (i) OF
    | Op.NilCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, v,
                               MSIR.ConstNil (MSIR.ValueType (v)));
    | Op.ShapeCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Ne, v,
                               MSIR.InsnOperand (i, 1));
    | Op.SubscriptCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Uge, v,
                               MSIR.InsnOperand (i, 1));
    | Op.RangeCheck =>
        VAR
          lo := MSIR.InsnOperand (i, 1);
          hi := MSIR.InsnOperand (i, 2);
          u  := MSIR.InsnCheckUnsigned (i);
          w  := MSIR.BitWidth (MSIR.ValueType (v));
          below, above: MSIR.Value := NIL;
        BEGIN
          IF NOT Extreme (lo, TRUE, u, w) THEN
            below := MSIR.BuildICmp (b, "", LtPred[u], v, lo);
          END;
          IF NOT Extreme (hi, FALSE, u, w) THEN
            above := MSIR.BuildICmp (b, "", GtPred[u], v, hi);
          END;
          IF below = NIL AND above = NIL THEN
            RETURN MSIR.ConstInt (MSIR.TI1 (), 0);
          ELSIF below = NIL THEN RETURN above
          ELSIF above = NIL THEN RETURN below
          ELSE RETURN MSIR.BuildIOr (b, "", below, above)
          END;
        END;
    ELSE <* ASSERT FALSE *>
    END;
  END FailCond;

(* In b, report check i's failure: RTHooks.ReportFault (module,
   line * 32 + code), which does not return. *)
PROCEDURE CallHook (m: MSIR.Module;  b: MSIR.Block;  i: MSIR.Insn) =
  VAR
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    intT := MSIR.TI (Target.Integer.size);
  BEGIN
    EVAL MSIR.BuildCall (b, "", MSIR.ModuleFaultHook (m),
           ARRAY OF MSIR.Value {
             MSIR.StructFieldRef (MSIR.ModuleInfoName (m), 0, ptrT),
             MSIR.ConstInt (intT, MSIR.InsnSrcLine (i) * 32
                                  + MSIR.InsnCheckCode (i))});
    MSIR.BuildUnreachable (b);
  END CallHook;

PROCEDURE ExpandProc (m: MSIR.Module;  p: MSIR.Proc) =
  VAR
    hook := MSIR.ModuleFaultHook (m);
    k := 0;
  BEGIN
    (* New continuation blocks are appended and examined in turn. *)
    WHILE k < MSIR.ProcBlockCount (p) DO
      VAR b := MSIR.ProcBlock (p, k);
      BEGIN
        IF HasCheck (b) THEN
          <* ASSERT hook # NIL *>
          VAR insns := MSIR.BlockTakeInsns (b);  cur := b;
          BEGIN
            FOR j := 0 TO LAST (insns^) DO
              VAR i := insns[j];
              BEGIN
                IF MSIR.InsnOp (i) = Op.Fault THEN
                  (* a terminator: the call and unreachable end the block *)
                  CallHook (m, cur, i);
                ELSIF IsCheck (MSIR.InsnOp (i)) THEN
                  VAR fault := NewBlk (p, b, "fault");
                      cont  := NewBlk (p, b, "cont");
                  BEGIN
                    MSIR.BuildCondBr (cur, FailCond (cur, i),
                                      fault, ARRAY OF MSIR.Value {},
                                      cont,  ARRAY OF MSIR.Value {});
                    CallHook (m, fault, i);
                    cur := cont;
                  END;
                ELSE
                  MSIR.BlockAppendInsn (cur, i);
                END;
              END;
            END;
          END;
        END;
        INC (k);
      END;
    END;
  END ExpandProc;

PROCEDURE Expand (m: MSIR.Module) =
  BEGIN
    FOR i := 0 TO MSIR.ModuleProcCount (m) - 1 DO
      ExpandProc (m, MSIR.ModuleProc (m, i));
    END;
  END Expand;

BEGIN
END MSIRChecks.
