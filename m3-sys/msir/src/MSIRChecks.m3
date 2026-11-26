MODULE MSIRChecks;

IMPORT MSIR, Fmt, Target;
FROM MSIR IMPORT Op;

VAR seq := 0;  (* for unique block labels *)

PROCEDURE IsCheck (op: Op): BOOLEAN =
  BEGIN
    RETURN op = Op.NilCheck;
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

(* The condition under which check i fails, built in block b. *)
PROCEDURE FailCond (b: MSIR.Block;  i: MSIR.Insn): MSIR.Value =
  VAR v := MSIR.InsnOperand (i, 0);
  BEGIN
    CASE MSIR.InsnOp (i) OF
    | Op.NilCheck =>
        RETURN MSIR.BuildICmp (b, "", MSIR.CmpPred.Eq, v,
                               MSIR.ConstNil (MSIR.ValueType (v)));
    ELSE <* ASSERT FALSE *>
    END;
  END FailCond;

PROCEDURE ExpandProc (m: MSIR.Module;  p: MSIR.Proc) =
  VAR
    hook := MSIR.ModuleFaultHook (m);
    ptrT := MSIR.TPtr (MSIR.TVoid ());
    intT := MSIR.TI (Target.Integer.size);
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
                IF IsCheck (MSIR.InsnOp (i)) THEN
                  VAR fault := NewBlk (p, b, "fault");
                      cont  := NewBlk (p, b, "cont");
                      modRef := MSIR.StructFieldRef (MSIR.ModuleInfoName (m),
                                                     0, ptrT);
                  BEGIN
                    MSIR.BuildCondBr (cur, FailCond (cur, i),
                                      fault, ARRAY OF MSIR.Value {},
                                      cont,  ARRAY OF MSIR.Value {});
                    EVAL MSIR.BuildCall (fault, "", hook,
                           ARRAY OF MSIR.Value {
                             modRef,
                             MSIR.ConstInt (intT, MSIR.InsnSrcLine (i) * 32
                                                  + MSIR.InsnCheckCode (i))});
                    MSIR.BuildUnreachable (fault);
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
