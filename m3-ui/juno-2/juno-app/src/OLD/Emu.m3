(* Copyright (C) 1992, Digital Equipment Corporation *)
(* Last modified on Sat Aug 22 21:54:21 1992 by myers *)

MODULE Emu;

IMPORT JunoAST AS AST, CurrCmd, AtomJVTbl, JunoValue AS Value,
       JunoScope AS Scope, JunoRT AS RT;

TYPE
  T = BRANDED OBJECT
        scp       : Scope.T;
        pointCache: AtomJVTbl.T := NIL;
      END;
(* "pointCache" contains the locations of the points in the executed
   command. *)

PROCEDURE Run (ast: AST.T; scp: Scope.T): AtomJVTbl.T =
  VAR
    cmds := CurrCmd.GetCmds(ast);
    e    := NEW(T, scp := scp);
  BEGIN
    InitCache(e, ast);
    RunSeq(e, cmds);
    RETURN e.pointCache
  END Run;

PROCEDURE RunSeq (e: T; cmds: AST.Cmd) =
  BEGIN
    TYPECASE cmds OF
      NULL => RETURN
    | AST.Seq (seq) => RunSeq(e, seq.c1); RunSeq(e, seq.c2)
    | AST.Skip =>               (* SKIP *)
    | AST.ProcCall (pc) => RunOne(e, pc)
    ELSE
      (* drop it on the floor *)
    END
  END RunSeq;

VAR frame := ARRAY [1 .. 100] OF Value.T{NIL, ..};

PROCEDURE Arg (i: CARDINAL): REFANY =
  BEGIN
    RETURN frame[i]
  END Arg;

PROCEDURE RunOne (e: T; pc: AST.ProcCall) =
  VAR
    qid := pc.name;
    ins := pc.ins.head;
    i   := pc.ins.size;
  BEGIN
    <* ASSERT pc.inouts.size = 0 *>
    <*ASSERT pc.ins.size <= NUMBER(frame) *>
    WHILE ins # NIL DO
      IF NOT e.pointCache.get(NARROW(ins.expr, AST.QId).id0, frame[i]) THEN
        <*ASSERT FALSE*>
      END;
      DEC(i);
      ins := ins.next
    END;

    VAR
      unit  : Scope.Entity;
      entity               := Scope.LookupQId(e.scp, qid, unit);
    BEGIN
      TYPECASE entity OF
        NULL =>                 <*NOWARN*>
        <* ASSERT FALSE *>
      | Scope.Proc (p) =>
          VAR c: RT.ExternalCode := RT.ext_code_tbl[p.index];
          BEGIN
            EVAL c.invoke()
          END
      END
    END
  END RunOne;

(* Extract the real numbers from a "Value.Pair" containing two reals *)
PROCEDURE RealPair (p: Value.Pair; VAR h, v: Value.Real) =
  BEGIN
    h := NARROW(p.car, REF Value.Real)^;
    v := NARROW(p.cdr, REF Value.Real)^
  END RealPair;

PROCEDURE RefReal (x: Value.Real): REF Value.Real =
  VAR res := NEW(REF Value.Real);
  BEGIN
    res^ := x;
    RETURN res
  END RefReal;

PROCEDURE Number (e: AST.Expr): REF Value.Real =
  BEGIN
    TYPECASE e OF
      AST.Number (n) =>         <* NOWARN *>
        RETURN n.val
    | AST.UMinus (e) => RETURN RefReal(-Number(e.e)^)
    END
  END Number;

PROCEDURE InitCache (e: T; ast: AST.T) =
  VAR
    vars           := CurrCmd.GetVariables(ast);
    dummy: Value.T;

  PROCEDURE EvalHint (hint: AST.Expr): Value.T =
    BEGIN
      IF hint = AST.NilExpr THEN RETURN NIL END;
      TYPECASE hint OF
        AST.Pair (p) =>

          RETURN
            NEW(REF Value.Pair, car := Number(p.e1), cdr := Number(p.e2))
      | AST.Rel (r) =>
          VAR
            b := NARROW(NARROW(r.e2, AST.Pair).e1, AST.QId).id0;
            c := NARROW(NARROW(r.e2, AST.Pair).e2, AST.QId).id0;
            bv, cv: Value.T;
            x               := Number(NARROW(r.e1, AST.Pair).e1)^;
            y               := Number(NARROW(r.e1, AST.Pair).e2)^;
          BEGIN
            IF e.pointCache.get(b, bv) AND e.pointCache.get(c, cv) THEN
              VAR bx, by, cx, cy: Value.Real;
              BEGIN
                RealPair(NARROW(bv, REF Value.Pair)^, bx, by);
                RealPair(NARROW(cv, REF Value.Pair)^, cx, cy);
                VAR
                  ax := bx + (cx - bx) * x - (cy - by) * y;
                  ay := by + (cx - bx) * y + (cy - by) * x;
                BEGIN
                  RETURN NEW(REF Value.Pair, car := RefReal(ax),
                             cdr := RefReal(ay))
                END
              END
            ELSE
              RETURN NIL
            END
          END
      ELSE
        RETURN NIL
      END
    END EvalHint;

  BEGIN
    e.pointCache := NEW(AtomJVTbl.T).init();
    IF vars # NIL THEN
      VAR done := FALSE;
      BEGIN
        WHILE NOT done DO
          done := TRUE;
          VAR v := vars.head;
          BEGIN
            WHILE v # NIL DO
              IF NOT e.pointCache.get(v.id, dummy) THEN
                VAR hint := EvalHint(v.hint);
                BEGIN
                  IF hint # NIL THEN
                    done := FALSE;
                    EVAL e.pointCache.put(v.id, hint)
                  END
                END
              END;
              v := v.next
            END
          END
        END
      END
    END
  END InitCache;

BEGIN
END Emu.
