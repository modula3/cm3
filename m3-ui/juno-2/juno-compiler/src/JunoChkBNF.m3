(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 28 13:17:28 PST 1997 by heydon                   *)
(*      modified on Fri Aug  7 21:54:06 PDT 1992 by myers                    *)

MODULE JunoChkBNF;

IMPORT JunoAST, JunoCompileErr, JunoASTUtils;

PROCEDURE TotalCmd(cmd: JunoAST.Cmd) RAISES {JunoCompileErr.Error} =
  BEGIN
    TYPECASE cmd OF
      JunoAST.Skip, JunoAST.Abort, JunoAST.Halt => (* SKIP *)
    | JunoAST.Else (c) => PartialCmd(c.c1); TotalCmd(c.c2)
    | JunoAST.Proj (c) => DefinedHints(c.vars); TotalCmd(c.body)
    | JunoAST.Seq (c) => TotalCmd(c.c1); TotalCmd(c.c2)
    | JunoAST.GroupedCmd (c) => TotalCmd(c.body);
    | JunoAST.Assign (c) => QIdList(c.vars); ExprList(c.exprs)
    | JunoAST.ProcCall (c) => QIdList(c.inouts); ExprList(c.ins);
    | JunoAST.Do (c) => PartialCmd(c.body);
    | JunoAST.If (c) => PartialCmd(c.body);
    | JunoAST.Save (c) => TotalCmd(c.body);
    ELSE ExpectedError("Total Command", cmd)
    END
  END TotalCmd;

PROCEDURE DefinedHints(vars: JunoAST.NearVarList)
  RAISES {JunoCompileErr.Error} =
(* Raises "Error" if any of the variables in "vars" is hinted with an
   expression that could be undefined. *)
  VAR l := vars.head; BEGIN
    WHILE l # NIL DO
      IF l.hint # JunoAST.NilExpr AND
         NOT JunoASTUtils.AlwaysDefined(l.hint) THEN
        ExpectedError("Total Command", vars)
      END;
      l := l.next
    END
  END DefinedHints;

PROCEDURE PartialCmd(cmd: JunoAST.Cmd) RAISES {JunoCompileErr.Error} =
  BEGIN
    TYPECASE cmd OF
      JunoAST.Else (c) => PartialCmd(c.c1); PartialCmd(c.c2)
    | JunoAST.Proj (c) => PartialCmd(c.body);
    | JunoAST.Guard (c) => Formula(c.grd); PartialCmd(c.body);
    | JunoAST.Seq (c) => PartialCmd(c.c1); TotalCmd(c.c2)
    | JunoAST.Query (c) => Formula(c.f)
    | JunoAST.GroupedCmd (c) => PartialCmd(c.body);
    ELSE
      TRY TotalCmd(cmd) EXCEPT JunoCompileErr.Error (e) =>
        IF e.ast = cmd
          THEN ExpectedError("Partial Command", cmd)
          ELSE RAISE JunoCompileErr.Error(e)
        END
      END
    END
  END PartialCmd;

PROCEDURE Formula(form: JunoAST.Formula) RAISES {JunoCompileErr.Error} =
  BEGIN
    TYPECASE form OF
      JunoAST.Or (f) => Formula(f.f1); Formula(f.f2);
    | JunoAST.And (f) => Formula(f.f1); Formula(f.f2);
    | JunoAST.Not (f) => Formula(f.f);
    | JunoAST.True, JunoAST.False => (* SKIP *)
    | JunoAST.Exists (f) => Formula(f.f)
    | JunoAST.BIUPred (f) => Expr(f.e)
    | JunoAST.Relation (f) => Expr(f.e1); Expr(f.e2)
    | JunoAST.Call => PredicateCall(form)
    | JunoAST.GroupedExpr (f) => Formula(f.expr)
    ELSE ExpectedError("Formula", form)
    END
  END Formula;

PROCEDURE PredicateCall(call: JunoAST.Call) RAISES {JunoCompileErr.Error} =
  BEGIN
    IF call.inouts.size # 0 THEN
      ExpectedError("Predicate Call", call);
    END;
    ExprList(call.ins)
  END PredicateCall;

PROCEDURE Expr(expr: JunoAST.Expr) RAISES {JunoCompileErr.Error} =
  BEGIN
    TYPECASE expr OF
      JunoAST.Rel (e) => Expr(e.e1); Expr(e.e2)
    | JunoAST.BuiltInAddFunc (e) => Expr(e.e1); Expr(e.e2)
    | JunoAST.BuiltInMulFunc (e) => Expr(e.e1); Expr(e.e2)
    | JunoAST.UMinus (e) => Expr(e.e);
    | JunoAST.AtomicExpr => (* SKIP *)
    | JunoAST.BIUFunc (e) => Expr(e.e)
    | JunoAST.BIBFunc (e) => Expr(e.e1); Expr(e.e2)
    | JunoAST.List (e) => ExprList(e.elts)
    | JunoAST.Call (e) => QIdList(e.inouts); ExprList(e.ins)
    | JunoAST.GroupedExpr (e) => Expr(e.expr)
    ELSE ExpectedError("Expression", expr)
    END
  END Expr;

PROCEDURE QIdList(qids: JunoAST.ExprList) RAISES {JunoCompileErr.Error} =
  VAR curr := qids.head; BEGIN
    WHILE curr # NIL DO
      IF NOT ISTYPE(curr.expr, JunoAST.QId) THEN
        ExpectedError("QID", curr.expr)
      END;
      curr := curr.next;
    END
  END QIdList;

PROCEDURE ExprList(exprs: JunoAST.ExprList) RAISES {JunoCompileErr.Error} =
  VAR curr := exprs.head; BEGIN
    WHILE curr # NIL DO
      Expr(curr.expr);
      curr := curr.next;
    END
  END ExprList;

PROCEDURE ExpectedError(t: TEXT; ast: JunoAST.T)
    RAISES {JunoCompileErr.Error} =
  BEGIN
    JunoCompileErr.Raise("Expected \"" & t & "\"", ast)
  END ExpectedError;

BEGIN
END JunoChkBNF.
