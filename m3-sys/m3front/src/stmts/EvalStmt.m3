(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EvalStmt.m3                                           *)
(* Last modified on Fri Jun 24 12:35:51 PDT 1994 by kalsow     *)
(*      modified on Tue Apr 10 22:53:33 1990 by muller         *)

MODULE EvalStmt;

IMPORT CG, Expr, Token, Scanner, Stmt, StmtRep, Error, Type;

TYPE
  P = Stmt.T OBJECT
        e       : Expr.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Scanner.Match (Token.T.tEVAL);
    p.e := Expr.Parse ();
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  BEGIN
    Expr.TypeCheck (p.e, cs);
    IF (Expr.TypeOf (p.e) = NIL) THEN
      Error.Msg ("expression doesn\'t have a value");
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Expr.Prep (p.e);
    Expr.Compile (p.e);
    CG.Discard (Type.CGType (Expr.TypeOf (p.e)));
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END EvalStmt.
