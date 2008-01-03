(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ReturnStmt.m3                                         *)
(* Last modified on Fri Jun 24 12:30:34 PDT 1994 by kalsow     *)
(*      modified on Thu Dec  5 17:22:32 PST 1991 by muller     *)

MODULE ReturnStmt;

IMPORT Expr, Error, Type, AssignStmt, Token, Scanner;
IMPORT Variable, Marker, Stmt, StmtRep;

TYPE
  P = Stmt.T OBJECT
        expr    : Expr.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    p.expr := NIL;
    Scanner.Match (Token.T.tRETURN);
    IF (Scanner.cur.token IN Token.ExprStart) THEN
      p.expr := Expr.Parse ();
    END;
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;  v: Variable.T;
  BEGIN
    Expr.TypeCheck (p.expr, cs);
    IF NOT Marker.ReturnOK () THEN
      Error.Msg ("RETURN not in a procedure");
      RETURN ;
    END;
    Marker.ReturnVar (t, v);
    IF (p.expr = NIL) THEN
      IF (t # NIL) THEN Error.Msg ("missing return result") END;
    ELSIF (t = NIL) THEN
      Error.Msg ("procedure does not have a return result");
    ELSE
      AssignStmt.Check (t, p.expr, cs);
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Marker.EmitReturn (p.expr, fromFinally := FALSE);
    RETURN Stmt.Outcomes {Stmt.Outcome.Returns};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.Returns};
  END GetOutcome;

BEGIN
END ReturnStmt.
