(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExitStmt.m3                                           *)
(* Last modified on Fri Jun 24 12:35:50 PDT 1994 by kalsow     *)
(*      modified on Tue Apr 10 22:54:44 1990 by muller         *)

MODULE ExitStmt;

IMPORT Stmt, StmtRep, Error, Token, Scanner, Marker;

TYPE
  P = Stmt.T OBJECT
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
    Scanner.Match (Token.T.tEXIT);
    RETURN p;
  END Parse;

PROCEDURE Check (<*UNUSED*> p: P;  <*UNUSED*> VAR cs: Stmt.CheckState) =
  BEGIN
    IF NOT Marker.ExitOK () THEN
      Error.Msg ("EXIT not contained in a loop");
    END;
  END Check;

PROCEDURE Compile (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    Marker.EmitExit ();
    RETURN Stmt.Outcomes {Stmt.Outcome.Exits};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.Exits};
  END GetOutcome;

BEGIN
END ExitStmt.
