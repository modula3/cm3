(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ExitStmt.m3                                           *)
(* Last modified on Fri Jun 24 12:35:50 PDT 1994 by kalsow     *)
(*      modified on Tue Apr 10 22:54:44 1990 by muller         *)

MODULE ExitStmt;

IMPORT Stmt, StmtRep, Error, Token, Scanner, Marker;
IMPORT MSIR, MSIRBuilder;

TYPE
  P = Stmt.T OBJECT
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
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

PROCEDURE CompileMSIR (<*UNUSED*> p: P) =
  VAR deadBlock: MSIR.Block;
  BEGIN
    (* To the innermost loop's exit block (unwind_to if that leaves EH
       scopes, running FINALLY code), or an enclosing EXCEPT ELSE. *)
    MSIRBuilder.EmitExitMSIR ();
    (* Instructions after EXIT are dead; switch to a fresh block. *)
    deadBlock := MSIRBuilder.NewBlock ("exit.dead");
    MSIRBuilder.SetCurrentBlock (deadBlock);
  END CompileMSIR;

BEGIN
END ExitStmt.
