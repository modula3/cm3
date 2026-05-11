(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CallStmt.m3                                           *)
(* Last modified on Tue Jun 22 14:29:40 PDT 1993 by kalsow     *)
(*      modified on Tue Dec 18 04:49:06 1990 by muller         *)

MODULE CallStmt;

IMPORT Expr, Stmt, StmtRep, Error, Type, ErrType, CaptureAnalysis;

TYPE
  P = Stmt.T BRANDED "CallStmt.P" OBJECT
        e       : Expr.T;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

PROCEDURE New (e: Expr.T): Stmt.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    p.e := e;
    RETURN p;
  END New;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.e, cs);
    t := Type.Base (Expr.TypeOf (p.e));
    IF (t # NIL) AND (t # ErrType.T) THEN
      Error.Msg ("Expression is not a statement");
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Expr.Prep (p.e);
    Expr.Compile (p.e);
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

PROCEDURE CompileMSIR (p: P) =
  BEGIN
    EVAL Expr.CompileMSIR(p.e);
  END CompileMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.e, ca);
  END Capture;

BEGIN
END CallStmt.
