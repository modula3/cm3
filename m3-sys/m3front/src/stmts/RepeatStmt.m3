(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RepeatStmt.m3                                         *)
(* Last modified on Fri Jun 24 14:00:24 PDT 1994 by kalsow     *)
(*      modified on Tue Oct 10 18:42:03 1989 by muller         *)

MODULE RepeatStmt;

IMPORT CG, Expr, Type, Bool, Error, Stmt, StmtRep;
IMPORT Token, Scanner, Marker;
IMPORT MSIR, MSIRBuilder, CaptureAnalysis;

TYPE
  P = Stmt.T OBJECT
        body    : Stmt.T;
        expr    : Expr.T;
        e_origin: INTEGER;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    Scanner.Match (TK.tREPEAT);
    p.body := Stmt.Parse ();
    Scanner.Match (TK.tUNTIL);
    p.e_origin := Scanner.offset;
    p.expr := Expr.Parse ();
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Marker.PushExit (CG.No_label);
    Stmt.TypeCheck (p.body, cs);
    Marker.Pop ();
    Expr.TypeCheck (p.expr, cs);
    t := Expr.TypeOf (p.expr);
    IF (Type.Base (t) # Bool.T) THEN
      Error.Msg ("REPEAT condition must be a BOOLEAN");
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  top := CG.Next_label (2);
  BEGIN
    Marker.PushExit (top+1);
      CG.Set_label (top);
      oc := Stmt.Compile (p.body);
      IF (Stmt.Outcome.FallThrough IN oc) THEN
        CG.Gen_location (p.e_origin);
        Expr.PrepBranch (p.expr, CG.No_label, top, CG.Likely);
        Expr.CompileBranch (p.expr, CG.No_label, top, CG.Likely);
      END;
      CG.Set_label (top+1);
    Marker.Pop ();

    IF (Stmt.Outcome.Exits IN oc) THEN
      oc := oc  + Stmt.Outcomes {Stmt.Outcome.FallThrough}
                - Stmt.Outcomes {Stmt.Outcome.Exits};
    END;
    RETURN oc;
  END Compile;

PROCEDURE CompileMSIR (p: P) =
  VAR
    bodyBlk: MSIR.Block;
    exitBlk: MSIR.Block;
    cond:    MSIR.Value;
  BEGIN
    bodyBlk := MSIRBuilder.NewBlock ("repeat.body");
    exitBlk := MSIRBuilder.NewBlock ("repeat.exit");

    MSIR.BuildBr (MSIRBuilder.CurrentBlock (), bodyBlk, ARRAY OF MSIR.Value{});
    MSIRBuilder.SetCurrentBlock (bodyBlk);

    MSIRBuilder.PushExitBlock (exitBlk);
    Stmt.CompileMSIR (p.body);
    MSIRBuilder.PopExitBlock ();

    IF MSIRBuilder.InProc () AND NOT MSIRBuilder.CurrentBlockTerminated () THEN
      cond := Expr.CompileMSIR (p.expr);
      IF cond # NIL THEN
        (* UNTIL expr: exit if TRUE, loop back if FALSE *)
        MSIR.BuildCondBr (MSIRBuilder.CurrentBlock (), cond,
                          exitBlk, ARRAY OF MSIR.Value{},
                          bodyBlk, ARRAY OF MSIR.Value{});
      END;
    END;

    MSIRBuilder.SetCurrentBlock (exitBlk);
  END CompileMSIR;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR oc := Stmt.GetOutcome (p.body);
  BEGIN
    IF (Stmt.Outcome.Exits IN oc) THEN
      oc := oc  + Stmt.Outcomes {Stmt.Outcome.FallThrough}
                - Stmt.Outcomes {Stmt.Outcome.Exits};
    END;
    RETURN oc;
  END GetOutcome;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Stmt.Capture (p.body, ca);
    Expr.Capture (p.expr, ca);
  END Capture;

BEGIN
END RepeatStmt.
