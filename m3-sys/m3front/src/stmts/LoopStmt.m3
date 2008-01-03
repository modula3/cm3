(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: LoopStmt.m3                                           *)
(* Last modified on Fri Jun 24 12:28:38 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 16 14:12:01 1989 by muller         *)

MODULE LoopStmt;

IMPORT CG, Scanner, Stmt, StmtRep, Marker, Token;

TYPE
  P = Stmt.T OBJECT
        body    : Stmt.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);
  BEGIN
    StmtRep.Init (p);
    Scanner.Match (TK.tLOOP);
    p.body := Stmt.Parse ();
    Scanner.Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  BEGIN
    Marker.PushExit (CG.No_label);
    Stmt.TypeCheck (p.body, cs);
    Marker.Pop ();
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  top := CG.Next_label (2);
  BEGIN
    Marker.PushExit (top+1);
      CG.Set_label (top);
      oc := Stmt.Compile (p.body);
      IF (Stmt.Outcome.FallThrough IN oc) THEN
        CG.Jump (top);
        oc := oc - Stmt.Outcomes {Stmt.Outcome.FallThrough};
      END;
      CG.Set_label (top+1);
    Marker.Pop ();
    IF (Stmt.Outcome.Exits IN oc) THEN
      oc := oc + Stmt.Outcomes {Stmt.Outcome.FallThrough}
               - Stmt.Outcomes {Stmt.Outcome.Exits};
    END;
    RETURN oc;
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR oc := Stmt.GetOutcome (p.body);
  BEGIN
    oc := oc - Stmt.Outcomes {Stmt.Outcome.FallThrough};
    IF (Stmt.Outcome.Exits IN oc) THEN
      oc := oc + Stmt.Outcomes {Stmt.Outcome.FallThrough}
               - Stmt.Outcomes {Stmt.Outcome.Exits};
    END;
    RETURN oc;
  END GetOutcome;

BEGIN
END LoopStmt.
