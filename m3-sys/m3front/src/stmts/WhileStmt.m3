(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WhileStmt.m3                                          *)
(* Last modified on Wed Nov  9 14:36:06 PST 1994 by kalsow     *)
(*      modified on Tue Oct 10 18:42:00 1989 by muller         *)

MODULE WhileStmt;

IMPORT CG, Expr, Type, Bool, Error, Marker, ErrType;
IMPORT Stmt, StmtRep, Token, Scanner;

TYPE
  P = Stmt.T OBJECT
        cond    : Expr.T;
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
    Scanner.Match (TK.tWHILE);
    p.cond := Expr.Parse ();
    Scanner.Match (TK.tDO);
    p.body := Stmt.Parse ();
    Scanner.Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t: Type.T;
  BEGIN
    Expr.TypeCheck (p.cond, cs);
    t := Expr.TypeOf (p.cond);
    IF (Type.Base (t) # Bool.T) AND (t # ErrType.T) THEN
      Error.Msg ("WHILE condition must be a BOOLEAN");
    END;
    Marker.PushExit (CG.No_label);
    Stmt.TypeCheck (p.body, cs);
    Marker.Pop ();
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  top := CG.Next_label (3);
  BEGIN
    Marker.PushExit (top+2);
      CG.Jump (top+1);
      CG.Set_label (top);
      oc := Stmt.Compile (p.body);
      CG.Gen_location (p.origin);
      CG.Set_label (top+1);
      Expr.PrepBranch (p.cond, top, CG.No_label, CG.Likely);
      Expr.CompileBranch (p.cond, top, CG.No_label, CG.Likely);
      CG.Set_label (top+2);
    Marker.Pop ();

    (* A WHILE statement can always FallThrough; consider the case where 
       the condition is initially FALSE *)
    RETURN oc + Stmt.Outcomes {Stmt.Outcome.FallThrough}
              - Stmt.Outcomes {Stmt.Outcome.Exits};
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body)
            + Stmt.Outcomes {Stmt.Outcome.FallThrough}
            - Stmt.Outcomes {Stmt.Outcome.Exits};
  END GetOutcome;

BEGIN
END WhileStmt.
