(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IfStmt.m3                                             *)
(* Last modified on Thu Nov 10 13:30:29 PST 1994 by kalsow     *)
(*      modified on Wed Feb 27 04:00:55 1991 by muller         *)

MODULE IfStmt;

IMPORT CG, Expr, Bool, Type, Error, Token, Stmt, StmtRep, Scanner, ErrType;
FROM Scanner IMPORT Match, GetToken, cur;

TYPE
  P = Stmt.T OBJECT
        clauses  : Clause;
        elseBody : Stmt.T;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

TYPE
  Clause = REF RECORD
             origin : INTEGER;
             next   : Clause;
             cond   : Expr.T;
             body   : Stmt.T;
           END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p := NEW (P);  c, last: Clause;
  BEGIN
    StmtRep.Init (p);

    Match (TK.tIF);
    c := NEW (Clause);
    c.origin := Scanner.offset;
    c.next := NIL;
    c.cond := Expr.Parse ();
    Match (TK.tTHEN);
    c.body := Stmt.Parse ();
    p.clauses := c;
    p.elseBody := NIL;
    last := c;

    WHILE (cur.token = TK.tELSIF) DO
      GetToken (); (* ELSIF *)
      c := NEW (Clause);
      c.origin := Scanner.offset;
      c.next := NIL;
      c.cond := Expr.Parse ();
      Match (TK.tTHEN);
      c.body := Stmt.Parse ();
      last.next := c;
      last := c;
    END;

    IF (cur.token = TK.tELSE) THEN
      GetToken (); (* ELSE *)
      p.elseBody := Stmt.Parse ();
    END;

    Match (TK.tEND);
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR c: Clause;  t: Type.T;
  BEGIN
    c := p.clauses;
    WHILE (c # NIL) DO
      Expr.TypeCheck (c.cond, cs);
      t := Expr.TypeOf (c.cond);
      IF (Type.Base (t) # Bool.T) AND (t # ErrType.T) THEN
        Scanner.offset := c.origin;
        Error.Msg ("IF condition must be a BOOLEAN");
      END;
      Stmt.TypeCheck (c.body, cs);
      c := c.next;
    END;
    Stmt.TypeCheck (p.elseBody, cs);
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    c       : Clause;
    l_end   : CG.Label;
    l_next  : CG.Label;
    oc, xc  : Stmt.Outcomes;
    gotoEnd := FALSE;
  BEGIN
    l_end := CG.Next_label ();

    c := p.clauses;
    oc := Stmt.Outcomes {};
    WHILE (c # NIL) DO
      l_next := CG.Next_label ();
      Scanner.offset := c.origin;
      CG.Gen_location (Scanner.offset);
      Expr.PrepBranch (c.cond, CG.No_label, l_next, CG.Always - CG.Likely);
      Expr.CompileBranch (c.cond, CG.No_label, l_next, CG.Always - CG.Likely);
      xc := Stmt.Compile (c.body);
      oc := oc + xc;
      IF (Stmt.Outcome.FallThrough IN xc)
        AND ((c.next # NIL) OR (p.elseBody # NIL)) THEN
        CG.Jump (l_end);
	gotoEnd := TRUE;
      END;
      CG.Set_label (l_next);
      c := c.next;
    END;

    IF (p.elseBody = NIL)
      THEN oc := oc + Stmt.Outcomes {Stmt.Outcome.FallThrough};
      ELSE oc := oc + Stmt.Compile (p.elseBody);
    END;

    IF (gotoEnd) THEN CG.Set_label (l_end) END;
    RETURN oc;
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  VAR c: Clause;  oc := Stmt.Outcomes {};
  BEGIN
    c := p.clauses;
    WHILE (c # NIL) DO
      oc := oc + Stmt.GetOutcome (c.body);
      c := c.next;
    END;
    IF (p.elseBody = NIL)
      THEN oc := oc + Stmt.Outcomes {Stmt.Outcome.FallThrough};
      ELSE oc := oc + Stmt.GetOutcome (p.elseBody);
    END;
    RETURN oc;
  END GetOutcome;

BEGIN
END IfStmt.
