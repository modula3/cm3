(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BlockStmt.m3                                          *)
(* Last modified on Fri Jun 24 15:49:52 PDT 1994 by kalsow     *)
(*      modified on Fri Feb 23 07:15:45 1990 by muller         *)

MODULE BlockStmt;

IMPORT M3ID, Scope, Token, Stmt, StmtRep, Scanner, Decl, ESet, Tracer;
FROM Scanner IMPORT Match, cur;

TYPE
  P = Stmt.T OBJECT
	scope   : Scope.T;
        body    : Stmt.T;
        fails   : ESet.T;
        trace   : TraceNode;
      OVERRIDES
        check       := Check;
	compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (needScope: BOOLEAN): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    p.fails := NIL;

    IF (needScope)
      THEN p.scope := Scope.PushNew (TRUE, M3ID.NoID, nested := TRUE);
      ELSE p.scope := NIL;
    END;

    WHILE (cur.token IN Token.DeclStart) DO
      Decl.Parse (FALSE, FALSE, p.fails);
    END;

    Match (TK.tBEGIN);
      p.trace := ParseTrace ();
      p.body := Stmt.Parse ();
    Match (TK.tEND);

    IF (needScope) THEN Scope.PopNew () END;
    RETURN p;
  END Parse;

PROCEDURE ExtractFails (t: Stmt.T): ESet.T =
  VAR x: ESet.T;
  BEGIN
    TYPECASE t OF
    | NULL =>  RETURN NIL;
    | P(p) =>  x := p.fails;  p.fails := NIL;  RETURN x;
    ELSE       RETURN NIL;
    END;
  END ExtractFails;

PROCEDURE BodyOffset (t: Stmt.T): INTEGER =
  BEGIN
    TYPECASE t OF
    | NULL =>  RETURN Scanner.offset;
    | P(p) =>  IF (p.body # NIL)
                 THEN RETURN p.body.origin;
                 ELSE RETURN Scanner.offset;
               END;
    ELSE       RETURN Scanner.offset;
    END;
  END BodyOffset;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR old, new: Scope.T;
  BEGIN
    new := p.scope;
    IF (new # NIL) THEN old := Scope.Push (new) END;
    ESet.TypeCheck (p.fails);
    ESet.Push (cs, NIL, p.fails, stop := FALSE);
      IF (new # NIL) THEN Scope.TypeCheck (new, cs) END;
      IF (p.trace # NIL) THEN Stmt.TypeCheck (p.trace.body, cs) END;
      Stmt.TypeCheck (p.body, cs);
      IF (new # NIL) THEN Scope.WarnUnused (new) END;
    ESet.Pop (cs, NIL, p.fails, stop := FALSE);
    IF (new # NIL) THEN Scope.Pop (old) END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR oc: Stmt.Outcomes;  zz: Scope.T;
  BEGIN
    IF (p.scope # NIL) THEN
      zz := Scope.Push (p.scope);
      Scope.Enter (p.scope);
      Scope.InitValues (p.scope);
      Tracer.Push (p.trace);
      oc := Stmt.Compile (p.body);
      Tracer.Pop (p.trace);
      Scope.Exit (p.scope);
      Scope.Pop (zz);
    ELSE
      Tracer.Push (p.trace);
      oc := Stmt.Compile (p.body);
      Tracer.Pop (p.trace);
    END;
    RETURN oc;
  END Compile;

PROCEDURE GetOutcome (p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.GetOutcome (p.body);
  END GetOutcome;

(*------------------------------------------------------- tracing support ---*)

TYPE TraceNode = Tracer.T OBJECT body: Stmt.T OVERRIDES apply := DoTrace END;

PROCEDURE ParseTrace (): Tracer.T =
  TYPE TK = Token.T;
  VAR s: Stmt.T;
  BEGIN
    IF (cur.token # TK.tTRACE) THEN RETURN NIL END;
    Match (TK.tTRACE);
    s := Stmt.Parse ();
    Match (TK.tENDPRAGMA);
    IF (s = NIL) THEN RETURN NIL END;
    RETURN NEW (TraceNode, body := s);
  END ParseTrace;

PROCEDURE DoTrace (x: TraceNode) =
  BEGIN
    EVAL Stmt.Compile (x.body);
  END DoTrace;

PROCEDURE CheckTrace (tt: Tracer.T;  VAR cs: Stmt.CheckState) =
  VAR x: TraceNode := tt;
  BEGIN
    IF (tt = NIL) THEN RETURN END;
    Stmt.TypeCheck (x.body, cs);
  END CheckTrace;

BEGIN
END BlockStmt.
