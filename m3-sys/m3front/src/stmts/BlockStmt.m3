(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BlockStmt.m3                                          *)
(* Last modified on Fri Jun 24 15:49:52 PDT 1994 by kalsow     *)
(*      modified on Fri Feb 23 07:15:45 1990 by muller         *)

MODULE BlockStmt;

IMPORT M3ID, Scope, Token, Stmt, StmtRep, Scanner, Decl, ESet, Tracer;
IMPORT CaptureAnalysis, Variable, Value, Expr, Procedure;
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
        compileMSIR := CompileMSIR;
        capture  := Capture;
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

PROCEDURE GetScope (t: Stmt.T): Scope.T =
  BEGIN
    TYPECASE t OF
    | P(p) => RETURN p.scope;
    ELSE      RETURN NIL;
    END;
  END GetScope;

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

PROCEDURE CompileMSIR (p: P) =
  BEGIN
    IF p.scope # NIL THEN
      (* Pre-register the captures of block-local nested procedures before their
         bodies compile, so a proc used as a value by an earlier-compiled sibling
         gets a closure shim with the correct capture args (p035 group O: a
         block-local `bar` capturing `i`, used as a value by `foo`). *)
      Procedure.PreRegisterScopeCapturesMSIR (p.scope);
      Scope.InitValues (p.scope);
    END;
    Stmt.CompileMSIR (p.body);
  END CompileMSIR;

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

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  VAR v: Value.T;
  BEGIN
    (* Walk this block's local-variable initializers so an up-level variable
       referenced ONLY inside an initializer (e.g. `VAR a := srcs[i]`, where
       srcs belongs to an enclosing proc) is discovered as a capture.  The
       body statements alone miss these — the classic case is a nested compare
       or emit proc whose only use of an outer VAR is in binding its own locals
       (M3Build.CmpUnit/Emit).  FilterOwnScope later removes this block's own
       locals from the capture set; the outer vars their initializers read
       survive. *)
    IF p.scope # NIL THEN
      v := Scope.ToList (p.scope);
      WHILE v # NIL DO
        TYPECASE v OF
        | Variable.T (var) => Expr.Capture (Variable.InitExpr (var), ca);
        ELSE
        END;
        v := v.next;
      END;
    END;
    Stmt.Capture (p.body, ca);
  END Capture;

BEGIN
END BlockStmt.
