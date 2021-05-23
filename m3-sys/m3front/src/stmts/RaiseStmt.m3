(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RaiseStmt.m3                                          *)
(* Last modified on Fri Jun 24 15:50:11 PDT 1994 by kalsow     *)
(*      modified on Sat Jan  5 04:47:38 1991 by muller         *)

MODULE RaiseStmt;

IMPORT M3ID, Expr, Token, Scanner, Stmt, StmtRep, Error, ESet;
IMPORT Value, Type, Scope, Exceptionz, AssignStmt;
FROM M3 IMPORT QID;

TYPE
  P = Stmt.T OBJECT
        scope   : Scope.T;
        qid     : QID;
        except  : Value.T;
        arg     : Expr.T;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Scanner.Match (TK.tRAISE);
    p.scope      := Scope.Top ();
    p.except     := NIL;
    p.arg        := NIL;
    p.qid.module := M3ID.NoID;
    p.qid.item   := Scanner.MatchID ();
    IF (Scanner.cur.token = TK.tDOT) THEN
      Scanner.GetToken (); (* . *)
      p.qid.module := p.qid.item;
      p.qid.item := Scanner.MatchID ();
    END;
    IF (Scanner.cur.token = TK.tLPAREN) THEN
      Scanner.GetToken ();  (* ( *)
      p.arg := Expr.Parse ();
      Scanner.Match (TK.tRPAREN);
    END;
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t, u: Type.T;  v: Value.T;
  BEGIN
    Expr.TypeCheck (p.arg, cs);
    t := Expr.TypeOf (p.arg);

    v := Scope.LookUpQID (p.scope, p.qid);
    Value.TypeCheck (v, cs);
    IF (v = NIL) THEN
      Error.QID (p.qid, "undefined");
    ELSIF (Value.ClassOf (v) # Value.Class.Exception) THEN
      Error.QID (p.qid, "not an exception");
    ELSE
      p.except := v;
      ESet.NoteException (cs, v);
    END;

    u := Exceptionz.ArgType (p.except);
    IF (p.except = NIL) THEN
      (* we've already generated an error... *)
      Expr.TypeCheck (p.arg, cs);
    ELSIF (u = NIL) THEN
      (* takes no argument *)
      IF (p.arg # NIL) THEN
        Error.QID (p.qid, "exception takes no argument");
      END;
    ELSIF (p.arg = NIL) THEN
      Error.QID (p.qid, "exception requires an argument");
    ELSIF NOT Type.IsAssignable (u, t) THEN
      Error.QID (p.qid, "argument has wrong type");
    ELSE
      (* We want p.arg compiled into a temp -> No NoteUseTargetVar *)
      AssignStmt.Check (u, p.arg, cs);
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Exceptionz.EmitRaise (p.except, p.arg);
    RETURN Stmt.Outcomes {(* Raises *)};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {(* Raises *)};
  END GetOutcome;

BEGIN
END RaiseStmt.
