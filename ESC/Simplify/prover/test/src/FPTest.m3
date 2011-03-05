(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Fri Jun  9 17:03:00 PDT 1995 by detlefs
*)

MODULE FPTest EXPORTS Main;

IMPORT ClausePrivate, Prover, Context;
IMPORT Sx, Atom, Stdio, Rd;

IMPORT Thread;
<*FATAL Thread.Alerted, Sx.ReadError, Prover.Error *>

VAR changed := Atom.FromText("changed");
    push := Atom.FromText("push");
    pop := Atom.FromText("pop");
    rd := Stdio.stdin;

BEGIN
  Prover.Init();
  VAR sx := Sx.Read(rd); <*NOWARN*>
      cl := ClausePrivate.SxToClause(sx);
      fp := ClausePrivate.FingerP(cl);
  BEGIN
    TRY
      LOOP
        VAR sx2 := Sx.Read(rd); BEGIN
          IF sx2 = push THEN
            Context.Push()
          ELSIF sx2 = pop THEN
            Context.Pop();
            <*ASSERT fp = ClausePrivate.FingerP(cl) *>
          ELSE
            VAR lit := ClausePrivate.SxToLiteral(sx2);
                b := Context.Assert(lit, NIL);
                flag := Sx.Read(rd);
            BEGIN
              <*ASSERT b*>
              <*ASSERT (flag = changed) # (fp = ClausePrivate.FingerP(cl)) *>
            END (* BEGIN *)
          END (* IF *)
        END (* BEGIN *)
      END (* LOOP *)
    EXCEPT
    | Rd.EndOfFile =>
        (* SKIP *)
    END (* TRY *)
  END (* BEGIN *)
END FPTest.
