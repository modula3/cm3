(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb  4 11:33:17 PST 2000 by heydon                   *)

MODULE PrintImpl;

IMPORT ExternalProc;
FROM ExternalProc IMPORT Closure, Bind;
IMPORT JunoScope, JunoValue, RTVal;
IMPORT JunoArgs;
IMPORT Atom, Stdio, Thread, Wr;

<* FATAL Thread.Alerted, Wr.Failure *>

VAR (*CONST*)
  PrintAtom := Atom.FromText("Print");
  Val := Atom.FromText("Val");

PROCEDURE New(): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 6);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(PrintAtom, scp);
    Bind(Val, NEW(Closure, invoke := ValProc), in := 1, out := 0);
    RETURN res
  END New;

PROCEDURE ValProc(<*UNUSED*> cl: Closure): BOOLEAN =
  BEGIN
    JunoValue.Unparse(Stdio.stdout, RTVal.ToJV(JunoArgs.ReadValue(1)));
    Wr.PutChar(Stdio.stdout, '\n');
    Wr.Flush(Stdio.stdout);
    RETURN TRUE
  END ValProc;

BEGIN
END PrintImpl.
