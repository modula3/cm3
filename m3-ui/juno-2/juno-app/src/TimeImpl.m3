(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 20 21:07:30 PDT 1995 by heydon                   *)

MODULE TimeImpl;

IMPORT ExternalProc;
FROM ExternalProc IMPORT Closure, Bind;
IMPORT JunoScope;
IMPORT JunoArgs, JunoValue;
IMPORT Atom, Time;

VAR (*CONST*)
  TimeSym := Atom.FromText("Time");
  NowSym  := Atom.FromText("Now");

VAR
  startT: Time.T;

PROCEDURE New(): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 6);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(TimeSym, scp);
    Bind(NowSym, NEW(Closure, invoke := NowProc), in := 0, out := 1);
    RETURN res
  END New;

PROCEDURE NowProc(<*UNUSED*> cl: Closure): BOOLEAN =
  BEGIN
    JunoArgs.WriteReal(1, FLOAT(Time.Now() - startT, JunoValue.Real));
    RETURN TRUE
  END NowProc;

BEGIN
  startT := Time.Now()
END TimeImpl.
