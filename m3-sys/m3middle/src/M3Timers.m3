(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Timers.m3                                           *)
(* Last modified on Wed Nov 30 16:04:57 PST 1994 by kalsow     *)

MODULE M3Timers;

IMPORT ETimer, Stdio;

PROCEDURE Start () =
  (* note: we allocate the timers in reverse order of printout *)
  BEGIN
    IF (pass_0 # NIL) THEN RETURN END;

    copy     := ETimer.New ("copying files");
    remove   := ETimer.New ("removing temporary files");
    pass_4   := ETimer.New ("indexing library archive");
    pass_3   := ETimer.New ("building library archive");
    exhale   := ETimer.New ("exhaling new link info");
    pass_2   := ETimer.New ("linking");
    genLink  := ETimer.New ("generating .M3LINK");
    genMain  := ETimer.New ("generating _m3main.c");
    chkpgm   := ETimer.New ("checking global consistency");
    pass_1   := ETimer.New ("compiling C -> object");
    pass_8   := ETimer.New ("patching object");
    pass_7   := ETimer.New ("compiling assembly -> object");
    pass_6   := ETimer.New ("compiling IL -> assembly");
    emit     := ETimer.New ("  emitting code");
    check    := ETimer.New ("  typechecking modules");
    parse    := ETimer.New ("  parsing modules");
    search   := ETimer.New ("  searching and opening imported files");
    pass_0   := ETimer.New ("compiling Modula-3 -> IL");
    merge    := ETimer.New ("merging new link info");
    stalem3  := ETimer.New ("checking old link info");
    staleobj := ETimer.New ("checking timestamps");
    localobj := ETimer.New ("getting derived timestamps");
    inhale   := ETimer.New ("inhaling library link info");

    ETimer.Enable ();
  END Start;

PROCEDURE Stop () =
  BEGIN
    IF (pass_0 # NIL) THEN ETimer.Dump (Stdio.stdout) END;
  END Stop;

BEGIN
END M3Timers.
