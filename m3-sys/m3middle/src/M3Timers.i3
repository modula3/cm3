(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Timers.i3                                           *)
(* Last modified on Wed Nov 30 16:04:34 PST 1994 by kalsow     *)

INTERFACE M3Timers;

IMPORT ETimer;

VAR (*READONLY*)
  copy:     ETimer.T := NIL;  (* copying files *)
  remove:   ETimer.T := NIL;  (* removing temporary files *)
  pass_4:   ETimer.T := NIL;  (* indexing library archive *)
  pass_3:   ETimer.T := NIL;  (* building library archive *)
  exhale:   ETimer.T := NIL;  (* exhaling new link info *)
  pass_2:   ETimer.T := NIL;  (* linking *)
  genLink:  ETimer.T := NIL;  (* generating .M3LINK *)
  genMain:  ETimer.T := NIL;  (* generating _m3main.c *)
  chkpgm:   ETimer.T := NIL;  (* checking global consistency *)
  pass_1:   ETimer.T := NIL;  (* compiling C -> object *)
  pass_8:   ETimer.T := NIL;  (* patching object *)
  pass_7:   ETimer.T := NIL;  (* compiling assembly -> object *)
  pass_6:   ETimer.T := NIL;  (* compiling IL -> assembly *)
  emit:     ETimer.T := NIL;  (*   emitting code *)
  check:    ETimer.T := NIL;  (*   typechecking modules *)
  parse:    ETimer.T := NIL;  (*   parsing modules *)
  search:   ETimer.T := NIL;  (*   searching and opening imported files *)
  pass_0:   ETimer.T := NIL;  (* compiling Modula-3 -> IL *)
  merge:    ETimer.T := NIL;  (* merging new link info *)
  stalem3:  ETimer.T := NIL;  (* checking old link info *)
  staleobj: ETimer.T := NIL;  (* checking timestamps *)
  localobj: ETimer.T := NIL;  (* getting derived timestamps *)
  inhale:   ETimer.T := NIL;  (* inhaling library link info *)

PROCEDURE Start ();
(* allocate and intialize the timers *)

PROCEDURE Stop ();
(* dump the running timers *)

END M3Timers.
