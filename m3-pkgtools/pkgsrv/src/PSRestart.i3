(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* PSRestart.i3 *)
(* Last modified on Tue May  4 17:36:05 PDT 1993 by wobber *)
(*      modified on Fri Dec  7 11:30:14 GMT+1:00 1990 by prusker *)

INTERFACE PSRestart;

PROCEDURE Recover (init: BOOLEAN);
  (* this forks a thread which attempts to bring
     the local package database is up to date *)

END PSRestart.
