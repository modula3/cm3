(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Skulker.i3 *)
(* Last modified on Tue May  4 17:42:11 PDT 1993 by wobber  *)
(*      modified on Tue Jun 23 13:48:36 GMT+2:00 1992 by prusker *)

INTERFACE Skulker;

IMPORT PackageObj, Siphon, Thread;

(* Init starts a daemon which periodically sychronize the version of all
   shared packages, and does some consistency checks; the first synch starts
   immediately *)

PROCEDURE Synch(synchKind: Siphon.SynchKind; pn: PackageObj.PN): TEXT
  RAISES {Thread.Alerted};
  (* writes actions to logfile (except if check only), prefixed by Skulk *)
  (* text returned includes everything, actions and checks *)

PROCEDURE Init() RAISES {};

END Skulker.
