(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Param.i3 *)
(* Last modified on Thu Nov 19 14:15:22 PST 1992 by wobber  *)
(*      modified on Fri Jul  3  2:05:08 GMT+2:00 1992 by prusker *)

INTERFACE Param;

(* contains global constant variables, and tunable constants *)

IMPORT ServerLog, LockOps;

CONST
  SystemAuth        = "pkgsrvr";

    (* ServerLog stats *)
    (* should be merged with other stats *)
CONST
    StatSend     = 0;    (* total packages sent *)
    StatRecv     = 1;    (* total packages received *)
    StatCurrSend = 2;    (* current packages sending *)
    StatCurrRecv = 3;    (* current packages receiving *)
    StatQueued   = 4;    (* # packages in all send queues *)

VAR (* fix *)
  log:          ServerLog.T;
  localSite:    LockOps.SiteName;

PROCEDURE StatIncr(which: CARDINAL);
PROCEDURE StatDecr(which: CARDINAL);

END Param.
