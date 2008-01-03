(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Nov 18 12:00:10 PST 1994 by kalsow   *)
(*      modified on Wed Mar  4 13:00:44 PST 1992 by muller   *)

INTERFACE RTSignal;

PROCEDURE InstallHandlers ();
(* Install some basic signal handlers. *)

PROCEDURE RestoreHandlers ();
(* Restore the signal handlers modified by "InstallHandlers" to
   their initial state. *)

END RTSignal.
