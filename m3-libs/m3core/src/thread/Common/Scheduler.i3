(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified On Wed Apr 21 13:12:45 PDT 1993 by mcjones *)
(*      modified On Thu Jan 28 10:26:02 PST 1993 by mjordan *)

(* The "Scheduler" interface allows a thread some control over its
   rate of execution.  *)

INTERFACE Scheduler;

PROCEDURE Yield();
(* If there are other threads ready to run, transfer control to one
   of them; otherwise continue with the current thread.  *)

(* Implementation note: the exact semantics of "Yield" varies widely
   from system to system.  You shouldn't use it without consulting the
   detailed documentation for your implementation. *)

END Scheduler.
