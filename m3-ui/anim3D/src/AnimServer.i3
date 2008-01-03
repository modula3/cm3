(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Sep  4 15:22:46 PDT 1995 by najork                   *)
(*       Created on Tue May 24 11:23:24 PDT 1994 by najork                   *)


INTERFACE AnimServer;

IMPORT AnimHandle, GO, RootGO, Wr;

PROCEDURE RegisterRootGO (root : RootGO.T);
(* Adds a root to the set of "RootGO"s managed by the animation server. *)

PROCEDURE PauseAnimHandle (ah : AnimHandle.T);
(* Pauses an animation handle until its endtime has arrived. *)

PROCEDURE SetErrorWr (wr : Wr.T);
(* Set the writer to which animation server error messages will be written to
   be "wr". By default, error messages are written to "Stdio.stderr". *)

PROCEDURE ReportError (msg : TEXT);
(* Write an error message to the error message writer. The default error
   message write is "Stdio.stderr"; it can be changed with "SetErrorWr". *)

VAR
  internalLock : MUTEX;
  externalLock : MUTEX;

(* Locking Order: "externalLock" must be acquired before "internalLock". *)

PROCEDURE IsServer(): BOOLEAN;
(* Debugging procedure. Access to the X display connection must be 
   single-threaded. One way to ensure that is through a locking scheme 
   (Trestle does it that way); another way is by designating a single 
   thread to be the only one allowed to access the X connection. 
   We chose the second approach. The animation server thread is the 
   only thread allowed to call X and PEX proceures. We can ensure proper 
   calling patterns by inserting <* ASSERT AnimServer.IsServer() *> 
   pragmas before every X/PEX call. *)

PROCEDURE NewDisplayList (go: GO.T): INTEGER;
(* Returns a new display list identifier (a unique number > 0) *)

EXCEPTION SolverError (TEXT);

VAR 
  SolverHook: PROCEDURE (time: LONGREAL): BOOLEAN RAISES {SolverError} := NIL;
(* If "SolverHook" is non-NIL, it will be called by the animation server after
   events have been processed and before property values are adjusted.  
   The newest version of Obliq-3D uses "SolverHook" to interface with the
   Juno-2 constraint solver. *)

END AnimServer.
