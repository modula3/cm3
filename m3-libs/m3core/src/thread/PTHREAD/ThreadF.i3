(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Apr  7 09:11:10 PDT 1995 by kalsow     *)
(*      modified on Fri May 14 16:18:01 PDT 1993 by mjordan    *)
(*      modified on Mon Apr  5 14:50:26 PDT 1993 by muller     *)
(*      modified on Mon Jul  6 16:43:19 PDT 1992 by muller     *)

INTERFACE ThreadF;

IMPORT FloatMode, RTHeapRep;

(*--------------------------------------------- garbage collector support ---*)

PROCEDURE SuspendOthers ();
(* Suspend all threads except the caller's *)

PROCEDURE ResumeOthers ();
(* Resume the threads suspended by "SuspendOthers" *)

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS));
(* Apply p to each thread stack, with [start..stop) being the limits
   of the stack.  All other threads must be suspended.  ProcessStacks
   exists solely for the garbage collector.  *)

PROCEDURE ProcessPools (p: PROCEDURE (VAR pool: RTHeapRep.AllocPool));
(* Apply p to each thread allocation pool.  All other threads must be
   suspended.  ProcessPools exists solely for the garbage collector.  *)

PROCEDURE MyAllocPool (): UNTRACED REF RTHeapRep.AllocPool;

(*------------------------------------------------ floating point support ---*)

(* access to the saved floating point state for the current thread. *)
PROCEDURE GetMyFPState (reader: PROCEDURE(READONLY s: FloatMode.ThreadState));
PROCEDURE SetMyFPState (writer: PROCEDURE(VAR s: FloatMode.ThreadState));

(*-------------------------------------------------- showthreads support ---*)

TYPE
  State = {
        alive    (* can run *),
        waiting  (* waiting for a condition via Wait *),
        locking  (* waiting for a mutex to be unlocked *),
        pausing  (* waiting until some time is arrived *),
        blocking (* waiting for some IO *),
        dying    (* done, but not yet joined *),
        dead     (* done and joined *)
	};

TYPE
  Id = INTEGER;

(*-------------------------------------------------------------- identity ---*)

PROCEDURE MyId(): Id RAISES {};
(* return Id of caller *)

(*---------------------------------------------------- exception delivery ---*)

PROCEDURE GetCurrentHandlers(): ADDRESS;
(* == RETURN Upthread.getspecific(handlersIndex) *)

PROCEDURE SetCurrentHandlers(h: ADDRESS);
(* == Upthread.setspecific(handlersIndex, h) *)

PROCEDURE Init();

END ThreadF.
