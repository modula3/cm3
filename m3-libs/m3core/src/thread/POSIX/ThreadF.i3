(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Apr  7 09:11:10 PDT 1995 by kalsow     *)
(*      modified on Fri May 14 16:18:01 PDT 1993 by mjordan    *)
(*      modified on Mon Apr  5 14:50:26 PDT 1993 by muller     *)
(*      modified on Mon Jul  6 16:43:19 PDT 1992 by muller     *)

INTERFACE ThreadF;

IMPORT FloatMode, Thread;

(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers(): ADDRESS;
(* == RETURN RTThread.handlerStack *)

PROCEDURE SetCurrentHandlers(h: ADDRESS);
(* == RTThread.handlerStack := h *)

(*--------------------------------------------- garbage collector support ---*)

PROCEDURE SuspendOthers ();
(* Suspend all threads except the caller's *)

PROCEDURE ResumeOthers ();
(* Resume the threads suspended by "SuspendOthers" *)

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS));
(* Apply p to each thread stack, with start and stop being the limits
   of the stack.  All other threads must be suspended.  ProcessStacks
   exists solely for the garbage collector.  *)

(*------------------------------------------------ floating point support ---*)

PROCEDURE MyFPState (): UNTRACED REF FloatMode.ThreadState;
(* returns the saved floating point state for the current thread.
   WARNING: the return value is an untraced pointer to a traced Thread.T!!  *)

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

(*--------------------------------------------------------- hooks support ---*)

(* PRIVATE VAR hooks: Hooks := NIL *)

TYPE
  Hooks = OBJECT METHODS
    fork (t: Thread.T);  (* called with RT0u.inCritical > 0 *)
    die  (t: Thread.T);  (* called with RT0u.inCritical > 0 *)
  END;

PROCEDURE RegisterHooks (h: Hooks; init := TRUE): Hooks RAISES {};
(* return current hooks and set hooks := h.   If init is true, 
   call hooks.fork (t) for every thread t in the ring in a single
   critical section. *)

PROCEDURE MyId(): Id RAISES {};
(* return Id of caller *)

<*EXTERNAL "ThreadF__myId"*>
VAR
  myId: Id;
  (* The id of the currently running thread *)

(*------------------------------------------------------------ preemption ---*)

PROCEDURE SetSwitchingInterval (usec: CARDINAL);
(* Sets the time between thread preemptions to 'usec' microseconds.
   Note that most Unix systems dont guarantee much if any precision
   on timer interrupts.  The default value is 100 milliseconds. *)

END ThreadF.
