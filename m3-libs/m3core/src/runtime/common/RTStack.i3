(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu May  4 09:11:40 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 25 18:20:47 PDT 1992 by muller     *)

UNSAFE INTERFACE RTStack;

(* This interface defines the low-level routines used to traverse
 * the runtime stack.  Not all platforms support these routines.
 *
 * NOTE: The meanings here have changed.
 * "Has_walker" used to mean:
 *  1 Modula-3 backend generates exception handling metadata.
 *    Instead of using slowly maintaining a thread local linked list.
 *    Well, hypothetical NT/x86 backend's best case is
 *    a highly optimized thread local link list, through FS:0.
 *  2 Modula-3 runtime could walk stack for exception handling.
 *  3 Modula-3 runtime could walk stack for printing a stack back trace.
 *
 * Now it means:
 *  1 Modula-3 backend generates exception handling metadata.
 *    Typically via the C++ backend where it is relatively easy.
 *  2 Does not apply. The C++ runtime does this, perhaps via shared OS cross
 *    language functionality (e.g. Windows: RaiseException leading to
 *    internally RtlVirtualUnwind and public RtlUwind).
 *  3 Modula-3 runtime could walk stack for printing a stack back trace.
 *    But these can also just always fail such as by returning pc=NIL.
 *    Unwind always fails, even on modern systems with walker.
 *    Implementing Unwind in Modula-3 runtime would break language interoperation.
 *
 * As such, "has walker" can be considered equivalent to "using C++ backend",
 * or perhaps other cases also.
 *
 * So "has_walker" does not mean much.
 * We could possibly link in ex_frame and ex_stack and use has_walker
 * to decide between them at raise time, but how to throw generally
 * can be guided by the backend. To output the slow frame maintenance
 * calls or not and you don't want to slow own ex_stack systems like that.
 * It only therefore makes residual sense if some system has a broken
 * or missing C++ runtime and you have some sorta of pessimized
 * dual bootstrap form.
 *)

<*EXTERNAL "RTStack_Has_walker"*>
PROCEDURE Has_walker (): BOOLEAN;
(* Indicates whether this platform has a stack walker.  If "Has_walker"
   is "FALSE", it is a checked runtime error to call any of the routines
   described below. *)

TYPE
  Frame = RECORD
    pc  : ADDRESS;
    sp  : ADDRESS;
    lock: INTEGER;        (* to ensure that cxt isn't overrun!! *)
    excRef  : ADDRESS;    (* ref to the exception activation *)
    tTypeIndex : INTEGER; (* tTypeIndex from the unwinder *)
    cursor : ADDRESS;     (* libunwind cursor to cur frame *)
    startIP : ADDRESS;    (* libunwind start ip of current proc *)
    endIP : ADDRESS;      (* libunwind end ip of current proc *)
    lsda : ADDRESS;       (* libunwind lsda *)
    persFn : ADDRESS;     (* libunwind handler pers fn *)
    landingPad : ADDRESS; (* libunwind landing pad - the IP of the handler *)
  END;
(* A stack frame, as the walker reports it.  Callers use only "pc" and
   "sp"; the rest is the walker's state.  The layout must match "Frame" in
   walker/RTStackC.c.  It used to be RTMachine.FrameInfo, from a time when
   it differed between targets; it no longer does. *)

<*EXTERNAL "RTStack__GetThreadFrame" *>
PROCEDURE GetThreadFrame (VAR(*OUT*) f: Frame;  start: ADDRESS;  len: INTEGER);
(* Return in "f" the frame of the thread whose machine state is in bytes
   [start .. start+len).  Returns with f.pc=NIL on failure. *)

<*EXTERNAL "RTStack__CurFrame" *>
PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame);
(* Return in "f" the frame of its caller.  Returns with f.pc=NIL on failure.*)

<*EXTERNAL "RTStack__PrevFrame" *>
PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*)caller: Frame);
(* Return in "caller" the stack frame that called "callee".
   Returns with pc = NIL if  "callee" is the first frame on
   the stack or its predecessor is ill-formed. *)

<*EXTERNAL "RTStack__Unwind" *>
PROCEDURE Unwind (READONLY f: Frame);
(* Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call.  Note that if the unwind operation encounters a
   signal handler frame, it must also restore the caller-saved registers. *)

<*EXTERNAL "RTStack__ProcName" *>
PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
(* Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. *)

<*EXTERNAL "RTStack__ThrowM3Exc" *>
PROCEDURE ThrowM3Exc (act: ADDRESS);
(* Throw a C++ _M3Exc carrying "act" (a pointer to a RT0.RaiseActivation)
   so that native C++ EH unwind dispatches to the appropriate TRY/EXCEPT
   or TRY/FINALLY handler.  Only meaningful when Has_walker () = TRUE and the
   M3C backend generates C++ try/catch for exception handling. *)

<*EXTERNAL "RTStack__InstallTerminateHandler" *>
PROCEDURE InstallTerminateHandler ();
(* Make an _M3Exc that no handler catches report "Unhandled exception" through
   RTException.ReportUnhandled, instead of terminating in the C++ runtime.
   Called once, from RTExStack's initialization; ex_stack only, like
   ThrowM3Exc. *)

END RTStack.
