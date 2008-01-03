(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu May  4 09:11:40 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 25 18:20:47 PDT 1992 by muller     *)

UNSAFE INTERFACE RTStack;

IMPORT RTMachine;

(* This interface defines the low-level routines used to traverse
   the runtime stack.  Not all platforms support these routines.
*)

CONST Has_walker = RTMachine.Has_stack_walker;
(* Indicates whether this platform has a stack walker.  If "Has_walker"
   is "FALSE", it is a checked runtime error to call any of the routines
   described below. *)

TYPE Frame = RTMachine.FrameInfo;
(* A machine-dependent type that minimally includes fields named "pc"
   and "sp". *)

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

END RTStack.

