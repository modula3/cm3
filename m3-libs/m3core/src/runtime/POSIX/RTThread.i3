(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Sat Nov 19 10:31:40 PST 1994 by kalsow    *)
(*      modified on Wed Dec 23 15:35:41 PST 1992 by jdd       *)
(*      modified on Thu Nov 12 14:06:13 PST 1992 by muller    *)

INTERFACE RTThread;

IMPORT Usignal, RTMachine;

(*--------------------------------------------------------- machine state ---*)

TYPE
  State = RTMachine.State;
  (* The machine state is saved in a State.  This type is really opaque
     to the client, i.e. it does not need to be an array. *)

CONST
  Save = RTMachine.SaveState;
  (* Captures the currently running thread's state *)

<*EXTERNAL "RTThread__Transfer"*>
PROCEDURE Transfer (VAR from, to: State);
  (* Records the current machine state in "from" and sets
     the machine state to that contained in "to". *)

PROCEDURE SP (READONLY s: State): ADDRESS;
  (* Returns the stack pointer associated with s *)

(*--------------------------------------------------------- thread stacks ---*)

CONST (* additional padding words to copy when creating a new thread *)
  FramePadBottom = RTMachine.FramePadBottom;
  FramePadTop    = RTMachine.FramePadTop;

TYPE
  StackSpace = UNTRACED REF ARRAY OF INTEGER;
  Stack = RECORD
    words : StackSpace;
    first : ADDRESS;
    last  : ADDRESS;
  END;
  (* The range of usable addresses in "bytes^" is "[first .. last)". *)

CONST
  PointerAlignment = RTMachine.PointerAlignment;
  (* The C compiler allocates all pointers on "PointerAlignment"-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

  StackFrameAlignment = RTMachine.StackFrameAlignment;
  (* Stack frames must be aligned to this constraint (in ADRSIZE units). 
     It's not a big deal if this value is too large, but it may break 
     the thread mechanism to make it too small. *)

PROCEDURE GetStack (size: INTEGER;  VAR(*OUT*) s: Stack);
(* Acquire a thread stack with at least "size" usable "INTEGER"s of storage,
   from the current pool.  If necessary, allocate a new stack. *)

PROCEDURE FreeStack (VAR(*IN/OUT*) s: Stack);
(* Return "s" to the free pool of stacks. *)

PROCEDURE NewStack (size: INTEGER;  VAR(*OUT*) s: Stack);
(* Allocate a new thread stack with at least "size" usable "INTEGER"s
   of storage,  if possible unmap its guard page, and return it in "s". *)

PROCEDURE DisposeStack (VAR(*IN/OUT*) s: Stack);
(* Dispose of "s" and remap its guard page. *)

PROCEDURE FlushStackCache ();
(* Ensure that the in-memory contents of the stack are up-to-date.
   On some machines, the stack is cached; for example, on SPARC, the stack
   is partially cached in the registers. *)

(*-------------------------------------------------- modifying the models ---*)

(* When a new thread is forked, the contents of its stack and its state
   are built from models.  The only difference with the model is the location
   of the stack.  The two procedures below are used to modify the values of 
   the model to point to the correct state.  offset is the difference 
   between the actual stack location and the model stack location *)

PROCEDURE UpdateStateForNewSP (VAR s: State;  offset: INTEGER);
(* Update the state *)

PROCEDURE UpdateFrameForNewSP (a: ADDRESS;  offset: INTEGER);
(* Update the stack frame.  "a" is the correct value for the "SP"
     on the stack that contains that frame. *)

(*------------------------------------ manipulating the SIGVTALRM handler ---*)

PROCEDURE setup_sigvtalrm (handler: Usignal.SignalHandler);
PROCEDURE allow_sigvtalrm ();
PROCEDURE disallow_sigvtalrm ();

(*---------------------------------------------------- exception delivery ---*)

<*EXTERNAL "RTThread__handlerStack" *>
VAR handlerStack: ADDRESS;
(* linked list of exception frames. *)

END RTThread.
