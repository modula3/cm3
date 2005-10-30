(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)
(*      modified on Tue May  2 11:43:35 PDT 1995 by kalsow  *)

(* This interface defines platform (machine + OS) dependent
   types and constants. *)

INTERFACE RTMachine;

IMPORT Uucontext, Usignal;
FROM Upthread IMPORT pthread_t;

(*--------------------------------------------------------- thread state ---*)

TYPE
  State = Uucontext.ucontext_t;
  (* The machine state is saved in a "State".  This type is really
     opaque to the client, i.e. it does not need to be an array. *)

<*EXTERNAL "getcontext" *>
PROCEDURE SaveState (VAR s: State): INTEGER;
(* Capture the currently running thread's state *)

CONST
  FramePadBottom = 2;
  FramePadTop    = 0;
  (* Additional padding words from above and below an existing
     thread's stack pointer to copy when creating a new thread *)

(*------------------------------------------------------------------ heap ---*)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerHeapPage    = 8192;        (* bytes per page *)
  LogBytesPerHeapPage = 13;
  AdrPerHeapPage      = 8192;        (* addresses per page *)
  LogAdrPerHeapPage   = 13;

(*** hooks for the C wrapper functions ***)

<*EXTERNAL*> VAR RTHeapRep_Fault: ADDRESS;  (* => RTHeapRep.Fault *)
<*EXTERNAL*> VAR RTCSRC_FinishVM: ADDRESS;  (* => RTCollectorSRC.FinishVM *)

(*** hooks for the stack walker ***)
<*EXTERNAL*> VAR RTProcedureSRC_FromPC: ADDRESS; (* => RTProcedureSRC.FromPC *)
<*EXTERNAL*> VAR RTHeapDep_Fault: ADDRESS;  (* => RTHeapDep.Fault *)

(*--------------------------------------------------------- thread stacks ---*)

CONST
  PointerAlignment = 4;
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries.  The garbage collector scans thread stacks, but only
     looks at these possible pointer locations.  Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

CONST
  StackFrameAlignment = 8;
  (* Stack frames must be aligned to this constraint (in ADRSIZE units). 
     It's not a big deal if this value is too large, but it may break 
     the thread mechanism to make it too small. *)

(*----------------------------------------------- exception stack walking ---*)
(* The "FrameInfo" type must minimally include fields named "pc" and "sp". *)

CONST
  Has_stack_walker = TRUE;
  (* Indicates whether this platform supports the stack walking functions
     defined in the "RTStack" interface. *)

TYPE
  FrameInfo = RECORD
    pc, sp  : ADDRESS;       (* sp here is actually SPARC fp *)
    true_sp : ADDRESS;
    ctxt    : Uucontext.ucontext_t;
    lock    : INTEGER;       (* to ensure that ctxt isn't overrun!! *)
  END;

(*------------------------------------------------------ pthreads support ---*)

(* Full context is in the signal handler frame so no need for state here. *)
TYPE ThreadState = RECORD END;

CONST
  SIG_SUSPEND = Usignal.SIGUSR1;
  SIG_RESTART = Usignal.SIGUSR2;
  SuspendThread: PROCEDURE(t: pthread_t) = NIL;
  RestartThread: PROCEDURE(t: pthread_t) = NIL;
  GetState: PROCEDURE(t: pthread_t; VAR sp: ADDRESS;
                      VAR state: ThreadState) = NIL;

<*EXTERNAL RTMachine__SaveRegsInStack*>
PROCEDURE SaveRegsInStack(): ADDRESS;

END RTMachine.

