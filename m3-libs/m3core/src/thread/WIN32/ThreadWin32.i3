(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE INTERFACE ThreadWin32;

FROM WinDef IMPORT LONG, HANDLE;
FROM ThreadF IMPORT State;
FROM ThreadContext IMPORT PCONTEXT;

(*---------------------------------------------------------------------------*)

(* locks (aka critical section aka mutex) *)


TYPE LockRE_t = ADDRESS;

<*EXTERNAL ThreadWin32__NewLockRE*> PROCEDURE NewLockRE(): LockRE_t;
<*EXTERNAL ThreadWin32__LockRE*> PROCEDURE LockRE(lock: LockRE_t);
<*EXTERNAL ThreadWin32__Unlock*> PROCEDURE UnlockRE(lock: LockRE_t);
<*EXTERNAL ThreadWin32__DeleteLockRE*> PROCEDURE DeleteLockRE(lock: LockRE_t);

(* static locks *)

(* Global lock for internals of Mutex and Condition *)
<*EXTERNAL ThreadWin32__giantLock*> VAR giantLock: LockRE_t;

<*EXTERNAL ThreadWin32__activeLock*> VAR activeLock: LockRE_t;
    (* Global lock for list of active threads *)
    (* It is illegal to touch *any* traced references while
       holding activeLock because it is needed by SuspendOthers
       which is called by the collector's page fault handler. *)

<*EXTERNAL ThreadWin32__slotLock*> VAR slotLock: LockRE_t;
    (* Global lock for thread slot table that maps untraced to traced *)

<*EXTERNAL ThreadWin32__initLock*> VAR initLock: LockRE_t;
  (* used when allocation the criticalsection within a mutex on-demand *)

(*------------------------------------------------------------------ Self ---*)

(* the untraced state of a thread, a thread local *)
TYPE Activation <: ADDRESS;

<*EXTERNAL ThreadWin32__SetActivation*> PROCEDURE SetActivation (act: Activation);
<*EXTERNAL ThreadWin32__GetActivation*> PROCEDURE GetActivation (): Activation;

(*------------------------------------------------------ ShowThread hooks ---*)

<*EXTERNAL ThreadWin32__perfLock*> VAR perfLock: LockRE_t;

(*------------------------------------------------------------- collector ---*)
(* synchronization for the allocator and collector *)

<*EXTERNAL ThreadWin32__heapLock*> VAR heapLock: LockRE_t;

(*---------------------------------------------------------------------------*)

<*EXTERNAL ThreadWin32__GetStackBounds*>
PROCEDURE GetStackBounds(VAR start, end: ADDRESS);

(*---------------------------------------------------------------------------*)

<*EXTERNAL ThreadWin32__InterlockedIncrement*>
PROCEDURE InterlockedIncrement(VAR a: LONG);

<*EXTERNAL ThreadWin32__InterlockedDecrement*>
PROCEDURE InterlockedDecrement(VAR a: LONG);

<*EXTERNAL ThreadWin32__InterlockedRead*>
PROCEDURE InterlockedRead(VAR a: LONG): LONG;

<*EXTERNAL ThreadWin32__InitC*>
PROCEDURE InitC(bottom: ADDRESS): HANDLE; (* returns current thread handle *)

<*EXTERNAL "ThreadWin32__ProcessLive"*>
PROCEDURE ProcessLive(bottom: ADDRESS; p: PROCEDURE(start, limit: ADDRESS));

<*EXTERNAL "ThreadWin32__ProcessStopped"*>
PROCEDURE ProcessStopped(stackStart, stackEnd: ADDRESS; context: PCONTEXT;
                         p: PROCEDURE(start, limit: ADDRESS));

<*EXTERNAL ThreadWin32__StackPointerFromContext*>
PROCEDURE StackPointerFromContext(context: PCONTEXT): ADDRESS;

<*EXTERNAL ThreadWin32__NewContext*>
PROCEDURE NewContext(): ADDRESS;

<*EXTERNAL ThreadWin32__DeleteContext*>
PROCEDURE DeleteContext(a: ADDRESS);

(*----------------------------------------------------- for SchedulerPosix --*)

PROCEDURE PerfChanged (s: State);
PROCEDURE PerfRunning ();
VAR perfOn: BOOLEAN := FALSE;		 (* LL = perfLock *)

END ThreadWin32.
