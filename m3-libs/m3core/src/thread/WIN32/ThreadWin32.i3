(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE INTERFACE ThreadWin32;

FROM WinDef IMPORT LONG;
FROM Thread IMPORT T;
FROM ThreadF IMPORT State;

(*---------------------------------------------------------------------------*)

(* locks (aka critical section aka mutex) *)


TYPE Lock_t = ADDRESS;

<*EXTERNAL ThreadWin32__NewLock*> PROCEDURE NewLock(): Lock_t;
<*EXTERNAL ThreadWin32__Lock*> PROCEDURE Lock(lock: Lock_t);
<*EXTERNAL ThreadWin32__Unlock*> PROCEDURE Unlock(lock: Lock_t);
<*EXTERNAL ThreadWin32__DeleteLock*> PROCEDURE DeleteLock(lock: Lock_t);

(* static locks *)

(* Global lock for internals of Mutex and Condition *)
<*EXTERNAL ThreadWin32__giantLock*> VAR giantLock: Lock_t;

<*EXTERNAL ThreadWin32__activeLock*> VAR activeLock: Lock_t;
    (* Global lock for list of active threads *)
    (* It is illegal to touch *any* traced references while
       holding activeLock because it is needed by SuspendOthers
       which is called by the collector's page fault handler. *)


<*EXTERNAL ThreadWin32__slotLock*> VAR slotLock: Lock_t;
    (* Global lock for thread slot table that maps untraced to traced *)


(*------------------------------------------------------------------ Self ---*)

(* the untraced state of a thread, a thread local *)
TYPE Activation <: ADDRESS;

<*EXTERNAL ThreadWin32__SetActivation*> PROCEDURE SetActivation (act: Activation);
<*EXTERNAL ThreadWin32__GetActivation*> PROCEDURE GetActivation (): Activation;

(*------------------------------------------------------ ShowThread hooks ---*)

<*EXTERNAL ThreadWin32__perfLock*> VAR perfLock: Lock_t;

(*------------------------------------------------------------- collector ---*)
(* synchronization for the allocator and collector *)

<*EXTERNAL ThreadWin32__heapLock*> VAR heapLock: Lock_t;

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
PROCEDURE InitC();

(*----------------------------------------------------- for SchedulerPosix --*)

PROCEDURE PerfChanged (s: State);
PROCEDURE PerfRunning ();
PROCEDURE XTestAlert (self: T): BOOLEAN;
VAR perfOn: BOOLEAN := FALSE;		 (* LL = perfLock *)

END ThreadWin32.
