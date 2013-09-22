(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE INTERFACE ThreadWin32;

FROM ThreadF IMPORT State;
FROM ThreadContext IMPORT PCONTEXT;
FROM WinBase IMPORT PCRITICAL_SECTION, CRITICAL_SECTION;
FROM WinNT IMPORT UINT32;

(*---------------------------------------------------------------------------*)

(* locks (aka critical section aka mutex) *)

(* static locks *)
(* implementing variables in C greatly increase debuggability (symbols work) *)

<*EXTERNAL ThreadWin32__activeLock*> VAR activeLock: CRITICAL_SECTION;
    (* Global lock for list of active threads *)
    (* It is illegal to touch *any* traced references while
       holding activeLock because it is needed by SuspendOthers
       which is called by the collector's page fault handler. *)

<*EXTERNAL ThreadWin32__slotLock*> VAR slotLock: CRITICAL_SECTION;
    (* Global lock for thread slot table that maps untraced to traced *)

<*EXTERNAL ThreadWin32__initLock*> VAR initLock: CRITICAL_SECTION;
    (* Global lock for initializing locks *)

(*------------------------------------------------------ ShowThread hooks ---*)

<*EXTERNAL ThreadWin32__perfLock*> VAR perfLock: CRITICAL_SECTION;

(*------------------------------------------------------------- collector ---*)
(* synchronization for the allocator and collector *)

<*EXTERNAL ThreadWin32__heapLock*> VAR heapLock: CRITICAL_SECTION;

(*---------------------------------------------------------------------------*)

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

(*---------------------------------------------------------------------------*)

<*EXTERNAL ThreadWin32__GetStackBounds*>
PROCEDURE GetStackBounds(VAR start: ADDRESS; VAR end: ADDRESS);

<*EXTERNAL ThreadWin32__ClonedHeaderCheck*>
PROCEDURE ClonedHeaderCheck(a: PClonedHeaderCheck_t; b: INTEGER);

<*EXTERNAL ThreadWin32__NewCriticalSection*>
PROCEDURE NewCriticalSection(): PCRITICAL_SECTION;

<*EXTERNAL ThreadWin32__DelCriticalSection*>
PROCEDURE DelCriticalSection(VAR a:PCRITICAL_SECTION);

(*CONST*)
<*EXTERNAL ThreadWin32__TLS_OUT_OF_INDEXES*> VAR TLS_OUT_OF_INDEXES: UINT32;
<*EXTERNAL ThreadWin32__WAIT_OBJECT_0*> VAR WAIT_OBJECT_0: UINT32;
<*EXTERNAL ThreadWin32__WAIT_TIMEOUT*> VAR WAIT_TIMEOUT: UINT32;
<*EXTERNAL ThreadWin32__CREATE_SUSPENDED*> VAR CREATE_SUSPENDED: UINT32;
<*EXTERNAL ThreadWin32__DUPLICATE_SAME_ACCESS*> VAR DUPLICATE_SAME_ACCESS: UINT32;
<*EXTERNAL ThreadWin32__INFINITE*> VAR INFINITE: UINT32;

<*EXTERNAL ThreadWin32__threadIndex*> VAR threadIndex: UINT32;

(*----------------------------------------------------- for SchedulerPosix --*)

PROCEDURE PerfChanged (s: State);
PROCEDURE PerfRunning ();
VAR perfOn: BOOLEAN := FALSE;		 (* LL = perfLock *)

END ThreadWin32.
