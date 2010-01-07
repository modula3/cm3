(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE INTERFACE ThreadWin32;

FROM ThreadF IMPORT State;
FROM ThreadContext IMPORT PCONTEXT;
FROM WinBase IMPORT CRITICAL_SECTION, TLS_OUT_OF_INDEXES;
FROM WinNT IMPORT UCHAR, UINT32, MEMORY_BASIC_INFORMATION;

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

TYPE ClonedHeaderCheckField_t = RECORD
    offset: UCHAR;
    size: UCHAR;
END;

TYPE PClonedHeaderCheck_t = UNTRACED REF ClonedHeaderCheck_t;
TYPE ClonedHeaderCheck_t = RECORD
    TlsOutOfIndexes: UINT32 := TLS_OUT_OF_INDEXES;
    sizeof_CRITICAL_SECTION: UCHAR := BYTESIZE(CRITICAL_SECTION);
    sizeof_MEMORY_BASIC_INFORMATION: UCHAR := BYTESIZE(MEMORY_BASIC_INFORMATION);
    MEMORY_BASIC_INFORMATION_AllocationBase: ClonedHeaderCheckField_t;
    MEMORY_BASIC_INFORMATION_BaseAddress: ClonedHeaderCheckField_t;
    MEMORY_BASIC_INFORMATION_RegionSize: ClonedHeaderCheckField_t;
END;

<*EXTERNAL ThreadWin32__ClonedHeaderCheck*>
PROCEDURE ClonedHeaderCheck(a: PClonedHeaderCheck_t; b: INTEGER);

(*----------------------------------------------------- for SchedulerPosix --*)

PROCEDURE PerfChanged (s: State);
PROCEDURE PerfRunning ();
VAR perfOn: BOOLEAN := FALSE;		 (* LL = perfLock *)

END ThreadWin32.
