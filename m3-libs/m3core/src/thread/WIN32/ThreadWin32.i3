(* Copyright (C) 1994, Digital Equipment Corporation               *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT for a full description.                  *)
(*                                                                 *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.               *)
(* See file COPYRIGHT-CMASS for details.                           *)

UNSAFE INTERFACE ThreadWin32;

FROM WinNT IMPORT HANDLE;
FROM WinDef IMPORT DWORD;
FROM WinBase IMPORT PCRITICAL_SECTION;
IMPORT RTHeapRep;

(*----------------------------------------- Exceptions, types and globals ---*)

<*EXTERNAL ThreadWin32__cm*> VAR cm: PCRITICAL_SECTION;
    (* Global lock for internals of Mutex and Condition *)

<*EXTERNAL ThreadWin32__activeMu*> VAR activeMu: PCRITICAL_SECTION;
    (* Global lock for list of active threads *)
    (* It is illegal to touch *any* traced references while
       holding activeMu because it is needed by SuspendOthers
       which is called by the collector's page fault handler. *)

<*EXTERNAL ThreadWin32__idleMu*> VAR idleMu: PCRITICAL_SECTION;
    (* Global lock for list of idle threads *)

<*EXTERNAL ThreadWin32__slotMu*> VAR slotMu: PCRITICAL_SECTION;
    (* Global lock for thread slot table *)


TYPE
  Activation = UNTRACED REF RECORD
      frame: ADDRESS := NIL; (* exception handling support *)
      next, prev: Activation := NIL;
        (* LL = activeMu; global doubly-linked, circular list of all active threads *)
      handle: HANDLE := NIL;
        (* LL = activeMu; thread handle in Windows *)
      stackbase: ADDRESS := NIL;
        (* LL = activeMu; base of thread stack for use by GC *)
      slot: INTEGER;
        (* LL = slotMu;  index into global array of active, slotted threads *)

      (* thread state *)
      heapState: RTHeapRep.ThreadState;
    END;

(*------------------------------------------------------------------ Self ---*)

<*EXTERNAL ThreadWin32__threadIndex*> VAR threadIndex: DWORD;
<*EXTERNAL ThreadWin32__SetActivation*> PROCEDURE SetActivation (act: Activation);
<*EXTERNAL ThreadWin32__GetActivation*> PROCEDURE GetActivation (): Activation;

(*------------------------------------------------------ ShowThread hooks ---*)

<*EXTERNAL ThreadWin32__perfMu*> VAR perfMu: PCRITICAL_SECTION; (* read-only *)

(*-------------------------------------------------------- Initialization ---*)

<*EXTERNAL ThreadWin32__InitC*> PROCEDURE InitC();

(*------------------------------------------------------------- collector ---*)

<*EXTERNAL ThreadWin32__cs*> VAR cs: PCRITICAL_SECTION; (* read-only *)
<*EXTERNAL ThreadWin32__inCritical*> VAR inCritical: INTEGER; (* LL = cs *)

(* These are implemented in Modula-3 and the C code calls them.
   This is so the pthread version can implement directly in C (extern)
*)
PROCEDURE WaitHeap ();
PROCEDURE BroadcastHeap ();

(*--------------------------------------------- exception handling support --*)

<*EXTERNAL ThreadWin32__handlersIndex*> VAR handlersIndex: DWORD;

(*----------------------------------------------------------------------------*)

END ThreadWin32.
