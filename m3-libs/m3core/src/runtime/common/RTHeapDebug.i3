(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Wed Jun 22 09:54:31 PDT 1994 by kalsow     *)
(*|      modified on Wed May 25 14:40:59 PDT 1994 by detlefs    *)

INTERFACE RTHeapDebug;

PROCEDURE Free(r: REFANY);
(* Asserts that "r" is not reachable. *)

PROCEDURE CheckHeap();
(* Examines the heap, searching for objects that have been asserted
   free, but are reachable from a global variable, printing (to
   standared error) a path from that global to the object for each
   such putatively free object. *)

CONST MaxFree = 25;
(* The maximum number of "free" objects that will be remembered.  If the
   runtime parameter "@M3heapDebugMaxFree" is set to a positive integer
   value, that value will be used instead. *)

END RTHeapDebug.
