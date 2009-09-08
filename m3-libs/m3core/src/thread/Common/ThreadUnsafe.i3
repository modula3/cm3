(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE ThreadUnsafe;

IMPORT RTHeapRep;

(*--------------------------------------------- garbage collector support ---*)

PROCEDURE MyHeapState(): UNTRACED REF RTHeapRep.ThreadState;

(*---------------------------------------------------------------------------*)

END ThreadUnsafe.
