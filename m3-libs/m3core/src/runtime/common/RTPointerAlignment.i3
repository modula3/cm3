(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)

INTERFACE RTPointerAlignment;

(*--------------------------------------------------------- thread stacks ---*)

CONST
  Value = BYTESIZE(INTEGER);
  (* The C compiler allocates all pointers on 'PointerAlignment'-byte
     boundaries. The garbage collector scans thread stacks, but only
     looks at these possible pointer locations. Setting this value
     smaller than is needed will only make your system run slower.
     Setting it too large will cause the collector to collect storage
     that is not free. *)

END RTPointerAlignment.
