(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

UNSAFE INTERFACE ThreadInternal;

FROM Ctypes IMPORT int;

(*---------------------------------------------------------------------------*)

(* Model a set of integers of arbitrary size? *)

CONST FDSetSize = BITSIZE(INTEGER);

TYPE FDSet = SET OF [0 .. FDSetSize-1];
     FDS = REF ARRAY OF FDSet;

<*EXTERNAL ThreadInternal__Select*>
PROCEDURE Select(nfds: int; VAR read, write, except: FDSet; 
                 timeout: LONGREAL(*Time.T*)): int;
(* Thin wrapper around select. *)

(*---------------------------------------------------------------------------*)


END ThreadInternal.
