(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE BuffOWrDotT;

FROM BuffOWr IMPORT B;
IMPORT OWr;

PROCEDURE NewB (self: B; wr: OWr.T; size: CARDINAL := 0): B;


END BuffOWrDotT.
