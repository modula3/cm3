(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: large INTEGER constants *)

INTERFACE Test;

TYPE
 Bits29 = BITS 29 FOR [0..16_1FFFFFFF];

END Test.
