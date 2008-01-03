(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: a simple subrange type in an interface *)

INTERFACE Test;

TYPE
  foo = [0..4];

END Test.
