(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: procedure & variables in an interface *)

INTERFACE Test;

TYPE
  t1 = [0..5];
  t2 = [3..4];
  t3 = [3..8];
  t4 = [-3..9];

VAR
  v1: t1;
  v2: t2;
  v3: t3;
  v4: t4;
  i: INTEGER;

PROCEDURE Init ();

END Test.
