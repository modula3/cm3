(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: user & language specified variable initialization *)

MODULE Main;

TYPE
  t1 = [0..5];
  t2 = [3..8];

VAR
  f, g : INTEGER := j;
  i, j : t1 := 4;
  k, m : t2 := i;
  n, p : INTEGER := k + j;

BEGIN
  i := 2;
  EVAL f;
  EVAL g;
  EVAL m;
  EVAL n;
  EVAL p;
END Main.
