(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: SUBARRAY *)

MODULE Main;

VAR
  a: ARRAY [5..10] OF INTEGER;
  i: INTEGER;

BEGIN
  i := a[4*i];
  i := SUBARRAY (a, i, 2*i) [3*i];
END Main.
