(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: CSE on array indexing expressions *)

MODULE Main;

TYPE
  T = REF ARRAY OF ARRAY OF INTEGER;

VAR
  x: T;
  i, j: INTEGER;
  y: ARRAY [0..10],[0..20] OF INTEGER;

BEGIN
  y[i,j] := y[i+1,j];
  x[i,j] := x[i+1,j];
END Main.
