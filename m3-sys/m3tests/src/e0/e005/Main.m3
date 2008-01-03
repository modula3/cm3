(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

CONST
  a : CARDINAL = 100;
  b : CARDINAL =   5;
  c : CARDINAL = 3 * (a / b); 

VAR
  x : ARRAY [0 .. c - 1] OF INTEGER;
  y : ARRAY [0 .. c]     OF INTEGER;

BEGIN

x [0] := 7;
y [0] := 8;

END Main.
