(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: assigning non-overlapping subrange types *)

MODULE Main;

VAR
  a : [-8 .. -4];
  b : [7 .. 9];
  c : CARDINAL;

BEGIN
  a := b;
  b := a;
  a := c;
  c := a;
END Main.
