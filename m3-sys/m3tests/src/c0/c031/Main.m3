(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: assignment & initialization of INTEGER subranges *)

MODULE Main;

VAR
  a: [3..5];
  b: [1..9];
  c: [1..4];
  d: [4..8];
  e: INTEGER;

BEGIN
  a := b;
  a := c;
  a := d;
  a := e;
END Main.
