(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: based constants & BITS FOR... *)

MODULE Main;

TYPE
  T = BITS 4 FOR [8_0..8_17];

CONST
  c: T = 8_13;
  d    = 8_13;
  e    = 16_ffffffff;

VAR
  x: T;
  y: INTEGER;

BEGIN
  x := d;
  x := c;
  y := e;  
END Main.
