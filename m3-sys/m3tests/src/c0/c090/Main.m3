(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  X = REF ARRAY [1..4] OF TEXT;
  Y = REF ARRAY [1..10] OF ARRAY [20..40] OF REF INTEGER;

VAR
  x := ARRAY [0..2] OF TEXT { "a", "b", "c" };
  y : ARRAY [1..10] OF ARRAY [20..40] OF REF INTEGER;

  xx: X;
  yy: Y;

BEGIN

xx [1] := "abc";
yy [4,24] := NIL;
EVAL x;
EVAL y;

END Main.
