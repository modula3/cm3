(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = ARRAY [0..2] OF REAL;
VAR x : ARRAY [1..100] OF T;

PROCEDURE foo (x: ARRAY OF T) =
  BEGIN
    EVAL x;
  END foo;

BEGIN
  foo (x);
END Main.



