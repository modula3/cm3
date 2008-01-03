(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

PROCEDURE foo (READONLY i: INTEGER) =
 BEGIN
   EVAL i;
 END foo;

VAR
  x: INTEGER := 23;
  y: ARRAY [1..10] OF INTEGER;
  z: RECORD a: CHAR; b: INTEGER; END;

BEGIN
  foo (3);
  foo (x);
  foo (y[3]);
  foo (z.b);
END Main.
