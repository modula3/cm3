(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: procedures that return structures in C *)

MODULE Main;

TYPE
  R1 = RECORD x: INTEGER; END;
  R2 = RECORD x, y: INTEGER; END;
  A  = ARRAY [0..10] OF INTEGER;


PROCEDURE fooR1 (): R1 =
  VAR x: R1;
  BEGIN
    RETURN (x);
  END fooR1;

PROCEDURE fooR2 (): R2 =
  VAR x: R2;
  BEGIN
    RETURN (x);
  END fooR2;

PROCEDURE fooA (): A =
  VAR x: A;
  BEGIN
    RETURN (x);
  END fooA;

VAR r1: R1; r2: R2; a: A;

BEGIN

r1 := fooR1 ();
r2 := fooR2 ();
a  := fooA  ();

END Main.

