(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

TYPE
  A = OBJECT wheel: INTEGER;  view: INTEGER END;
  B = A OBJECT axle: INTEGER;  view: INTEGER END;

PROCEDURE X (i: INTEGER) =
  VAR z := NEW (B, axle := 3, wheel := 4);   a: A;
  BEGIN
    z.view := i;
    Test.checkI (z.view, 5);
    Test.checkI (z.axle, 3);
    Test.checkI (z.wheel, 4);
    a := z;
    Test.checkI (a.wheel, 4);
    Test.checkI (a.view, 0);
  END X;

BEGIN
  X (5);
  Test.done ();
END Main.
