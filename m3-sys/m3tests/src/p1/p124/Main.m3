(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

VAR
    x: [8..8];
    y: INTEGER;
BEGIN
    x := 8;
    y := x DIV (-3);
    Test.checkI (y, -3);
    Test.done ();
END Main.
