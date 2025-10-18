(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main32 EXPORTS Main;
IMPORT Test;
IMPORT Word;

VAR
    x: [8..8];
    y: INTEGER;
    w1,w2 : Word.T;

BEGIN
    x := 8;
    y := x DIV (-3);
    Test.checkI (y, -3);
    w1 := FIRST(INTEGER);
    w2 := 15;
    w1 := Word.Divide(w1, w2);
    Test.checkI(w1, 143165576);
    Test.done ();
END Main32.
