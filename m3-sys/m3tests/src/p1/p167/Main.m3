(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Test, Text;

TYPE T3 = ARRAY [0..2] OF TEXT;
TYPE T0 = ARRAY [0..-1] OF TEXT;

PROCEDURE A (VAR x: INTEGER;
             <*NOWARN*> y: ARRAY OF TEXT;
             <*NOWARN*> z: ARRAY OF TEXT := T0{}) =
  BEGIN
    B (x, y, z);
  END A;

PROCEDURE B (<*UNUSED*> VAR x: INTEGER;
             <*NOWARN*> y: ARRAY OF TEXT;
             <*NOWARN*> <*UNUSED*> z: ARRAY OF TEXT := T0{}) =
  BEGIN
    Test.check (Text.Equal (y[0], "The"));
    Test.check (Text.Equal (y[1], "Quick"));
    Test.check (Text.Equal (y[2], "Brown"));
  END B;


VAR i: INTEGER;
BEGIN
  A (i, T3 {"The", "Quick", "Brown"});
  Test.done ();
END Main.
