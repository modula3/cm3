(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

TYPE
  Color     = {Red, Orange, Yellow, Blue, Green, Indigo, Violet};
  Warm      = [Color.Red .. Color.Yellow];
  Cold      = [Color.Green .. Color.Violet];
  Invisible = [Color.Violet .. Color.Red];
  Short     = (*BITS 16 FOR*) [5 .. 30];

VAR
  a: ARRAY [10 .. 20] OF Short;
  b: ARRAY [-100 .. 50] OF Short;

PROCEDURE Size (VAR x: ARRAY OF Short): CARDINAL =
  BEGIN
    Test.checkI (NUMBER(x), LAST(x) + 1);
    RETURN NUMBER(x);
  END Size;

BEGIN
  Test.checkI (NUMBER(Color), 7);
  Test.checkI (NUMBER(Warm), 3);
  Test.checkI (NUMBER(Cold), 3);
  Test.checkI (NUMBER(Invisible), 0);
  Test.checkI (NUMBER(Short), 26);
  Test.checkI (NUMBER(a), 11);
  Test.checkI (NUMBER(b), 151);
  Test.checkI (Size(a), 11);
  Test.checkI (Size(b), 151);
  Test.done ();
END Main.
