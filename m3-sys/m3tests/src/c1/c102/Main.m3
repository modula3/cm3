(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = ARRAY [0..2] OF BOOLEAN;

CONST c = T {TRUE, FALSE, TRUE}; 

PROCEDURE Foo () =
  VAR  x := c;
  BEGIN
    EVAL x;
  END Foo;

BEGIN
  Foo ();
END Main.

   
