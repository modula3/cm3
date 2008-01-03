(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Test;

VAR
  i: INTEGER;
BEGIN
  i := FLOOR( 1.6 );
  Test.checkI (i, 1);
  Test.done ();
END Main.
