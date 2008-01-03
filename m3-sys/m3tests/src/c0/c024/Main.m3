(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: importing an external subrange type *)

MODULE Main;

IMPORT Test;

VAR
  i: Test.foo;
  j: INTEGER;

BEGIN
  i := j;
END Main.
