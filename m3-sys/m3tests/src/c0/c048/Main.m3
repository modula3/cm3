(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: NUMBER *)

MODULE Main;

PROCEDURE Foo (
  READONLY a : ARRAY OF CHAR;
  i : INTEGER;
  j : CARDINAL) =

BEGIN
  i := NUMBER (a);
  j := NUMBER (a);
  i := NUMBER ([0..3]);
END Foo;

BEGIN
  EVAL Foo;
END Main.
