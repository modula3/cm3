(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE A;

PROCEDURE Foo (i: INTEGER) RAISES {A, C} =
  BEGIN
    EVAL i;
  END Foo;

BEGIN
END A.
