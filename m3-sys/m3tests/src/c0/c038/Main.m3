(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: open ARRAY indexing *)

MODULE Main;

PROCEDURE foo (a: ARRAY OF INTEGER;
               b: ARRAY OF ARRAY OF INTEGER;
               i: INTEGER ) RAISES ANY =
  BEGIN
    i := a[i];
    i := b[i, 2*i];
    a := b[i];
  END foo;

BEGIN
  EVAL foo;
END Main.

