(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: simple procedure with integer argument *)

MODULE Main;

PROCEDURE P (i: INTEGER) =
  BEGIN
    i := i + 1;
  END P;

BEGIN
  P (3);
END Main.
