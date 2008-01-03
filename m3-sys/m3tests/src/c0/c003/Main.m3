(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: simple integer arithmetic *)

MODULE Main;

VAR
  i: INTEGER;

BEGIN
  i := 55;
  i := i + i*3;
END Main.
