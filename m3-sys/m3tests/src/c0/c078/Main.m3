(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Jerome Chailloux <jerome> 2 jan 90 *)

MODULE Main;

VAR
  foo: TEXT := "abc\100\101\102\120\200\300\377";

BEGIN
  EVAL foo;
END Main.
