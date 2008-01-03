(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

VAR
  x := ARRAY [1..10] OF CHAR {};
  y := ARRAY OF CHAR {};

BEGIN
  EVAL x;
  EVAL y;
END Main.
