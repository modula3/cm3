(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: address subtraction *)

UNSAFE MODULE Main;

VAR
  a, b: UNTRACED REF CHAR;
  i: INTEGER;

BEGIN
  i :=  a - b;
END Main.
