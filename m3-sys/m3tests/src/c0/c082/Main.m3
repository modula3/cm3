(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: lvalues vs rvalues *)

MODULE Main;

VAR x: REAL;

BEGIN
  x := 10.0;
  x := MIN (0.0, x);
  x := x + 20.0;
END Main.
