(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE Axis = [0..1];
TYPE Axis_T = {Hor, Ver};

VAR a: ARRAY Axis_T OF REAL;

BEGIN
  a := ARRAY Axis OF REAL {0.0, 0.0};
END Main.
