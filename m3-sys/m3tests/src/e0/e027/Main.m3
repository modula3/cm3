(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

VAR v : ARRAY[1..0] OF CHAR;
(* arrays with zero elements are allowed *)

VAR z : [1..0];
(* Variables are not allowed to have empty types *)

BEGIN
  EVAL NUMBER (v);
  EVAL z;
END Main.
