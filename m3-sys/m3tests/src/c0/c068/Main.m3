(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: does refany have a typecell ? *)

MODULE Main;

TYPE T = REFANY;

VAR t: T;
BEGIN
  EVAL t;
END Main.
