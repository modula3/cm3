(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: declaration of a REF type *)

MODULE Main;

TYPE
  T = REF INTEGER;

VAR t: T;
BEGIN
  EVAL t;
END Main.
