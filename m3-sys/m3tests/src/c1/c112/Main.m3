(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  T <: U;
  U <: UNTRACED ROOT;

VAR u: U;

PROCEDURE F (t: T) =
  BEGIN
   EVAL t;
  END F;

BEGIN
  F (u);
END Main.
