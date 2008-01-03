(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: conflict of non-interned floating point constants *)

MODULE Main;

PROCEDURE F (x, y: REAL) = BEGIN EVAL x; EVAL y; END F;

BEGIN
  F (-0.4, -0.6);
END Main.
