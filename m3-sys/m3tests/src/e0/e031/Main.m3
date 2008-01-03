(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
(* spurious '..' in array initializer *)

VAR a := ARRAY OF INTEGER { 1, 2, 3, .. };
BEGIN
END Main.
