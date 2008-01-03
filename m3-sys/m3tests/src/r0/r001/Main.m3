(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: missing handler *)

MODULE Main;

EXCEPTION a;

BEGIN

RAISE a;

END Main.

