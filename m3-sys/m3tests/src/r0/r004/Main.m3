(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
(* allocate a negatively sized open array *)

VAR
  a: INTEGER;
  r: REFANY;

BEGIN
  a := -1;
  r := NEW (REF ARRAY OF INTEGER, a);
END Main.
