(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE b = BRANDED "string" REF INTEGER;
TYPE d = BRANDED "string" REF INTEGER;

VAR a: b;
    c: d;
BEGIN
      a := c;
END Main.

