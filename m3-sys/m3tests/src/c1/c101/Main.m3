(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = RECORD a, b, c: INTEGER; END;

VAR A, B, C: INTEGER;

BEGIN
 WITH a = T{A, B, C}, b = T{C, B, A} DO END;
END Main.
