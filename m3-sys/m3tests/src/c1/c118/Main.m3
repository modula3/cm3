(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

CONST
   c: CARDINAL = 3;

VAR
    a: INTEGER := 10;

BEGIN
    CASE a OF
    | BYTESIZE (a) => a := 30;
    | c => a := 30;
    ELSE
    END
END Main.
