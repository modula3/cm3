(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = OBJECT
           a, b: INTEGER;
         METHODS
           c (x: INTEGER): INTEGER;
         END;

CONST P = T.c;

VAR i: INTEGER;
VAR p: PROCEDURE (t: T;  x: INTEGER): INTEGER;
BEGIN
  p := P;
  i := P(NIL, 4);
END Main.
