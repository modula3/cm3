(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: IF statements *)

MODULE Main;

VAR
  a, b: INTEGER;
  c: BOOLEAN;
  j: INTEGER;

BEGIN
  IF (a > 3) THEN
    j := 1;
  ELSIF (b < 3) THEN
    j := 2;
  ELSIF c THEN
    j := 3;
  ELSE
    j := 4;
  END;
END Main.
