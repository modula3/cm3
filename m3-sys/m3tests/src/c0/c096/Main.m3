(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = RECORD a, b: INTEGER; END;

PROCEDURE foo (a, b: INTEGER) : T =
  BEGIN
    WITH t = T { MAX (a, b), MIN (a, b) } DO
      RETURN t; END;
  END foo;

BEGIN
  EVAL foo (3, 4);
END Main.
