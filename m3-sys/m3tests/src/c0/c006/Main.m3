(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: variables in nested blocks and procedures *)

MODULE Main;

VAR
  i: INTEGER;
  j: INTEGER;

PROCEDURE X () RAISES ANY =
  VAR i: INTEGER;
  BEGIN
    i := 2;

    VAR i: INTEGER;
    PROCEDURE Q () RAISES ANY =
      BEGIN
        i := 3;
      END Q;
    BEGIN
      i := 4;
      EVAL Q;
    END;


    i := 5;
  END X;

BEGIN
  i := 1;
  EVAL j;
  EVAL X;
END Main.
