(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: variables in nested blocks and procedures *)

MODULE Main;

FROM Test IMPORT msgI, check, done;

VAR
  i: INTEGER;

PROCEDURE X () =
  VAR i: INTEGER;
  BEGIN
    i := 2;
    msgI (i); check (i=2);

    VAR i: INTEGER;
    PROCEDURE Q () =
      BEGIN
        i := 3;
	msgI (i); check (i=3);
      END Q;
    BEGIN
      i := 4;
      msgI (i); check (i=4);
      Q ();
      msgI (i); check (i=3);
    END;

    msgI (i); check (i=2);
    i := 5;
    msgI (i); check (i=5);
  END X;

BEGIN
  i := 1;
  X ();
  msgI (i); check (i=1);

  done();
END Main.

