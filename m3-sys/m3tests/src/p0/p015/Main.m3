(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: simple procedure with integer argument *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR i: INTEGER;

PROCEDURE P (i: INTEGER)=
  BEGIN
    checkI (i, 2);
    i := i + 1;
    checkI (i, 3);
  END P;

BEGIN
  i := 2;
  checkI (i, 2);
  P (i);
  checkI (i, 2);

  done ();
END Main.
