(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: procedure arguments *)

MODULE Main;

TYPE
  P = PROCEDURE (x: INTEGER);

PROCEDURE Q (p: P) =
  BEGIN
    EVAL p;
  END Q;

PROCEDURE R (x: INTEGER) =
  BEGIN
    EVAL x;
  END R;

BEGIN
Q (R);
END Main.
