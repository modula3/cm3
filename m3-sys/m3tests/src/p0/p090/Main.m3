(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT msg, msgR, checkR, done;

CONST V = 1.0;

PROCEDURE CheckSet (a: REAL;  VAR b: REAL;  READONLY c: REAL) =
  BEGIN
    msg ("VALUE:");    msgR (a); checkR (a, V);
    msg ("VAR:");      msgR (b); checkR (b, V);
    msg ("READONLY:"); msgR (c); checkR (c, V);
  END CheckSet;

PROCEDURE Check (x: REAL) =
  VAR y: REAL := x;
  BEGIN
    CheckSet (x, x, x);
    CheckSet (y, y, y);
  END Check;

BEGIN
  Check (V);
  done ();
END Main.
