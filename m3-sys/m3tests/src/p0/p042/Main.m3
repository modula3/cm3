(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* RelDist does not return _s *)

MODULE Main;

FROM Test IMPORT checkR, done;

TYPE T = ARRAY [0..5] OF REAL;

PROCEDURE RelDist(READONLY x, y: T; eps: REAL := 1.0e-37): REAL = 
  VAR u, v: REAL;
      s, m: REAL;
  BEGIN
    s := 0.0;
    FOR i := 0 TO 5 DO
      u := x[i]; v := y[i];
      m := MAX(MAX(ABS(u), ABS(v)), eps);
      s := MAX(ABS(u/m - v/m) - eps/m, s);
    END;
    RETURN s
  END RelDist;

BEGIN
  checkR (RelDist (T{0.0,..}, T{1.0,..}), 1.0);
  done ();
END Main.
