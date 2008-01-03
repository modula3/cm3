(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE P (READONLY a: ARRAY OF INTEGER;  x, y: INTEGER) =
  BEGIN
    checkI (NUMBER (a), 2);
    checkI (a[0], x);
    checkI (a[1], y);
  END P;

BEGIN
  P (ARRAY [0..1] OF INTEGER {10, 11}, 10, 11);

  P (ARRAY OF INTEGER {20, 21}, 20, 21);

  WITH x = ARRAY [0..1] OF INTEGER {30, 31} DO P(x, 30, 31) END;

  WITH x = ARRAY OF INTEGER {40, 41} DO P(x, 40, 41) END;

  done ();
END Main.
