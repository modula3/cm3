(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: nested procedures and variables *)

MODULE Main;

EXCEPTION E (INTEGER);
VAR i: INTEGER;

PROCEDURE P (pArg: INTEGER) =
  VAR p: INTEGER;

  PROCEDURE Q (qArg: INTEGER) =
    VAR q, x: INTEGER;

    PROCEDURE R () =
      VAR r: INTEGER;
      BEGIN i := p + qArg + q + r END R;

    BEGIN
      x := q + qArg + p;
      WITH x = x+3 DO
        qArg := x;
      END;
      TRY
        q := 4;
      EXCEPT E(x) =>
        q := x;
      END;
      R ();
    END Q;

  BEGIN
    Q (pArg);
  END P;

BEGIN
  P (i);
END Main.
