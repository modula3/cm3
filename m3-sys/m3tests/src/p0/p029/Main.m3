(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: nested procedures with up-level variable references *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR i: INTEGER;

PROCEDURE P (p: INTEGER) =

  PROCEDURE Q (q: INTEGER) =

    PROCEDURE R (r: INTEGER) =
        VAR rr, x: INTEGER;
        BEGIN
          rr := 1;
	  x := r + rr + p + pp + q + qq + i; (* 128, 1, 122 *)
	  checkI (x, 251);
	END R;

    VAR qq, x: INTEGER;
    BEGIN
      qq := 2;
      x := p + pp + q + qq + i; (* 32, 8, 64, 2, 16 *)
      checkI (x, 122);
      R (2*q);
    END Q;

  PROCEDURE S (s: INTEGER) =
    VAR ss, x: INTEGER;
    BEGIN
      ss := 4;
      x := p + pp + s + ss + i;   (* 32, 8, 64, 4, 16 *)
      checkI (x, 124);
    END S;

  VAR pp, x: INTEGER;
  BEGIN
    pp := 8;
    x := p + pp + i;
    checkI (x, 56);
    Q (2*p);
    S (2*p);
  END P;

BEGIN
  VAR x: INTEGER;
  BEGIN
    i := 16;
    x := 32;
    checkI (x, 32);
    P (x);
  END;

  done ();

END Main.
