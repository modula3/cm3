(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: nested procedures with up-level variable references *)

MODULE Main;

VAR i: INTEGER;

PROCEDURE Z (z: INTEGER) =
  BEGIN
    Z (0);
    P (1);
    EVAL z;
  END Z;

PROCEDURE P (p: INTEGER) =

  PROCEDURE Q (q: INTEGER) =

    PROCEDURE R (r: INTEGER) =
        VAR rr, x: INTEGER;
        BEGIN
	  x := r + rr + p + pp + q + qq + i + 1;
	  Z (x);
	  P (x);
	  Q (x);
	  R (x);
	  S (x);
	END R;

    VAR qq, x: INTEGER;
    BEGIN
      x := p + pp + q + qq + i + 2;
      Z (x);
      P (x);
      Q (x);
      R (x);
      S (x);
    END Q;

  PROCEDURE S (s: INTEGER) =
    VAR ss, x: INTEGER;
    BEGIN
      x := p + pp + s + ss + i + 3;
      Z (x);
      P (x);
      Q (x);
      S (x);
    END S;

  VAR pp, x: INTEGER;
  BEGIN
    x := p + pp + i + 4;
    Z (x);
    P (x);
    Q (x);
    S (x);

    VAR zz: INTEGER;
    PROCEDURE T () =
      VAR x: INTEGER;
      BEGIN
        x := zz + pp;
      END T;
    BEGIN
      T ();
    END;
        
  END P;

BEGIN
  VAR x: INTEGER;
  BEGIN
    x := 5 * x;
    Z (x);
    P (x);
  END;
END Main.
