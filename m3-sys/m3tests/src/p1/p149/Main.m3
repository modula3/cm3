(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:12:04 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:18:49 PDT 1990 by ellis  *)
(*      modified on Mon Mar  5 15:54:32 1990 by saxe       *)

(* Up-level addressing *)

MODULE Main;
IMPORT Test;

VAR
  intA, intB: INTEGER;

PROCEDURE Outer(x: INTEGER): INTEGER =
  VAR y, z: INTEGER;

  PROCEDURE Middle(x, y: INTEGER): INTEGER =
    VAR w, z: INTEGER;

    PROCEDURE Inner(y: INTEGER; x: INTEGER): INTEGER =
      BEGIN (* Inner *)
       Test.checkI (w, -2 * intA);
       Test.checkI (x, 6 * intA);
       Test.checkI (y, 4 * intA);
       Test.checkI (z, -42);
       w := 27;
       RETURN x;
      END Inner;

    BEGIN (* Middle *)
      w := x - y;
      Test.checkI (w, -2 * intA);
      Test.checkI (x, 4 * intA);
      Test.checkI (y, 6 * intA);
      z := -42;
      z := Inner(x, y);
      Test.checkI (z, 6 * intA);
      Test.checkI (w, 27);
      RETURN z;
    END Middle;

  BEGIN (* Outer *)
    y := x + x;
    z := x + y;
    RETURN Middle(y + y, z + z) + 4;
  END Outer;

BEGIN
  intA := 176;
  intB := Outer(intA);
  Test.checkI (intB, 1060);
  Test.done ();
END Main.
