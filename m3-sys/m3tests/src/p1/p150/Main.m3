(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:16:06 PST 1992 by kalsow *)
(*      modified on Sun Sep  1 01:28:40 1991 by ellis  *)
(*      modified on Wed Oct 24 12:05:59 1990 by saxe   *)

(* Up-level addressing with INLINEs *)

MODULE Main;
IMPORT Test;

VAR
  intA, intB: INTEGER;

<*INLINE*> PROCEDURE Outer(x: INTEGER): INTEGER =
  VAR y, z: INTEGER;

  <*INLINE*> PROCEDURE Middle(x, y: INTEGER): INTEGER =
    VAR w, z: INTEGER;

    <*INLINE*> PROCEDURE Inner(y: INTEGER; x: INTEGER): INTEGER =
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
