(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:10:18 PST 1992 by kalsow *)
(*      modified on Mon Aug  6 20:18:40 PDT 1990 by ellis  *)
(*      modified on Fri Feb 16 16:02:10 1990 by saxe       *)

(* Simple examples of procedure calls *)

MODULE Main;
IMPORT Test;

VAR
  intA, intB, intC: INTEGER;

PROCEDURE Plus(x: INTEGER; y: INTEGER): INTEGER =
  BEGIN RETURN x + y; END Plus;

PROCEDURE Assign(VAR x: INTEGER; y: INTEGER) =
  BEGIN x := y; RETURN; END Assign;

PROCEDURE Increment(VAR x: INTEGER): INTEGER =
  BEGIN x := x + 1; RETURN x; END Increment;

BEGIN
  Assign(intA, 30);
  Test.checkI (intA, 30);
  Assign(intB, 20);
  Test.checkI (intB, 20);
  Assign(intC, Increment(intB));
  Test.checkI (intC, 21);
  Test.checkI (intB, 21);
  Assign(intC, Plus(intA, intB));
  Assign(intC, Plus(intC, intC));
  Test.checkI (intC, Plus(100, 2));
  Test.checkI (Increment(intB), 22);
  Assign(intB, Plus(Increment(intB), Plus(Increment(intB), intA)));
  (* for any legal order of evaluation *)
  Test.checkI (intB, 77);
  Test.done ();
END Main.
