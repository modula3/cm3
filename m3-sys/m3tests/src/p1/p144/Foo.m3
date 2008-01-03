(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:41:46 PST 1992 by kalsow *)
(*      modified on Wed Mar 28 15:40:11 1990 by saxe       *)

MODULE Foo;

(* VAR intA, intB, intC: INTEGER; *)

PROCEDURE Plus(x: INTEGER; y: INTEGER): INTEGER =
  BEGIN RETURN x + y; END Plus;

PROCEDURE Assign(VAR x: INTEGER; y: INTEGER) =
  BEGIN x := y; RETURN; END Assign;

PROCEDURE Increment(VAR x: INTEGER): INTEGER =
  BEGIN x := x + 1; RETURN x; END Increment;

BEGIN
END Foo.
