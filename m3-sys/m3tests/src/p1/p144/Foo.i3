(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:38:30 PST 1992 by kalsow *)
(*      modified on Wed Jan  9 13:27:23 1991 by saxe       *)

INTERFACE Foo;

VAR
  intA, intB, intC: INTEGER;

PROCEDURE Plus(x: INTEGER; y: INTEGER): INTEGER;

PROCEDURE Assign(VAR x: INTEGER; y: INTEGER);

PROCEDURE Increment(VAR x: INTEGER): INTEGER;

END Foo.
