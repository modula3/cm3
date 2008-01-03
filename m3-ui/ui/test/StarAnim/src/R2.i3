(* Copyright (C)-ip992, Digital Equipment Corporation                        *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 15 13:06:44 PST 1996 by heydon                   *)

INTERFACE R2;

(* This simple interface defines points in the vector space R^2 and a small
   collection of operations upon them. *)

IMPORT Point;

TYPE
  T = RECORD x, y: REAL END;

CONST
  Origin = T{ 0.0, 0.0 };

PROCEDURE Plus(READONLY a, b: T): T;
(* Return the vector sum "a + b". *)

PROCEDURE Minus(READONLY a, b: T): T;
(* Return the vector difference "a - b". *)

PROCEDURE Scale(READONLY p: T; k: REAL): T;
(* Return the scalar product "k * p". *)

PROCEDURE Length(READONLY p: T): REAL;
(* Return the Euclidean length of the vector "p". *)

PROCEDURE Normalize(READONLY p: T): T;
(* Return "Scale(p, 1.0 / Length(p))". It is a checked run-time error for
   "Length(p) = 0.0". *)

PROCEDURE Rel(READONLY p, a, b: T): T;
(* Return the point "p REL (a,b)" as defined by the Juno-2 language. *)

PROCEDURE FromPoint(READONLY p, origin: Point.T): T;
(* Convert the point "p" in Trestle coordinates to a "R2.T" such that
   "FromPoint(p, p)" is mapped to "Origin". *)

PROCEDURE ToPoint(READONLY p: T; READONLY origin: Point.T): Point.T;
(* Convert the point "p" to a Trestle point, where "Origin" is mapped to
   "origin". This rounds the coordinates to the integer lattice and inverts
   the sense of the Y axis. *)

END R2.
