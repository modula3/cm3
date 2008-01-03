(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:25 PDT 1992 by muller                   *)

INTERFACE RealPoint;

(* Basic definitions for geometric points

   Basic definitions for geometric points.

   Index: points; geometry, points *)

IMPORT Axis, Point;

TYPE T = RECORD h, v: REAL END;

CONST Origin = T { 0.0, 0.0 };

(* --- Initialization --- *)

PROCEDURE FromCoords (h, v: REAL): T RAISES {};
(* Build a point out of REALs *)

PROCEDURE FromAxes (axis: Axis.T; n, m: REAL): T RAISES {};
(* If axis=Hor then FromCoords (n,m), else FromCoords (m,n) *)

PROCEDURE Float(p: Point.T): T;
(* Same as FromCoords(FLOAT(p.h),FLOAT(p.v)) *)

PROCEDURE Trunc(p: T): Point.T;
(* Same as Point.FromCoords(TRUNC(p.h),TRUNC(p.v)) *)

PROCEDURE Floor(p: T): Point.T;
(* Same as Trunc, but rounds down (instead of towards zero) *)

PROCEDURE Round(p: T): Point.T;
(* Same as Trunc, but rounds to nearest integer (instead of towards zero) *)

(* --- Selection --- *)

PROCEDURE DistSquare (READONLY p, q: T): REAL RAISES {};
(* square of the distance between p and q *)

(* --- Transformation --- *)

PROCEDURE Minus (READONLY p: T): T RAISES {};
(* -p.h,-p.v *)

PROCEDURE Add (READONLY p, q: T): T RAISES {};
(* Coordinate-wise addition *)

PROCEDURE Sub (READONLY p, q: T): T RAISES {};
(* Coordinate-wise subtraction *)

PROCEDURE Mul (READONLY p: T; n: REAL): T RAISES {};
(* Multiply both coordinates of p by n *)

PROCEDURE Div (READONLY p: T; n: REAL): T RAISES {};
(* Divide both coordinates of p by n *)

PROCEDURE Mod (READONLY p: T; n: REAL): T RAISES {};
(* Mod both coordinates of p by n *)

PROCEDURE Scale (READONLY p: T; num, den: REAL): T RAISES {};
(* Div (Mul (p, num), den) *)

PROCEDURE Min (READONLY p, q: T): T RAISES {};
(* Coordinate-wise min *)

PROCEDURE Max (READONLY p, q: T): T RAISES {};
(* Coordinate-wise max *)

PROCEDURE Move (READONLY p, dp: T): T RAISES {};
(* Increment p.h by dp.h and p.v by dp.v; same as Add *)

PROCEDURE MoveH (READONLY p: T; dh: REAL): T RAISES {};
(* Increment p.h by dh *)

PROCEDURE MoveV (READONLY p: T; dv: REAL): T RAISES {};
(* Increment p.v by dv *)

PROCEDURE MoveHV (READONLY p: T; dh, dv: REAL): T RAISES {};
(* Increment p.h by dh and p.v by dv *)

(* --- Test --- *)

PROCEDURE Equal (READONLY p, q: T): BOOLEAN RAISES {};
(* Point equality *)

PROCEDURE Less (READONLY p, q: T): BOOLEAN RAISES {};
(* Whether p.h < q.h and p.v < q.v *)

PROCEDURE LessEq (READONLY p, q: T): BOOLEAN RAISES {};
(* Whether p.h <= q.h and p.v <= q.v *)

(* --- Standard type operations --- *)

PROCEDURE New (READONLY value: T): REF T;
(* Allocates and initializes a new heap value *)

PROCEDURE NewArray (size: CARDINAL;  READONLY value := Origin): REF ARRAY OF T;
(* Allocates a new array from the heap
  and initializes all its elements with the given value *)

PROCEDURE UntracedNew (READONLY value: T): UNTRACED REF T;
(* Allocates and initializes a new untraced value *)

PROCEDURE UntracedNewArray (size: CARDINAL; READONLY value := Origin):
                                                       UNTRACED REF ARRAY OF T;
(* Allocates a new untraced array from the heap
  and initializes all its elements with the given value *)

PROCEDURE Compare (READONLY a, b: T): INTEGER;
(* == RETURN (-1 if Lt (a, b), 0 if Eq (a, b), +1 o. w.) *)

PROCEDURE Lt (READONLY a, b: T): BOOLEAN;
(* == RETURN (a.h < b.h) OR ((a.h = b.h) AND (a.v < b.v)) *)

PROCEDURE Eq (READONLY a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END RealPoint.
