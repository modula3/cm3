(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 12 12:07:20 PDT 1993 by swart      *)
(*      modified on Tue Feb 11 16:22:57 PST 1992 by muller     *)
(*      modified on Mon Nov 18 22:13:44 PST 1991 by gnelson    *)
(*      modified on Mon Oct  2 09:27:44 1989 by kalsow         *)
(*      modified on Tue Mar 3 18:14:21 PST 1987 by luca        *)

(* A "Point.T" is a pair of integers representing a position in the
   plane.  If "pt" is a point, then "pt.h" is the distance of "pt" to
   the right of the coordinate origin, and "pt.v" is the distance of
   "pt" below the coordinate origin.  That is, the "hv" coordinate
   system is related to the Cartesian coordinate system by the equation
   "(h, v) = (x, -y)".  
   
   Index: points; geometry, points *)
   
INTERFACE Point;

IMPORT Axis, Word;

TYPE T = RECORD h, v: INTEGER END;

CONST Origin = T { 0, 0 };

(* --- Initialization --- *)

PROCEDURE FromCoords (h, v: INTEGER): T;
(* Build a point out of integers *)

PROCEDURE FromAxes (axis: Axis.T; n, m: INTEGER): T;
(* If axis=Hor then FromCoords (n,m), else FromCoords (m,n) *)

(* --- Selection --- *)

PROCEDURE DistSquare(READONLY p, q: T): INTEGER;
(* Return the square of the Euclidean distance between "p" and "q". *)

(* --- Transformation --- *)

PROCEDURE Minus (READONLY p: T): T;
(* -p.h,-p.v *)

PROCEDURE Add(READONLY p, q: T): T;
(* Return "T{p.h + q.h, p.v + q.v}". *)

PROCEDURE Sub(READONLY p, q: T): T;
(* Return "T{p.h - q.h, p.v -q.v}". *)

PROCEDURE Mul(READONLY p: T; n: INTEGER): T;
(* Return "T{p.h * n, p.v * n}". *)

PROCEDURE Div(READONLY p: T; n: INTEGER): T;
(* Return "T{p.h DIV n, p.v DIV n}". *)

PROCEDURE Mod(READONLY p: T; n: INTEGER): T;
(* Return "T{p.h MOD n, p.v MOD n}". *)

PROCEDURE Scale(READONLY p: T; num, den: INTEGER): T;
(* Return "Div(Mul(p, num), den)". *)

PROCEDURE Min(READONLY p, q: T): T;
(* Return "T{MIN(p.h, q.h), MIN(p.v, q.v)}". *)

PROCEDURE Max(READONLY p, q: T): T;
(* Return "T{MAX(p.h, q.h), MAX(p.v, q.v)}". *)

PROCEDURE Move (READONLY p, dp: T): T;
(* Increment p.h by dp.h and p.v by dp.v; same as Add *)

PROCEDURE MoveH(READONLY p: T; dh: INTEGER): T;
(* Return "T{p.h+dh, p.v}". *)

PROCEDURE MoveV(READONLY p: T; dv: INTEGER): T;
(* Return "T{p.h, p.v+dv}".  *)

PROCEDURE MoveHV(READONLY p: T; dh, dv: INTEGER): T;
(* Return "T{p.h+dh, p.v+dv}". *)

PROCEDURE Transpose(READONLY p: T; ax := Axis.T.Ver): T;
(* If "ax = Hor" then return "p" else return "T{p.v, p.h}". *)

(* --- Test --- *)

PROCEDURE Less (READONLY p, q: T): BOOLEAN;
(* Whether p.h < q.h and p.v < q.v *)

PROCEDURE LessEq (READONLY p, q: T): BOOLEAN;
(* Whether p.h <= q.h and p.v <= q.v *)

(* --- Standard type operations --- *)

PROCEDURE Equal (READONLY p, q: T): BOOLEAN;
(* Point equality *)

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1];
(* == RETURN (-1 if Lt (a, b), 0 if Equal (a, b), +1 o. w.) *)

PROCEDURE Hash (READONLY a: T): Word.T;
(* == RETURN a suitable hash value  *)

END Point.



