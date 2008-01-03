(* Copyright (C) 1992, Digital Equipment Corporation                 *)
(* All rights reserved.                                              *)
(* See the file COPYRIGHT for a full description.                    *)
(*                                                                   *)
(* by Stephen Harrison and Steve Glassman *)

(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

INTERFACE RealTransform;

(* A Matrix.T is a simplified representation of a true 3 x 3 matrix.  We
   assume the last column is always {0, 0, 1} so we do not explicitly hold
   it.

   Here is the layout of the elements of our matrix.  A number is the
   corresponding index into the array, `x' means the element is not held
   explicitly.

|       Column
|      0   1   2
|    +---+---+---+
|   0| 0 | 1 | x |
| R  +---+---+---+
| o 1| 2 | 3 | x |
| w  +---+---+---+
|   2| 4 | 5 | x |
|    +---+---+---+

   *)

IMPORT RealPoint;

TYPE T = ARRAY [0 .. 5] OF REAL;

CONST
  Identity = T{1.0, 0.0,        (* 0.0 *)
               0.0, 1.0,        (* 0.0 *)
               0.0, 0.0};       (* 1.0 *)

PROCEDURE Scale(READONLY sx, sy: REAL): T;
PROCEDURE Translate(READONLY tx, ty: REAL): T;
PROCEDURE Rotate(READONLY radians: REAL): T;

PROCEDURE Concat(READONLY m, n: T): T;
PROCEDURE Concat3(READONLY l, m, n: T): T;

PROCEDURE Inverse (READONLY m: T): T;

PROCEDURE Transform(READONLY m: T; READONLY p: RealPoint.T): RealPoint.T;

END RealTransform.

