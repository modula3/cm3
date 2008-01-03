(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 17:18:20 PDT 1993 by swart          *)
(*      modified on Thu Nov  2 18:28:26 1989 by muller         *)
(*      modified on Fri Sep 29 17:27:18 1989 by kalsow         *)
(*      modified on Fri Jun  3 16:15:44 PDT 1988 by glassman   *)
(*      modified on Tue Feb  9 19:53:16 1988 by luca           *)

INTERFACE Transform;
(* Creating and manipulating 2-dimensional transformations
   This interface with 2 dimensional transformations. See Newman and Sproull,
   Chapter 4, for more information.
   Index: matrices, transformations ; transformations *)

IMPORT Point;
(* If X is of type T then X represents the matrix
|
|   [ a11  a12  0 ]
|   [ a21  a22  0 ]
|   [ a31  a32  1 ]
|
   Points in the (h,v) coordinate system (e.g., those represented by
   Point.T's) are interpreted as (h,v)==(h, v, 1). An application of X to a
   point (h,v) consists of a single post-multiplication:
|
|  (h, v, 1) [ a11  a12  0 ]      (H, V, 1)
|            [ a21  a22  0 ]  =
|            [ a31  a32  1 ]
|
   The values (H,V) are the transformed points. The transformation matrices
   have REAL elements however they operate on, and produce, integer elements.
   This is done as follows, shown for the H element above:
|
|      H := TRUNC(FLOAT(h)*a11 + FLOAT(v)*a21 + a31 + 0.5)
|
   The leading 2 by 2 submatrix of X is the usual rotation/scaling matrix
   while the a31 and a32 elements provide translation. Composition is
   performed by pre-multiplication, i.e., A composed with B is AB *)

TYPE
  T = RECORD a11, a12, a21, a22, a31, a32: REAL END;


PROCEDURE Identity (): T;
(* Returns the identity transformation.  Use this to get new transformations *)

PROCEDURE Apply (tr: T; p: Point.T): Point.T;
(* Returns the result of applying the transformation "tr" to the point "p". *)

PROCEDURE Translate (h, v: REAL; READONLY tr: T): T;
(* Returns the transformation that is the composition of the input
   transformation and the translation (h,v) *)

PROCEDURE Rotate (theta: REAL; READONLY tr: T): T;
(* Returns the transformation that is the composition of the input
   transformation and the rotation by `theta' radians *)

PROCEDURE Scale (fh, fv: REAL; READONLY tr: T): T;
(* Returns the transformation that is the composition of the input
   transformation and the scaling of the h axis by fh and the v axis by fv.
   Hence, the scaling is anisotropic if fh#fv *)
(* Here are a few convenience procedures *)

PROCEDURE FromPoint (READONLY p: Point.T): T;
(* Returns a translation transformation *)

PROCEDURE Compose (READONLY t1, t2: T): T;
(* Composes t1 and t2, result is t1*t2. Note that this means that t1 will be
   applied first by, e.g., Apply above. *)

PROCEDURE RotateAbout (READONLY p: Point.T; theta: REAL): T;
(* Returns the transformation that rotates "theta" radians about the point
   "p". This is equivalent to the composition of three transformations:
   translate to origin, rotate theta, translate back to p *)

PROCEDURE IsoScale (f: REAL): T;
(* Returns a transformation that scales each axis by f *)

PROCEDURE AnIsoScale (fh, fv: REAL): T;
(* See Scale *)

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1];
(* == RETURN (-1 if a.h < b.h) OR ((a.h = b.h) AND (a.v < b.v)), 0 if a =
   b, +1 o.  w.) *)

PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END Transform.
