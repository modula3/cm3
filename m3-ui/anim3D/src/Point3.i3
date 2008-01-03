(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Created by Marc Najork                                                    *)
(* Last modified on Thu Jul 14 10:33:29 PDT 1994 by najork                   *)

(* A "Point3.T" is a point in 3-space. It is represented as a record 
   with three components, "x", "y", and "z", all holding real values. *)

INTERFACE Point3;

TYPE
  T = RECORD
    x,y,z : REAL;
  END;

CONST
  Origin = T {0.0, 0.0, 0.0};
  Min    = T {FIRST(REAL), FIRST(REAL), FIRST(REAL)};
  Max    = T {LAST (REAL), LAST (REAL), LAST (REAL)};
(* "Origin" is the origin of the coordinate system. "Min" and "Max" are used 
   in bounding box calculations to represent imaginary smallest and largest
   points. *)

PROCEDURE Plus (a, b : T) : T;
(* "Plus(a,b)" returns the sum of the points "a" and "b". *)

PROCEDURE Minus (a, b : T) : T;
(* "Minus(a,b)" returns the difference between "a" and "b". *)

PROCEDURE TimesScalar (a : T; x : REAL) : T;
(* "TimesScalar(a,s)" returns "a" with each component 
   multiplied with the scalar value "s". *)

PROCEDURE MidPoint (a, b : T) : T;
(* "MidPoint(a,b)" returns the point in the middle between "a" and "b". *)

PROCEDURE Distance (a, b : T) : REAL;
(* "Distance(a,b)" returns the distance between "a" and "b". *)

PROCEDURE ToText (a : T) : TEXT;
(* "ToText(a)" returns a textual representation of "a"; for example,
   "ToText(T{2.0,3.0,5.0})" returns the text "(2.0,3.0,5.0)" .*)

(* The remaining operations interpret their arguments as vectors, not as
   points. The vector "a" is the vector going from the origin to point "a". *)

PROCEDURE Length (a : T) : REAL;
(* "Length(a)" returns the length of the vector "a". *)

PROCEDURE DotProduct (a, b : T) : REAL;
(* Returns the dot product of "a" and "b". See [Foley] p. 1094ff for an 
   explanation of the geometric significance of dot products. *)

PROCEDURE CrossProduct (a, b : T) : T;
(* Returns the cross product of "a" and "b". See [Foley] p. 1104ff for an 
   explanation of the geometric significance of cross products. One important
   property is that $a \times b$ is orthogonal to the plane described by the
   vectors "a" and "b". *)

PROCEDURE OrthoVector (a : T) : T;
(* Returns a unit vector which is orthogonal to "n". There are infinitely many
   such vectors, "OrthoVector" will return one of them. *)

PROCEDURE ScaleToLen (a : T; len : REAL) : T;
(* Returns a vector parallel to "a" with length "len". *)

END Point3.
