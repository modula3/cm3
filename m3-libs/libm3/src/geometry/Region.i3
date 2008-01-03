(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Feb  9 12:10:13 PST 1993 by msm     *)
(*      modified on Tue Feb 11 16:24:02 PST 1992 by muller  *)
(*      modified on Mon Nov 11  0:56:49 PST 1991 by gnelson *)
(*      modified on Fri Mar  9 14:29:08 PST 1990 by steveg *)
(*      modified on Thu Feb  1 17:41:31 PST 1990 by glassman *)
<*PRAGMA LL*>

(* A "Region.T" represents a set of integer lattice points.   *)

INTERFACE Region;

IMPORT Rect, Point, Axis;

TYPE
  T = RECORD r: Rect.T; p: P := NIL END;

  P <: REFANY;

(* If "rg" is a region, then "rg.r" is the smallest rectangle 
   containing all points in "rg", and "rg.p" is the private 
   representation of the region as a sorted array of disjoint 
   rectangles.  *)

CONST 
  Empty = T{Rect.Empty, NIL};
  Full = T{Rect.Full, NIL};

PROCEDURE FromRect(READONLY r: Rect.T): T;
(* Return the region containing the same points as "r". *)

PROCEDURE FromRects(READONLY ra: ARRAY OF Rect.T): T;
(* Return the region containing all points in any rectangle of "ra". *)

PROCEDURE ToRects(READONLY rg: T): REF ARRAY OF Rect.T;
(* Returns a list of disjoint rectangles that partition "rg". *)

(* The call "ToRects(Empty)" produces an array of length zero. *)

PROCEDURE FromPoint(READONLY p: Point.T): T;
(* Return the region containing exactly the point "p". *)

PROCEDURE BoundingBox(READONLY rg: T): Rect.T;
(* Return the smallest rectangle containing all the points of "rg";
   this is equivalent to "rg.r". *)

PROCEDURE Add(READONLY rg: T; READONLY p: Point.T): T;
(* Return the translation of "rg" by "p". *)

(* That is, "Add(rg, p)" contains "pt" if and only if "rg" contains
   "Point.Sub(pt, p)".  *)
   
PROCEDURE Sub(READONLY rg: T; READONLY p: Point.T): T;
(* Return "Add(rg, Point.Minus(p))".  *)

PROCEDURE AddHV(READONLY rg: T; dh, dv: INTEGER): T;
(* Return "Add(rg, Point.T{dh,dv})". *)

PROCEDURE Inset(READONLY rg: T; n: INTEGER): T;
(* Return the region inset into "rg" by "n". *)

(* That is, if "n" is non-negative, "Inset(rg, n)" contains a point
   "pt" if all points within distance "n" of "pt" are contained in "rg".
   If "n" is non-positive, "Inset(rg, n)" contains a point "pt" if some
   point within distance "-n" of "pt" is in "rg".  For the purposes
   of this definition, points "p" and "q" are ``within distance "n"''
   if both "ABS(p.h-q.h)" and "ABS(p.v-q.v)" are at most "n".  (If "n"
   is zero, both definitions give "Inset(rg, n) = rg".)  *)
    
PROCEDURE PlaceAxis(READONLY rg: T; 
  n: INTEGER; hv: Axis.T): T;
(* Return the retraction of "rg" by "n" along the "hv" axis. *)

(* That is, let "rect" equal "Rect.FromSize(1, ABS(n))" if "hv" is "Axis.T.Ver"
   or "rect.FromSize(1, ABS(n))" if "hv" is "Axis.T.Hor".  If "n" is
   non-negative, then "PlaceAxis(rg, n, hv)" contains a point "pt" if
   the rectangle "Rect.Add(pt, rect)" is contained in "rg".  If "n"
   is negative, then "PlaceAxis(rg, n, hv)" contains a point "pt" if
   "Rect.Add(pt, rect)" contains some point in "rg".  *)

PROCEDURE Place(READONLY rg: T; h, v: INTEGER): T;
(* Return the retraction of "rg" by "h" along the horizontal
   axis and by "v" along the vertical axis. *)
   
(* More precisely, "Place(rg, h, v)" is defined by the expression

| PlaceAxis(PlaceAxis(rg, h, Axis.Hor), v, Axis.Ver) . 

*)

PROCEDURE Join(READONLY rg, rgP: T): T;
(* Return the union of the points in "rg" and "rgP". *)

PROCEDURE JoinRect(READONLY r: Rect.T; 
  READONLY rg: T): T;
(* Return the union of the points in "r" and "rg". *)

PROCEDURE JoinRegions(READONLY rg: REF ARRAY OF T): T;
(* Return the union of all the regions in "rg". *)

PROCEDURE Meet(READONLY rg, rgP: T): T;
(* Return the intersection of "rg" and "rgP". *)

PROCEDURE MeetRect(READONLY r: Rect.T; 
  READONLY rg: T): T;
(* Return the intersection of the points in "r" and "rg". *)

PROCEDURE Difference(READONLY rg, rgP: T): T;
(* Return the set of points in "rg" and not in "rgP". *)

PROCEDURE SymmetricDifference(READONLY rg, rgP: T): T;
(* Return the set of points in exactly one of "rg" and "rgP". *)

PROCEDURE MaxSubset(READONLY r: Rect.T; 
  READONLY rg: T): Rect.T;
(* Return a large rectangular subset of "rg" containing "r",
   or return "Empty" if "r" is not a subset of "rg".  *)

PROCEDURE Flip(READONLY rg: T; hor, ver: BOOLEAN): T;
(* Return the region which flips "rg" about the horizontal and
   vertical axes, depending on whether "hor" and "ver" are "TRUE". *)

(* More precisely, let "H = -1" if "hor" is "TRUE", and "+1" otherwise,
   and similarly for "V".  Then a point "(h,v)" is in the flipped region
   iff "(H*h, V*v)" is in "rg". *)

PROCEDURE Equal(READONLY rg, rgP: T): BOOLEAN;
(* Return whether "rg" and "rgP" contain the same points. *)

PROCEDURE IsEmpty(READONLY rg: T): BOOLEAN;
(* Return whether "rg" is empty. *)

PROCEDURE IsRect(READONLY rg: T): BOOLEAN;
(* Return whether "rg" is a rectangle, that is, whether
   it contains all the points in its bounding box. *)

PROCEDURE Member(READONLY p: Point.T; 
  READONLY rg: T): BOOLEAN;
(* Return whether "p" is in "rg". *)

PROCEDURE SubsetRect(READONLY r: Rect.T; 
  READONLY rg: T): BOOLEAN;
(* Return whether "r" is contained in "rg". *)

PROCEDURE Subset(READONLY rg, rgP: T): BOOLEAN;
(* Return whether "rg" is contained in "rgP". *)

PROCEDURE OverlapRect(READONLY r: Rect.T; 
  READONLY rg: T): BOOLEAN;
(* Return whether "r" and "rg" have any point in common. *)

PROCEDURE Overlap(READONLY rg, rgP: T): BOOLEAN;
(* Return whether "rg" and "rgP" have any point in common. *)


END Region.
