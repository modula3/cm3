(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Feb 11 16:23:21 PST 1992 by muller   *)
(*      modified on Wed Sep 11 15:32:52 PDT 1991 by msm      *)
<*PRAGMA LL*>

INTERFACE PolyRegion;

IMPORT Rect, Region, Point;

(* A PolyRegion.T represents a set of points as a list of Region.T's. 
   If n rectangles are joined together into a PolyRegion.T with the
   JoinRect procedure below, the PolyRegion will contain about lg n
   Regions, and the total cost of the join is likely to be
   about n (lg n)^2 instead of n^2, assuming the rectangles don't
   overlap too badly. 
   
   The procedures modify the data structure pointed to by the private
   field of a PolyRegion; therefore, you must not assign PolyRegions. *) 

TYPE 
  T = RECORD r: Rect.T; p: Private END;
  (* pr.r is the bounding rectangle of the PolyRegion pr. *)
  Private <: REFANY;

PROCEDURE JoinRect(VAR pr: T; READONLY rect: Rect.T);
(* pr := Union(pr, rect). *)

PROCEDURE JoinRgn(VAR pr: T; READONLY rgn: Region.T);
(* pr := Union(pr, rgn). *)

PROCEDURE ToRegion(READONLY pr: T): Region.T;
(* Return pr as a region. *)

PROCEDURE OverlapRect(READONLY pr: T; READONLY rect: Rect.T): BOOLEAN;
(* Return whether pr and rect have a non-empty intersection. *)

PROCEDURE Complement(READONLY pr: T; READONLY rgn: Region.T): Region.T;
(* Return rgn-pr as a region. *)

PROCEDURE Meet(READONLY pr: T; READONLY rgn: Region.T): Region.T;
(* Return the intersection of pr and rgn as a region. *)

CONST Empty = T{Rect.Empty, NIL};

(* The following procedure belongs in Region *)

PROCEDURE Factor(READONLY t: Region.T; READONLY r: Rect.T; 
  READONLY delta: Point.T; VAR rl: REF ARRAY OF Rect.T): CARDINAL;
(* Set rl to a list of disjoint rectangles which partition t meetrect
   r, modifying rl^ if possible.  Return the number of rectangles, n,
   in the partition; only the first n rectangles in rl^ are meaningful.
   The order of rl^ is such that if i<j then rl[i] translated by delta
   doesn't intersect rl[j].  If the intersection is empty, 0 is returned. *)

END PolyRegion.

