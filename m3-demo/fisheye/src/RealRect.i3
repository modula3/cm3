(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May 17 21:12:16 PDT 1994 by mhb                      *)
(*      modified on Tue Jun 16 16:46:23 PDT 1992 by muller                   *)

INTERFACE RealRect;

(* A geometric rectangle; sides must be parallel to coordinate axes

   "Rect" is short for rectangle; the rectangles referred to must have their
   sides parallel to the coordinate axes. All coordinates are REALs.

   Index: rectangles *)

IMPORT Axis, RealInterval, RealPoint, Rect;

(* A "rectangle" is a quadruple of REALs (w,e,n,s) (west, east, north,
   south) with either w<e, n<s or w=e, n=s. It is meant to represent the set
   of points p=(h,v) with w<=h<e and n<=v<s. There are many rectangles
   denoting the empty set; all rectangles with w=e and n=s are called "empty
   rectangles". By "empty" or "the empty rectangle" we mean an unspecified
   empty rectangle. A "bad rectangle" (which is not a rectangle at all,
   according to the previous definition) is any quadruple of REALs
   (w,e,n,s) with w>=e or n>=s. *)

(* NOTATION: By <w,e,n,s> we mean empty if w>=e or n>=s, (w,e,n,s) otherwise.
   By |w,e,n,s| we mean empty if w=e or n=s; otherwise the rectangle
   determined by the intervals |w,e| and |n,s| (see Interval.def). *)

(* LAWS: (1) Programs which assume or generate bad rectangles are illegal. (2)
   All empty rectangles are identified; programs which discriminate between
   different empty rectangles are illegal. (3) The operations in this module
   are undefined on bad rectangles. *)

TYPE T = RECORD west, east, north, south: REAL END;

TYPE Edge = {W, E, N, S};

TYPE Vertex = {NW, NE, SW, SE};

CONST Empty = T {0.0, -1.0, 0.0, -1.0};  (* An empty rectangle *)

EXCEPTION Error; (* Client error *)

(* --- Initialization --- *)

PROCEDURE FromEdges (w, e, n, s: REAL): T RAISES {};
(* Returns the rectangle <w,e,n,s> *)

PROCEDURE FromAbsEdges (w, e, n, s: REAL): T RAISES {};
(* Returns the rectangle |w,e,n,s| *)

PROCEDURE Float(READONLY r: Rect.T): T;
(* The set of all REAL points (h,v) such that h >= r.west, h < r.east,
   v >= r.north, and v < r.south. Same as FromEdges(FLOAT(r.west),
   FLOAT(r.east) - epsilon, FLOAT(r.north), FLOAT(r.south) - epsilon) *)

PROCEDURE Floor(READONLY r: T): Rect.T;
(* Rounds all points in the rectangle down to integer coordinates,
and returns enclosing integer rectangle. *)

PROCEDURE Round(READONLY r: T): Rect.T;
(* Rounds all points in the rectnagle to the nearest integer coords,
and returns enclosing integer rectangle. *)

PROCEDURE FromPoint (READONLY p: RealPoint.T): T RAISES {};
(* Returns the rectangle whose only member is p *)

PROCEDURE FromCorners (READONLY p, q: RealPoint.T): T RAISES {};
(* Returns the rectangle |p.h,q.h,p.v,q.v| *)

PROCEDURE FromCorner (READONLY p: RealPoint.T; hor, ver: REAL): T RAISES {};
(* Returns the rectangle <p.h,p.v,p.h+hor,p.v+ver> *)

PROCEDURE FromSize (hor, ver: REAL): T RAISES {};
(* Returns the rectangle <0.0,0.0,hor,ver> *)

PROCEDURE Center (READONLY r: T; READONLY p: RealPoint.T): T RAISES {};
(* Return a rectangle congruent to r, with middle at p *)

PROCEDURE FromIntervals (READONLY hor, ver: RealInterval.T): T RAISES {};
(* Returns the rectangle <hor.lo, hor.hi, ver.lo, ver.hi> *)

PROCEDURE FromAxes (axis: Axis.T; READONLY n, m: RealInterval.T): T RAISES {};
(* If axis=Hor then FromIntervals(n,m), else FromIntervals(m,n) *)

(* --- Selection --- *)

PROCEDURE NorthWest (READONLY r: T): RealPoint.T RAISES {};
(* north-west point *)

PROCEDURE NorthEast (READONLY r: T): RealPoint.T RAISES {};
(* north-east point *)

PROCEDURE SouthWest (READONLY r: T): RealPoint.T RAISES {};
(* south-west point *)

PROCEDURE SouthEast (READONLY r: T): RealPoint.T RAISES {};
(* south-east point *)

PROCEDURE GetVertex (v: Vertex; READONLY r: T): RealPoint.T RAISES {};
(* the point corresponding to the vertex v of r; origin if r is empty *)

PROCEDURE HorSize (READONLY r: T): REAL RAISES {};
(* r.east - r.west *)

PROCEDURE VerSize (READONLY r: T): REAL RAISES {};
(* r.south - r.north *)

PROCEDURE Size (a: Axis.T; READONLY r: T): REAL RAISES {};
(* HorSize(r) if a=Hor; VerSize(r) if a=Ver *)

PROCEDURE DiagSizeSquare (READONLY r: T): REAL RAISES {};
(* HorSize(r)**2.0+VerSize(r)**2.0 *)

PROCEDURE Middle (READONLY r: T): RealPoint.T RAISES {};
(* (floor((r.west+r.east)/2.0), floor((r.north+r.south)/2.0)); an unspecified
   point if empty *)

PROCEDURE PickEdge (READONLY r: T; READONLY p: RealPoint.T): Edge RAISES {};
(* Return the edge of r closest to p (one of them if not unique) *)

PROCEDURE PickVertex(READONLY r: T; READONLY p: RealPoint.T): Vertex RAISES {};
(* Return the vertex of r closest to p (one of them if not unique) *)

PROCEDURE Project (READONLY r: T; 
                   READONLY p: RealPoint.T): RealPoint.T RAISES {Error};
(* Return the point in r that is closest to p. r must be non-empty *)

(* --- Transformation --- *)

PROCEDURE Add (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error};
(* increment r.e and r.w by p.h; increment r.n and r.s by p.v *)

PROCEDURE Sub (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error};
(* decrement r.e and r.w by p.h; decrement r.n and r.s by p.v *)

PROCEDURE Move (READONLY r: T; READONLY p: RealPoint.T): T RAISES {Error};
(* increment r.e and r.w by p.h; increment r.n and r.s by p.v *)

PROCEDURE MoveH (READONLY r: T; h: REAL): T RAISES {Error};
(* increment r.e and r.w by h *)

PROCEDURE MoveV (READONLY r: T; v: REAL): T RAISES {Error};
(* increment r.n and r.s by v *)

PROCEDURE MoveHV (READONLY r: T; h, v: REAL): T RAISES {Error};
(* increment r.e and r.w by h, r.n and r.s by v *)

PROCEDURE Scale (READONLY r: T; factor: REAL): T RAISES {};
(* scale a rectangle by a fraction *)

PROCEDURE Inset (READONLY r: T; n: REAL): T RAISES {};
(* If r is emtpy return emtpy, else return <r.w+n,r.e-n,r.n+n,r.s-n> *)

PROCEDURE Change (READONLY r: T; dw, de, dn, ds: REAL): T RAISES {};
(* If r is empty return empty, else return <r.w+dw,r.e+de,r.n+dn,r.s+ds> *)

PROCEDURE MoveEdge (READONLY r: T; e: Edge; dn: REAL): T RAISES {};
(* If r is empty return empty, else move the edge e of r by dn in the positive
   direction of the axis perpendicular to it *)

PROCEDURE MoveVertex (READONLY r: T; v: Vertex; READONLY dp: RealPoint.T): T
  RAISES {};
(* If r is empty return empty, else move the vertex v of r by dp in the
   northwest-southeast direction *)

PROCEDURE Stretch (READONLY r: T; axis: Axis.T; lo, hi: REAL): T RAISES {};
(* If r is empty return empty, else change the interval of r determined by
   axis. *)

PROCEDURE Join (READONLY r, s: T): T RAISES {};
(* the least rectangle including the union of the points in r and s *)

PROCEDURE Meet (READONLY r, s: T): T RAISES {};
(* the largest rectangle included in the intersection of r and s *)

PROCEDURE Extend (READONLY r: T; READONLY p: RealPoint.T): T RAISES {};
(* Returns Join(r,FromPoint(p)) *)

PROCEDURE Chop (hv: Axis.T; READONLY r: T; n: REAL; VAR (*out*) s, t: T)
  RAISES {};
(* Chop a rectangle in two. If hv=Ver, s and t become the top and bottom parts
   of r resp. If hv=Hor, s and t become the left and right parts of r resp. *)

TYPE Partition = ARRAY [0..4] OF T;

PROCEDURE Factor (READONLY r, by: T; VAR (*out*) f: Partition; dh, dv: REAL)
  RAISES {};

(* r is partitioned into 5 pieces f[0]..f[4], where f[2] = Meet(r,by). The
   order of f is such that if i<j then f[i] translated by (dh,dv) doesn't
   intersect f[j]. (Only the sign of dh and dv affects the order, not their
   magnitude.) *)

PROCEDURE Mod (READONLY p: RealPoint.T; READONLY r: T): RealPoint.T 
  RAISES {Error};
(* Return the member of r whose distance from p in either coordinate is a
   multiple of the size of r in the corresponding coordinate. r must be
   non-empty *)

(* --- Test --- *)

PROCEDURE Equal (READONLY r, s: T): BOOLEAN RAISES {};
(* Rectangle equality; all empty rectangles are equal *)

PROCEDURE IsEmpty (READONLY r: T): BOOLEAN RAISES {};
(* whether r is empty *)

PROCEDURE Member (READONLY p: RealPoint.T; READONLY r: T): BOOLEAN RAISES {};
(* whether a point is in a rectangle *)

PROCEDURE Overlap (READONLY r, s: T): BOOLEAN RAISES {};
(* whether two rectangles overlap *)

PROCEDURE Subset (READONLY r, s: T): BOOLEAN RAISES {};
(* wheter r is a subset of s *)

(* --- Coordinate Transformation --- *)

PROCEDURE GlobToLoc (READONLY r: T; 
                     READONLY p: RealPoint.T): RealPoint.T RAISES {};
(* Transform p (in global coordinates) to the local coordinate system of r.
   Return RealPoint.Sub(p,NorthWest(r)) *)

PROCEDURE LocToGlob (READONLY r: T; 
                     READONLY p: RealPoint.T): RealPoint.T RAISES {};
(* Transform p (in the local coordinate system of r) to global coordinates.
   Returns RealPoint.Add(p,NorthWest(r)) *)

(* --- Standard type operations --- *)

PROCEDURE New (READONLY value: T): REF T;
(* Allocates and initializes a new heap value *)

PROCEDURE NewArray (size: CARDINAL;  READONLY value := Empty): REF ARRAY OF T;
(* Allocates a new array from the heap
  and initializes all its elements with the given value *)

PROCEDURE UntracedNew (READONLY value: T): UNTRACED REF T;
(* Allocates and initializes a new untraced value *)

PROCEDURE UntracedNewArray (size: CARDINAL;  READONLY value := Empty):
                                                       UNTRACED REF ARRAY OF T;
(* Allocates a new untraced array from the heap
  and initializes all its elements with the given value *)

PROCEDURE Compare (READONLY a, b: T): INTEGER;
(* == RETURN (-1 if Lt (a, b), 0 if Eq (a, b), +1 o. w.) *)

PROCEDURE Lt (READONLY a, b: T): BOOLEAN;
(* == RETURN lexicographic comparison of a, b by <w, e, n, s> *)

PROCEDURE Eq (READONLY a, b: T): BOOLEAN;
(* == RETURN (a = b) *)

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END RealRect.
