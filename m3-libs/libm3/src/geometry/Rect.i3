(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 17:11:59 PDT 1993 by swart      *)
(*      modified on Mon Nov 18 23:09:06 PST 1991 by gnelson    *)
(*      modified on Thu Nov  2 18:28:27 1989 by muller         *)
(*      modified on Mon Oct  2 10:21:21 1989 by kalsow         *)
(*      modified on Fri Jun  3 16:22:22 PDT 1988 by glassman   *)
(*      modified on Thu Aug 13 18:24:20 PDT 1987 by luca       *)
(*      modified on Thu Mar 5 17:17:13 1987 by msm             *)

(* A "Rect.T" is a set of points lying in a rectangle with its sides
   parallel to the coordinate axes.  The directions of the screen are
   named after the compass points, with north at the top.  A rectangle
   "rect" contains a point "pt" if
   
   | pt.h `is in` [rect.west .. rect.east - 1]  AND
   | pt.v `is in` [rect.north .. rect.south - 1]

   We impose the restriction that if a rectangle contains no points,
   then it must be equal as a record to "Rect.Empty".  *)

INTERFACE Rect;

IMPORT Axis, Interval, Point;

TYPE T = RECORD west, east, north, south: INTEGER END;

TYPE Edge = {W, E, N, S};

TYPE Vertex = {NW, NE, SW, SE};

CONST Empty = T {0,0,0,0};  (* An empty rectangle *)

CONST Full = T {FIRST(INTEGER), LAST(INTEGER), FIRST(INTEGER), LAST(INTEGER)};
                (* The biggest possible rectangle *)

(* --- Initialization --- *)

PROCEDURE FromEdges(w, e, n, s: INTEGER): T;
(* If "w >= e" or "n >= s" return "Empty", else return "T{w,e,n,s}". *)

PROCEDURE FromAbsEdges(h1, h2, v1, v2: INTEGER): T;
(* Return 

| FromEdges(MIN(h1,h2), MAX(h1,h2), 
|           MIN(v1,v2), MAX(v1,v2))

*)

PROCEDURE FromPoint(READONLY p: Point.T): T;
(* Return the rectangle whose only element is "p". *)

PROCEDURE FromCorners(READONLY p, q: Point.T): T;
(* Return "FromAbsEdges(p.h,q.h,p.v,q.v)". *)

PROCEDURE FromCorner(
  READONLY p: Point.T; 
  hor, ver: CARDINAL): T;
(* Return "FromEdges(p.h, p.h+hor, p.v, p.v+ver)". *)

PROCEDURE FromSize(hor, ver: CARDINAL): T;
(* Return "FromCorner(Point.Origin,hor,ver)". *)

PROCEDURE Center(READONLY r: T; READONLY p: Point.T): T;
(* If "r" is empty then return "Empty" else return a rectangle "s"
   such that "Congruent(r, s)" and "Middle(s)=p". *)

PROCEDURE FromIntervals
  (READONLY hor, ver: Interval.T): T;
(* Return "FromEdges(hor.lo,hor.hi,ver.lo,ver.hi)". *)

PROCEDURE FromAxes (axis: Axis.T; READONLY n, m: Interval.T): T;
(* If axis=Hor then FromIntervals(n,m), else FromIntervals(m,n) *)

(* --- Selection --- *)

PROCEDURE NorthWest(READONLY r: T): Point.T;
(* Return "Point.T{r.west,r.north}". *)

PROCEDURE NorthEast(READONLY r: T): Point.T;
(* Return "Point.T{r.east,r.north}". *)

PROCEDURE SouthWest(READONLY r: T): Point.T;
(* Return "Point.T{r.west,r.south}". *)

PROCEDURE SouthEast(READONLY r: T): Point.T;
(* Return "Point.T{r.east,r.south}". *)

PROCEDURE GetVertex (v: Vertex; READONLY r: T): Point.T;
(* the point corresponding to the vertex v of r; origin if r is empty *)

PROCEDURE HorSize(READONLY r: T): CARDINAL;
(* Return "r.east - r.west". *)

PROCEDURE VerSize(READONLY r: T): CARDINAL;
(* Return "r.south - r.north". *)

PROCEDURE Size (a: Axis.T; READONLY r: T): CARDINAL;
(* HorSize(r) if a=Hor; VerSize(r) if a=Ver *)

PROCEDURE DiagSizeSquare (READONLY r: T): CARDINAL;
(* HorSize(r)**2+VerSize(r)**2 *)

PROCEDURE Middle(READONLY r: T): Point.T;
(* Return "Point.T{r.west+r.east DIV 2, r.north+r.south DIV 2}". *)

PROCEDURE PickEdge (READONLY r: T; READONLY p: Point.T): Edge;
(* Return the edge of r closest to p (one of them if not unique) *)

PROCEDURE PickVertex (READONLY r: T; READONLY p: Point.T): Vertex;
(* Return the vertex of r closest to p (one of them if not unique) *)

PROCEDURE Project(READONLY r: T; 
  READONLY p: Point.T): Point.T;
(* Return the element of "r" that is closest to "p". This is a
   checked runtime error if "r" is empty. *)

(* --- Transformation --- *)

PROCEDURE Add(READONLY r: T; READONLY p: Point.T): T;
(* Return

| FromEdges(r.west+p.h, r.east+p.h, 
|           r.north+p.v,r.south+p.v)

*)

PROCEDURE Sub(READONLY r: T; READONLY p: Point.T): T;
(* Return "Add(r, Point.Minus(p))". *)

PROCEDURE Move (READONLY r: T; READONLY p: Point.T): T;
(* increment r.e and r.w by p.h; increment r.n and r.s by p.v *)

PROCEDURE MoveH (READONLY r: T; h: INTEGER): T;
(* increment r.e and r.w by h *)

PROCEDURE MoveV (READONLY r: T; v: INTEGER): T;
(* increment r.n and r.s by v *)

PROCEDURE MoveHV (READONLY r: T; h, v: INTEGER): T;
(* increment r.e and r.w by h, r.n and r.s by v *)

PROCEDURE Scale (READONLY r: T; num, den: INTEGER): T;
(* scale a rectangle by a fraction *)

PROCEDURE Change
  (READONLY r: T; dw,de,dn,ds: INTEGER): T;
(* If "r" is empty return "Empty", else return the rectangle
   "FromEdges(r.west+dw, r.east+de, r.north+dn, r.south+ds)". *)

PROCEDURE Inset(READONLY r: T; n: INTEGER): T;
(* Return "Change(r, n, -n, n, -n)". *)

PROCEDURE Transpose(READONLY r: T; ax := Axis.T.Ver): T;
(* If "r" is empty or if "ax = Axis.Hor", then return 
   "r", else return "T{r.north, r.south, r.west, r.east}". *)

PROCEDURE MoveEdge (READONLY r: T; e: Edge; dn: INTEGER): T;
(* If r is empty return empty, else move the edge e of r by dn in the positive
   direction of the axis perpendicular to it *)

PROCEDURE MoveVertex (READONLY r: T; v: Vertex; READONLY dp: Point.T): T
 ;
(* If r is empty return empty, else move the vertex v of r by dp in the
   northwest-southeast direction *)

PROCEDURE Stretch (READONLY r: T; axis: Axis.T; lo, hi: INTEGER): T;
(* If r is empty return empty, else change the interval of r determined by
   axis. *)

PROCEDURE Join(READONLY r, s: T): T;
(* Return the smallest rectangle containing both "r" and "s". *)

PROCEDURE Meet(READONLY r, s: T): T;
(* Return the largest rectangle contained in both "r" and "s". *)

PROCEDURE Extend (READONLY r: T; READONLY p: Point.T): T;
(* Returns Join(r,FromPoint(p)) *)

PROCEDURE Chop (hv: Axis.T; READONLY r: T; n: INTEGER; VAR (*out*) s, t: T)
 ;
(* Chop a rectangle in two. If hv=Ver, s and t become the top and bottom parts
   of r resp. If hv=Hor, s and t become the left and right parts of r resp. *)

TYPE Partition = ARRAY [0..4] OF T;

PROCEDURE Factor(
  READONLY r, s: T; 
  VAR (*out*) f: Partition; 
  dh, dv: INTEGER) ;
(* Partition "r" into "5" pieces "f[0]..f[4]" where "f[2] = Meet(r,s)",
   and the other rectangles in "f" partition the set difference "r-s". *)

(* The order of "f" is such that if "i<j" then "f[i]" translated by
   any positive multiple of "(dh,dv)" doesn't intersect "f[j]".  (Only
   the signs of "dh" and "dv" affect the order, not their magnitude.)  *)

PROCEDURE Mod(READONLY p: Point.T; 
  READONLY r: T): Point.T;
(* Return the element of "r" whose distance from "p" in each axis is a
   multiple of the size of "r" in that axis. This is a checked runtime 
   error if "r" is empty. *)

(* --- Test --- *)

PROCEDURE Equal (READONLY r, s: T): BOOLEAN;
(* Rectangle equality; all empty rectangles are equal *)

PROCEDURE IsEmpty(READONLY r: T): BOOLEAN;
(* Return whether "r" is empty. *)

PROCEDURE Member(READONLY p: Point.T; 
  READONLY r: T): BOOLEAN;
(* Return whether p is in r. *)

PROCEDURE Overlap(READONLY r, s: T): BOOLEAN;
(* Return whether "r" and "s" have any element in common. *)

PROCEDURE Subset(READONLY r, s: T): BOOLEAN;
(* Return whether "r" is contained in "s". *)

PROCEDURE Congruent(READONLY r, s: T): BOOLEAN;
(* Return whether "r" and "s" are congruent, that is,
   whether they have the same height and width. *)

(* --- Coordinate Transformation --- *)

PROCEDURE GlobToLoc (READONLY r: T; READONLY p: Point.T): Point.T;
(* Transform p (in global coordinates) to the local coordinate system of r.
   Return Point.Sub(p,NorthWest(r)) *)

PROCEDURE LocToGlob (READONLY r: T; READONLY p: Point.T): Point.T;
(* Transform p (in the local coordinate system of r) to global coordinates.
   Returns Point.Add(p,NorthWest(r)) *)

(* --- Standard type operations --- *)

PROCEDURE Compare (READONLY a, b: T): INTEGER;
(* == lexicographic comparison of a, b by <w, e, n, s> *)

PROCEDURE Hash (READONLY a: T): INTEGER;
(* == RETURN a suitable hash value  *)

END Rect.
