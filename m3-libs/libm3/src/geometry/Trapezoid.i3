(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Trapezoid.def, coded July 87 by Scott Seligman *)
(* Last modified on Tue Feb 11 16:25:00 PST 1992 by muller     *)
(*      modified on Tue Dec 17 13:11:39 PST 1991 by mhb        *)
(*      modified on Mon Nov  4 19:52:51 PST 1991 by gnelson    *)
(*      modified on Mon Sep  7 19:11:20 1987 by seligman       *)
<*PRAGMA LL*>

(* A "Trapezoid.T" represents a set of points lying in a quadrilateral
   whose north and south edges are horizontal and whose west and east
   edges have arbitrary non-horizontal slopes.  For example, a diagonal 
   line can be represented as a tall skinny trapezoid.  *)

INTERFACE Trapezoid;

IMPORT Point;

TYPE
  T = RECORD
    vlo, vhi: INTEGER; 
    m1, m2: Rational;
    p1, p2: Point.T; 
  END;
  Rational = RECORD n, d: INTEGER END;

(* For a trapezoid "tr", 

\medskip\bulletitem "tr.vlo" and "tr.vhi" are the "v" coordinates of its north and south edges, respectively;

\medskip\bulletitem "tr.m1" and "tr.m2" are the slopes of its west and east edges, respectively, as "(delta v) / (delta h)". A denominator
of zero represents an infinite slope; i.e., a vertical
edge. A numerator of zero is illegal. 

\medskip\bulletitem "tr.p1" and "tr.p2" are points on the infinite lines
that extend the west and east edges, respectively.  

\medskip
Trapezoids are closed on the north and west edges, open on the
south and east edges, closed on the northwest corner, and open on
the other corners. 

A "Rational" "q" represents the rational number "q.n/q.d". *)


PROCEDURE FromEdges (y1, p1, q1: INTEGER; 
                     y2, p2, q2: INTEGER): T;
(* Return a trapezoid whose vertices are "(p1, y1)", "(q1, y1)",
   "(p2, y2)", and "(q2, y2)". The altitude of the trapezoid must be
   non-zero. *)

PROCEDURE FromVertices (READONLY p1, p2, q1, q2: Point.T): T;
(* Return a trapezoid from four vertices. The "p1" and "p2" vertices
   must have the same y-coordinates, as must the "q1" and "q2" vertices. 
   Furthermore, the altitude of the trapezoid must be non-zero. *)

PROCEDURE FromTriangle (READONLY a, b, c: Point.T): T;
(* Return a trapezoid from the vertices of a triangle. One of the sides of
   the triangle must be parallel to the x-axis, and the triangle's altitude
   must be non-zero. *)

END Trapezoid.
