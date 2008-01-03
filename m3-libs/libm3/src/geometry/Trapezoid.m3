(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Marc H. Brown                                            *)
(* Last modified on Tue Feb 11 16:25:16 PST 1992 by muller     *)
(*      modified on Tue Dec 17 13:11:37 PST 1991 by mhb        *)

MODULE Trapezoid;

IMPORT Point;

PROCEDURE FromEdges (y1, p1, q1: INTEGER; 
                     y2, p2, q2: INTEGER): T =
  VAR 
    t: T;
    west1 := MIN(p1, q1);
    east1 := MAX(p1, q1);
    west2 := MIN(p2, q2);
    east2 := MAX(p2, q2);
  BEGIN
    t.vlo := MIN(y1, y2);
    t.vhi := MAX(y1, y2);
    t.m1.n := y1 - y2;
    t.m1.d := west1 - west2;
    t.p1 := Point.FromCoords(west1, y1);
    t.m2.n := y1 - y2;
    t.m2.d := east1 - east2;
    t.p2 := Point.FromCoords(east1, y1);
    RETURN t;
  END FromEdges;

PROCEDURE FromVertices (READONLY p1, p2, q1, q2: Point.T): T =
  BEGIN
    RETURN FromEdges(p1.v, p1.h, p2.h, q1.v, q1.h, q2.h);
  END FromVertices;

PROCEDURE FromTriangle (READONLY a, b, c: Point.T): T =
  BEGIN
    IF a.v = b.v THEN 
      RETURN FromVertices(a, b, c, c)
    ELSIF a.v = c.v THEN
      RETURN FromVertices(a, c, b, b)
    ELSE
      RETURN FromVertices(b, c, a, a)
    END;
  END FromTriangle;

BEGIN
END Trapezoid.
