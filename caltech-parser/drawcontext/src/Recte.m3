(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Recte.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Recte;
IMPORT Point;

PROCEDURE FromAbsEdges (w, e, n, s: INTEGER): T RAISES {} =
  VAR r: T;
  BEGIN
(*    IF (w = e) OR (n = s) THEN RETURN Empty;  END; *)
    IF (w < e) THEN
      r.west := w;
      r.east := e;
    ELSE
      r.west := e;
      r.east := w;
    END;
    IF (n < s) THEN
      r.north := n;
      r.south := s;
    ELSE
      r.north := s;
      r.south := n;
    END;
    RETURN r;
  END FromAbsEdges;

PROCEDURE FromCorners (READONLY p, q: Point.T): T RAISES {} =
  BEGIN
    RETURN FromAbsEdges (p.h, q.h, p.v, q.v);
  END FromCorners;


PROCEDURE Inset (READONLY r: T; n: INTEGER): T RAISES {} =
  VAR s: T;
  BEGIN
(*    IF (r.west >= r.east) OR (r.north >= r.south) THEN RETURN Empty END; *)
    s.west := r.west + n;
    s.east := r.east - n;
    s.north := r.north + n;
    s.south := r.south - n;
(*    IF (s.west >= s.east) OR (s.north >= s.south) THEN RETURN Empty;  END; *)
    RETURN s;
  END Inset;

BEGIN
END Recte.
