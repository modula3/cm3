(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE RealPoint;
IMPORT Point;

PROCEDURE FromPoint(p: Point.T): T =
  BEGIN
    RETURN T{FLOAT(p.h), FLOAT(p.v)};
  END FromPoint;

PROCEDURE Round(p: T): Point.T =
  BEGIN
    RETURN Point.T{ROUND(p.h), ROUND(p.v)};
  END Round;

BEGIN
END RealPoint.
