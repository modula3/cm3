(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: RealPoint.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE RealPoint;
IMPORT Point;

TYPE
  T = RECORD
    h, v: REAL;
  END;

PROCEDURE FromPoint(p: Point.T): T;
PROCEDURE Round(p: T): Point.T;

END RealPoint.
