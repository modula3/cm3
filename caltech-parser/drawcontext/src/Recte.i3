(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Recte.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE Recte;
(* Like Rect.i3, but my implementation doesn't have any problem
   with infinitely thin rectangles *)
IMPORT Point;
IMPORT Rect;
TYPE
  T = Rect.T;

PROCEDURE FromCorners(READONLY p, q: Point.T): T;
PROCEDURE Inset(READONLY r: T; n: INTEGER): T;
(* ought to have an IsEmpty *)

END Recte.
