(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE Pointe;
(* Like Point.i3, but we have a brand. *)
IMPORT Point;
TYPE
  T = Point.T;
CONST
  Brand = "Point";
  Equal = Point.Equal;
  Hash = Point.Hash;

PROCEDURE Format(a: T): TEXT;
END Pointe.
