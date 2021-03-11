(* $Id$ *)

INTERFACE GridPointSetWithDistance;
IMPORT GridPointSet;

TYPE T = GridPointSet.T;

PROCEDURE Distance(a, b : T) : LONGREAL;

CONST Brand = GridPointSet.Brand;

END GridPointSetWithDistance.
