(* $Id$ *)

MODULE GridPointSetWithDistance;
IMPORT GridPoint;
IMPORT GridPointSetRoutines;

PROCEDURE Distance(a, b : T) : LONGREAL =
  VAR
    ap, bp : GridPoint.T;
    d : CARDINAL;
  BEGIN
    GridPointSetRoutines.NearestPoints(a, b, ap, bp, d);
    RETURN FLOAT(d,LONGREAL)
  END Distance;

BEGIN END GridPointSetWithDistance.
