(* $Id$ *)
INTERFACE GridPointSetRoutines;
IMPORT GridPointSet;
IMPORT GridPoint;

TYPE
  T = GridPointSet.T;

PROCEDURE NearestPoints(a, b : T; 
                        VAR aPt, bPt : GridPoint.T; VAR dist : CARDINAL);
PROCEDURE NearestPoint(a : GridPoint.T; inSet : T) : GridPoint.T;
PROCEDURE CostExtent(a : T) : CARDINAL;
PROCEDURE Format(a : T) : TEXT;

END GridPointSetRoutines.
