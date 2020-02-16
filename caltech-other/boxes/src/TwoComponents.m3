(* $Id$ *)

MODULE TwoComponents;
IMPORT GridPointSet;

REVEAL
  T = Public BRANDED Brand OBJECT
    cost : LONGREAL;
  OVERRIDES
    distanceEstimate := DistanceEstimate;
    init := Init;
  END;

PROCEDURE DistanceEstimate(self : T) : LONGREAL =
  BEGIN
    RETURN self.cost
  END DistanceEstimate;

PROCEDURE Init(t : T; c1, c2 : GridPointSet.T; cost : LONGREAL) : T =
  BEGIN
    t.c1 := c1; t.c2 := c2; t.cost := cost;
    RETURN t
  END Init;

BEGIN END TwoComponents.
