(* $Id$ *)

INTERFACE TwoComponents;
IMPORT GridPointSet;

TYPE
  T <: Public;

  Public = OBJECT
    c1, c2 : GridPointSet.T;
  METHODS
    init(c1, c2 : GridPointSet.T; cost : LONGREAL) : T;
    distanceEstimate() : LONGREAL
  END;

CONST Brand = "TwoComponents";
  

END TwoComponents.
