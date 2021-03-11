(* $Id$ *)

INTERFACE ForbiddenSteps;
IMPORT GridPoint;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    isForbidden(READONLY a, b : GridPoint.T) : BOOLEAN;
    addPair(READONLY a, b : GridPoint.T);
    copy() : T;
  END;

CONST Brand = "ForbiddenSteps";

END ForbiddenSteps.
