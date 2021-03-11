(* $Id$ *)

INTERFACE CompRoute;
IMPORT GridPointSet;

(* completed components route *)

TYPE
  T = OBJECT 
    activeEndPoints : GridPointSet.T;
    allPoints : GridPointSet.T;
  END;

CONST Brand = "CompRoute";

PROCEDURE Equal(a, b : T) : BOOLEAN;

END CompRoute.
