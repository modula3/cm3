(* $Id$ *)

INTERFACE Loc;
IMPORT GridPoint;

TYPE
  T = RECORD 
    prev : GridPoint.T;
    point : GridPoint.T;
    minCost : CARDINAL;  (* actual cost of going this far *)
    state : REFANY; (* normally RouteState.T, but 
                       also used to store on recycle queue *)
    remains : [-1..LAST(CARDINAL) ] := -1;
  END;

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

CONST
  Brand = "Location";

END Loc.

