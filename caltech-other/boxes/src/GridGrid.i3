(* $Id$ *)

INTERFACE GridGrid;
IMPORT GridPoint, GridPointList;

TYPE 
  T <: Public;

  Public = OBJECT METHODS
    neighbors() : GridPointList.T;
  END;

CONST Brand = "GridGrid";

PROCEDURE MarkNeighbors(READONLY p, q : GridPoint.T; tag : REFANY);

PROCEDURE PointStatus(READONLY p : GridPoint.T) : T;

END GridGrid.
