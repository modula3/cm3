(* $Id$ *)

INTERFACE CurrentlyRoutedTbl;
IMPORT GridPointStatusTbl, GridPointList, GridPoint;
IMPORT RouteTag;
IMPORT GridPointSet;

TYPE
  T <: Public;

  (* N.B. for this code to work, 
     all points along gpl must be DIRECT NEIGHBORS (for now---restriction
     may be lifted later) *)

  Super = GridPointStatusTbl.Default;

  Public = Super  OBJECT
  METHODS
    (* add a point list (path) *)
    addPointList(gpl : GridPointList.T; tag : RouteTag.T);

    (* delete one *)
    delPointList(gpl : GridPointList.T);

    (* is there ANYTHING at a gridpoint? *)
    gotSomething(at : GridPoint.T) : BOOLEAN;

    (* get all the points that match a certain tag... 
       returns empty set if no points *)
    getPointsByTag(tag : RouteTag.T) : GridPointSet.T;
  END;

CONST Brand = "CurrentlyRoutedSet";

END CurrentlyRoutedTbl.
