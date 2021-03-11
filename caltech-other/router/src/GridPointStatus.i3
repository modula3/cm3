(* $Id$ *)

INTERFACE GridPointStatus;
IMPORT GridPoint, GridPointSet, Word, RouteTag;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    routeNeighbors() : GridPointSet.T;
    
    (* we should really add a layout session here, so it can be
       ripped up.. hmm maybe not? *)
    init(READONLY me : GridPoint.T; isEnd : BOOLEAN; tag : RouteTag.T) : T;
    addANeighbor(READONLY neighbor : GridPoint.T);
    delANeighbor(READONLY neighbor : GridPoint.T);

    size() : CARDINAL; (* how many neighbors do we connect to? *)

    (* hmm, what do we want for this...???? *)
    addLayoutSession();

    getTag() : RouteTag.T;
    
  END;

CONST Brand = "GridPointStatus";

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

END GridPointStatus.
