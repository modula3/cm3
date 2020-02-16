(* $Id$ *)
INTERFACE RouteState;
IMPORT RouteID;
IMPORT GridPoint, GridPointSet;

(* remember the state along a route *)

TYPE
  T <: Public;

  (* The methods of a RouteState.Public allocate a new RouteState and *)
  (* modify it as necessary to reflect the changes in the state       *)

  Public = OBJECT 
  METHODS
    (* add a new ripup of a route at given point *)
    addRipUp(of : RouteID.T; at : GridPoint.T; first := FALSE) : T;
    
    ripUpIterate() : RipUpIterator;
    (* iterate thru ripups *)

    firstRipUpIterate() : RipUpIterator;
    (* iterate thru FIRST ripups of each route *)
    
    rippedUpAnywhere(rte : RouteID.T) : BOOLEAN;
    rippedUpHere(rte : RouteID.T; p : GridPoint.T) : BOOLEAN;
    
    (* how many different RouteIDs are ripped up in this state? *)
    numRipUps() : CARDINAL;
    
    (* what points are ripped up in this state? *)
    ripUpPoints() : GridPointSet.T;

    previous() : T; (* what was the previous state *)
  END;

  RipUpIterator = OBJECT
  METHODS
    next(VAR deadPath : RouteID.T; VAR at : GridPoint.T) : BOOLEAN;
  END;

END RouteState.


