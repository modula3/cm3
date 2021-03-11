(* $Id$ *)

INTERFACE Components;
IMPORT GridPointList, GridPointSet, GridPoint, Word;
IMPORT TwoComponents, Thread, Wr, RouteID, ForbiddenSteps;

EXCEPTION CantConnectThose(GPPair);

TYPE GPPair = RECORD p1, p2 : GridPoint.T END;

TYPE 
  T = OBJECT METHODS
    size() : CARDINAL;
(* how many components are there? suggestion: keep routing until
   there's only one! *)

    connect(gp : GridPointList.T) : GridPointList.T RAISES { CantConnectThose };
(* make a connection that goes through the specified gpl;
   returns the actual list of points connected (may be different if
   you've specified a route that goes back on itself...!)
   some connections may be illegal, they will raise an exception *)

    longest() : TwoComponents.T;
(* get endpoints in longest path in MST of all as-yet-unconnected nodes *)

    pointSet() : GridPointSet.T;

    deleteAPoint(gp : GridPoint.T);
(* delete a set of points from the components; this may delete more
   than the points in the set!  (must reschedule afterwards unless you
   want to leave gaps in the net) *)

    deleteAllEndPointNeighbors(ep : GridPoint.T);
(* delete all the non-endpoints neighboring an endpoint;
   call commitripups to unravel spaghetti after this *)

    commitRipups(writeToDelWr := TRUE) : GridPointSet.T RAISES { Wr.Failure, Thread.Alerted } ;
(* commit all deletions (do we need this?); the exceptions come from 
   failing to update the .deleted file (if applicable);
   returns all points deleted *)

    id() : RouteID.T;
(* get this net's identifier *)

    nonObstaclesForCurrentRoute() : GridPointSet.T;
(* un-mark (in boxes router) appropriate spots for 
   running a specific route *)

    nonEndPointsInNetForCurrentRoute() : GridPointSet.T;
(* the points that are IN this net already but are not endpoints
   (i.e., the points that it's OK to just route through for free! *)
    
    forbiddenStepsForCurrentRoute() : ForbiddenSteps.T;
(* forbiddensteps are the steps that are truly forbidden---the
   steps into and out of the endpoints that would require ripping
   up PRE-EXISTING layout *)

    approxSize() : LONGREAL;
(* the approximate size of MST in GridPoint units *)

    deleteAllPoints();
(* delete all the points---can be used to implement hierarchical routing *)

    iterateComponents() : ComponentIterator;

  END;

TYPE
  ComponentIterator = OBJECT METHODS
    next(VAR comp : GridPointSet.T) : BOOLEAN;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

CONST Brand = "Components";

END Components.
