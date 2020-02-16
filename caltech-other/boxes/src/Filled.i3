(* $Id$ *)

INTERFACE Filled;
IMPORT GridPoint, GridPointSet;
IMPORT RouteID;
IMPORT Wr, Rd, Thread, Pickle;

TYPE T <: REFANY;

PROCEDURE Marked(p : GridPoint.T) : BOOLEAN;

PROCEDURE MarkedRoute(p : GridPoint.T) : RouteID.T;

(* 
   mark a GridPoint.T as blocked by path.  If path is NIL that means
   the router can't rip it up (e.g., obstacles already there before
   the router runs for the first time)
*)
PROCEDURE Mark(p : GridPoint.T; path : RouteID.T);

(* mark something so that it cannot be unmarked *)
PROCEDURE MarkAsRigid(p : GridPoint.T);

(* the nonObstacles are points that it's OK if they are already marked;
   no other points in the segment are allowed to be marked *)
PROCEDURE MarkSegment(from, to : GridPoint.T; path : RouteID.T;
                      nonObstacles : GridPointSet.T);

PROCEDURE UnMark(p : GridPoint.T);

PROCEDURE UnMarkSegment(from, to : GridPoint.T);

(* call the following if you don't know every point is marked or not
   (could be b/c of Steiner tree) *)
PROCEDURE UnMarkSegmentWhereMarked(from, to : GridPoint.T);

(* make a new plane to work in *)
PROCEDURE New() : T;

PROCEDURE DumpToWriter(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted};
(* ASCII dump to writer currently doesn't work.. *)

PROCEDURE PickleToWriter(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted, Pickle.Error };
(* pickle and write to writer *)

PROCEDURE UnPickleFromReader(rd : Rd.T) RAISES { Rd.Failure, Thread.Alerted,
                                                 Rd.EndOfFile, Pickle.Error };
(* opposite of above *)

END Filled.

