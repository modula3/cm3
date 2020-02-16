(* $Id$ *)

INTERFACE ScheduledRoute;
IMPORT RectSet, MagLabelList;

(* this interface describes an entry in the routing priority queue *)
(* Each ScheduledRoute.T runs from a set of rects to another set of *)
(* rects.  Compare will order the route entries such that the heaviest *)
(* one comes out first (no joke).  This will ensure that the heaviest *)
(* routes get done first, to save on wiring space. *)
(* (?---This is a heuristic.) *)

TYPE 
  T = RECORD 
    from, to : RectSet.T; 
    data : REFANY;
    multiplier : REAL;
    nominalCost : CARDINAL;
    labels : MagLabelList.T; (* used as identifier *)
  END;

CONST Brand = "ScheduledRoute";

PROCEDURE Compare(READONLY a, b : T) : [-1..1];

(* Equal is an ABORT.  Don't use ScheduledRoute.Equal, please. *)
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN; 

END ScheduledRoute.
