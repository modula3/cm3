(* $Id$ *)

INTERFACE RipUp;
IMPORT RouteState;
IMPORT Route, RouteID;

(* compute cost for killerPath to rip up deadPath *)
PROCEDURE Cost(killerRoute : Route.T; 
               deadRoute : RouteID.T; 
               state : RouteState.T) : INTEGER;


END RipUp.
