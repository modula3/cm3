(* $Id$ *)

INTERFACE Router;
IMPORT MagRouter, GridPointSet;

TYPE
  T = MagRouter.T OBJECT METHODS
    (* override the following method so that the router clears up the
       points in the graveyard; it is also permitted to clear up anything
       else as long as it properly reschedules the routes *)
    ripUpGraveyard(graveyard : GridPointSet.T);
  END;

END Router.
