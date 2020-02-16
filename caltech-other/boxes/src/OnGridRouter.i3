(* $Id$ *)

INTERFACE OnGridRouter;

(* router for routing on grid ONLY *)

IMPORT GridPointSet, GridPointSetSeq, Bbox;
IMPORT Wr, IntSet;
IMPORT IntCompRouteTbl;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(bbox : Bbox.T; delWr : Wr.T := NIL) : T;
    addNet(numbered : CARDINAL; components : GridPointSetSeq.T;
           nonObstacles : GridPointSet.T);
    (* nonObstacles are "extra" non-obstacles *)

    setDeferrals(deferrals : IntSet.T);

    close(); (* signal no more changes *)

    run(respectBbox : BOOLEAN;
        (* OUT *)VAR failed : IntSet.T);

    getRoutes() : IntCompRouteTbl.T; 
    (* includes deferred routes? *)

    setQuittingRules(threshold : LONGREAL;
                     failures : CARDINAL;
                     fileName : TEXT);
    (* set up rules for quitting early and printing a quitfile.
       quitfile will be written to fileName and quitting will occur as
       soon as (1) the velocity falls under the threshold or (2) the 
       failures are exceeded *)
  END;

CONST Brand = "OnGridRouter";

END OnGridRouter.
