(* $Id$ *)

INTERFACE OnGridComponents;
IMPORT Components, GridPointSet, GridPointSetSeq, GridPointRouteIDTbl, Wr;
IMPORT RouteID;

TYPE
  T <: Public;

  Public = Components.T OBJECT METHODS
    init(initComponents : GridPointSetSeq.T;
         delWr : Wr.T;
         id : RouteID.T;
         (* OUT *) specialEndPtsTbl : GridPointRouteIDTbl.T := NIL) : T;

    allPoints() : GridPointSet.T;
    (* all DRAWN points---active endpoints and routing points *)
    
    activeEndPoints() : GridPointSet.T;
    (* which endpoints are active? *)

    cleanOutNonArts(deleted : GridPointSet.T;writeToDelWr : BOOLEAN := TRUE); 
    (* clean out all non-endpoint & non-articulation points *)
  END;

CONST Brand = "OnGridComponents";

END OnGridComponents.
