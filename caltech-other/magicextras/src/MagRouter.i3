(* $Id$ *)

INTERFACE MagRouter;
IMPORT MagCell, TextMagLayerTbl AS TextLayerTbl, LabelListSet;

(* the way in which the completed routes are communicated back to the
   caller is left unspecified here.. *)

(* is there really a point to splitting up "init" and "run"? *)

TYPE
  T = OBJECT METHODS
    init(layout       : MagCell.T;
         layerDBArg   : TextLayerTbl.T) : T;

    run(labelsSet    : LabelListSet.T;
        failedNodes  : LabelListSet.T;
        respectBbox : BOOLEAN);
(*
  run the router to route together the set of label lists in labelSet;
  any failed lists will be in failedNodes.
  
  will respect the bounding box of the original cell (or fail) if
  respectBbox = TRUE 
*)
  END;

END MagRouter.
