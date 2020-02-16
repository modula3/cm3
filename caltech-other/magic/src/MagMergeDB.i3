(* $Id$ *)

INTERFACE MagMergeDB;
IMPORT MagLayer, MagLayerList, MagLayerSet;

TYPE 
  T <: Public;

  Public = OBJECT METHODS
    init() : T;
    addEquivalence(compositeLayer : MagLayer.T;
                   componentLayers : MagLayerList.T);

    merge(unmerged : MagLayerSet.T) : MagLayerSet.T;

    isMergeable(layer : MagLayer.T) : BOOLEAN;

    debugDump();
  END;

CONST
  Brand = "MagMergeDB";

END MagMergeDB.
