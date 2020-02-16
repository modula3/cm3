(* $Id$ *)
INTERFACE RectBinsClass;

IMPORT MagCell;
IMPORT MagLayerRect AS LayerRect;
IMPORT IntPair, IntPairSet;
IMPORT RectBins;
IMPORT MagLayerRectSet;

TYPE
  T <: Private;

  Private = RectBins.Public OBJECT
    step : CARDINAL;
    root : MagCell.T;
  METHODS
    initClass() : T;
    put(READONLY key : IntPair.T;
        READONLY val : LayerRect.T);
    get(READONLY key : IntPair.T; VAR val : MagLayerRectSet.T);
    flush(READONLY key : IntPair.T);
    initAtKeys(keys : IntPairSet.T);
  END;
    
END RectBinsClass.
