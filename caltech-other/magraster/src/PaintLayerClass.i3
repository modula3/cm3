(* $Id$ *)

INTERFACE PaintLayerClass;
IMPORT PaintLayer, BaseLayerList;
IMPORT MagLayer AS Layer;

REVEAL 
  PaintLayer.T <: Private;

TYPE
  Private = Layer.T OBJECT 
    baseLayers : BaseLayerList.T := NIL;
  METHODS
    init(canonicalName : TEXT) : PaintLayer.T;
    format() : TEXT;
  END;

END PaintLayerClass.
