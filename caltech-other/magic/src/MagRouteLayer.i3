(* $Id$ *)

INTERFACE MagRouteLayer;
IMPORT MagLayer AS Layer;
IMPORT MagLayerSet AS LayerSet;
IMPORT Word;

TYPE
  T <: Public;

  Public = Layer.T OBJECT 
    name : TEXT;
    connects : LayerSet.T; (* layers that are electrically connected to 
                              this one *)
  METHODS
    init(canonicalName : TEXT) : T;
    
    (* set the layer to be used for routing *)
    setRouting(level : CARDINAL);

    (* get the routing layer; abort if the layer hasn't been set for routing *)
    getHeight() : CARDINAL;

    (* add a routing via connection to the database *)
    addVia(targetLayer, viaLayer : T);

    (* find the layer that corresponds to the via layer between this
       layer and another *)
    getVia(targetLayer : T) : T;

    (* can this layer be used for routing? *)
    routable() : BOOLEAN;
    
  END;

CONST Brand = "Route " & Layer.Brand;

PROCEDURE Compare(a, b : T) : [-1..1];
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END MagRouteLayer.
