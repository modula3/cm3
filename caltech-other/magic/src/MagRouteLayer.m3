(* $Id$ *)

MODULE MagRouteLayer;
IMPORT Integer;
IMPORT LayerLayerTbl;
IMPORT MagLayer AS Layer;
IMPORT Word;
IMPORT MagLayerSetDef AS LayerSetDef;

REVEAL 
  T = Public BRANDED Brand OBJECT 
    viaTbl : LayerLayerTbl.T;
    height : CARDINAL := LAST(CARDINAL);
  OVERRIDES 
    init := Init ;
    addVia := AddVia;
    getVia := GetVia;
    setRouting := SetRouting;
    routable := Routable;
    getHeight := GetHeight;
  END;

PROCEDURE GetHeight(self : T) : CARDINAL =
  BEGIN
    <* ASSERT self.height # LAST(CARDINAL) *>
    RETURN self.height
  END GetHeight;

PROCEDURE Init(self : T; canonicalName : TEXT) : T =
  BEGIN 
    EVAL Layer.T.init(self);
    self.connects := NEW(LayerSetDef.T).init();
    EVAL self.connects.insert(self);
    self.viaTbl := NEW(LayerLayerTbl.Default).init();
    self.name := canonicalName; 
    RETURN self 
  END Init;
    
PROCEDURE Routable( a : T ) : BOOLEAN = 
  BEGIN RETURN a.height # LAST(CARDINAL) END Routable;

PROCEDURE Compare(a, b : T) : [-1..1] = 
  BEGIN RETURN Integer.Compare(a.height, b.height) END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Layer.Hash(a) END Hash;

PROCEDURE AddVia(self : T; targetLayer, viaLayer : T) =
  VAR
    x : BOOLEAN;
  BEGIN 
    x := self.viaTbl.put(targetLayer, viaLayer);
    <* ASSERT NOT x *>
  END AddVia;

PROCEDURE GetVia(self : T; to : T) : T =
  VAR 
    x : BOOLEAN;
    res : Layer.T;
  BEGIN
    x := self.viaTbl.get(to, res);
    <* ASSERT x *>
    RETURN res
  END GetVia;

PROCEDURE SetRouting(self : T; level : CARDINAL) =
  BEGIN self.height := level END SetRouting;

BEGIN END MagRouteLayer.
