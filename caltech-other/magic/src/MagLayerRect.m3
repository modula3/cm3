(* $Id$ *)
MODULE MagLayerRect;
IMPORT MagLayer, MagRect;
IMPORT Word;
IMPORT MagRouteLayer AS RouteLayer;
IMPORT Fmt;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN MagRect.Equal(a.rect, b.rect) AND MagLayer.Equal(a.layer, b.layer) END Equal;

PROCEDURE Hash(READONLY a : T): Word.T =
  BEGIN RETURN Word.Plus(MagRect.Hash(a.rect),MagLayer.Hash(a.layer)) END Hash;

PROCEDURE ElectricallyConnected(READONLY a, b : T) : BOOLEAN =
  VAR
    aLayer : RouteLayer.T := a.layer;
    bLayer : RouteLayer.T := b.layer;
    lRes : BOOLEAN;
  BEGIN
    lRes := aLayer.connects.member(bLayer);
    <* ASSERT lRes = bLayer.connects.member(aLayer) *>
    RETURN lRes AND MagRect.Overlap(a.rect, b.rect)
  END ElectricallyConnected;

PROCEDURE Distance(READONLY a, b : T; layerPenalty : CARDINAL) : CARDINAL =
  VAR 
    penalty := 0;
  BEGIN
    IF NOT MagLayer.Equal(a.layer, b.layer) THEN penalty := layerPenalty END;
    
    RETURN MagRect.Distance(a.rect, b.rect) + penalty
  END Distance;

PROCEDURE Format(READONLY a : T) : TEXT =
  VAR
    lay : TEXT;
  BEGIN
    IF ISTYPE(a.layer,RouteLayer.T) THEN
      lay := NARROW(a.layer,RouteLayer.T).name
    ELSE
      lay := "UNKNOWN"
    END;
    RETURN Fmt.F("%s %s %s %s %s",
                 Fmt.Int(a.rect.ll.x), Fmt.Int(a.rect.ll.y),
                 Fmt.Int(a.rect.ur.x), Fmt.Int(a.rect.ur.y),
                 lay)
  END Format;

BEGIN END MagLayerRect.
