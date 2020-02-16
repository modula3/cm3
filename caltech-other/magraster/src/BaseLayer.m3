(* $Id$ *)

MODULE BaseLayer;
IMPORT MagLayer, Word, Integer;
IMPORT BaseLayerList, PaintLayer, PaintLayerClass;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self : T; canon : TEXT ) : PaintLayer.T =
  BEGIN
    self.baseLayers := BaseLayerList.List1(self);
    RETURN PaintLayer.T.init(self, canon)
  END Init;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;
PROCEDURE Hash(READONLY a : T) : Word.T = BEGIN RETURN MagLayer.Hash(a) END Hash;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] = 
  BEGIN RETURN Integer.Compare(a.priority, b.priority) END Compare;

BEGIN END BaseLayer.
