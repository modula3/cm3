(* $Id$ *)

MODULE LayerIntTbl;
IMPORT LayerIntImplTbl AS Impl;
IMPORT PixelTintTbl;
IMPORT MagLayer, BaseLayer;

REVEAL
  Default = DefaultPublic BRANDED OBJECT
    layers : REF ARRAY OF MagLayer.T := NIL;
  OVERRIDES
    init := Init;
    getLayer := GetLayer; (* backwards lookup *)
  END;

PROCEDURE Init(self : Default; sizeHint: CARDINAL): Impl.Default =
  BEGIN
    self.pixelTintTbl := NEW(PixelTintTbl.Default).init();
    RETURN Impl.Default.init(self, sizeHint)
  END Init;

(* inefficient for now *)
(* future plan: use a REF ARRAY OF Layer.T in Default, above *)
(* override put method to NIL that array when new elements are added *)
PROCEDURE GetLayer(self : Default; n : CARDINAL) : MagLayer.T =
  VAR
    iter := self.iterate();
    int : INTEGER;
    layer : BaseLayer.T;
  BEGIN
    WHILE iter.next(layer, int) DO
      IF int = n THEN RETURN layer END
    END;
    RETURN NIL
  END GetLayer;

BEGIN END LayerIntTbl.
