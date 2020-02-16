MODULE PaintLayer;
IMPORT MagLayer, PaintLayerClass;

REVEAL
  T = PaintLayerClass.Private BRANDED Brand OBJECT
    canon : TEXT;
  OVERRIDES
    init := Init;
    format := Format;
  END;

PROCEDURE Init(self : T; canonicalName : TEXT) : T = 
  BEGIN
    self.canon := canonicalName;
    RETURN MagLayer.T.init(self);
  END Init;

PROCEDURE Format(self : T) : TEXT = BEGIN RETURN self.canon END Format;

BEGIN END PaintLayer.
