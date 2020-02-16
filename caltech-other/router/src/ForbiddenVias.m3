(* $Id$ *)

MODULE ForbiddenVias;
IMPORT MagCell, GridPoint, MagRect;
IMPORT IntSet, IntSetDef, RectIntSetTbl;
FROM MagicStuff IMPORT ViaSize, GridStep;
IMPORT MagLayer AS Layer;
IMPORT Conf;

REVEAL
  T = Public BRANDED Brand OBJECT
    tbl : RectIntSetTbl.T;
  OVERRIDES
    init := Init;
    isForbidden := IsForbidden;
  END;

PROCEDURE ObsVia(rect : MagRect.T) : BOOLEAN =
  BEGIN
    RETURN rect.ll.x MOD GridStep() = 0 AND rect.ll.y MOD GridStep() = 0 AND
           rect.ur.x - rect.ll.x = ViaSize() AND rect.ur.y - rect.ll.y = ViaSize() 
  END ObsVia;


PROCEDURE Scanner(rect : MagRect.T; layer : Layer.T; args : REFANY) =
<* FATAL Conf.LayerNotFound *>
  PROCEDURE Put(int : GridPoint.Layer) =
    VAR
      s : IntSet.T;
    BEGIN
      IF NOT self.tbl.get(rect,s) THEN 
        s := NEW(IntSetDef.T).init(); EVAL self.tbl.put(rect,s) 
      END;
      EVAL s.insert(int)
    END Put;

  VAR
    self : T := args; 
    obs := Conf.GetObstructedRoutingLayers(layer);

    (* we do not want "basic" paint to obstruct vias-- i.e., m1 on m1, etc. *)
    (* note that obs can be NIL... *)
    baseLayer := obs # NIL AND obs.tail = NIL AND Conf.LayerLookup(obs.head) = layer;
  BEGIN
    IF ObsVia(rect) AND NOT baseLayer THEN
      WHILE obs # NIL DO Put(obs.head); obs := obs.tail END
    END
  END Scanner;

PROCEDURE Init(self : T; layout : MagCell.T) : T =
  BEGIN
    self.tbl := NEW(RectIntSetTbl.Default).init();
    layout.flatClipMap(Scanner,self);
    RETURN self
  END Init;

PROCEDURE IsForbidden(self : T; 
                      viaRect : MagRect.T; level : GridPoint.Layer) : BOOLEAN =
  VAR
    s : IntSet.T;
  BEGIN
    <* ASSERT ObsVia(viaRect) *>
    RETURN self.tbl.get(viaRect, s) AND s.member(level) 
  END IsForbidden;

BEGIN END ForbiddenVias.
