(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE RuleItem;

IMPORT VBT, Rect, PaintOp, 
  PointR, RectR, Trans, Focus, ItemClass;

REVEAL T = Public BRANDED "RuleItem" OBJECT
    OVERRIDES
      paint := Paint;
      hilite := Hilite;
      unhilite := Paint;
      move := Move;
    END;


PROCEDURE Paint (it: T; wn: VBT.T; focus: Focus.T) =
  VAR rect := Trans.RectB2W (it.box, focus);
  BEGIN
    IF NOT Rect.Overlap (VBT.Domain (wn), rect) 
     THEN RETURN 
    END;
    VBT.PaintTint (wn, rect,  Trans.Color2Op (it.color));
  END Paint;

PROCEDURE Hilite (it: T; wn: VBT.T; focus: Focus.T) =
  VAR rect := Trans.RectB2W (it.box, focus);
  BEGIN
    IF NOT Rect.Overlap (VBT.Domain (wn), rect) 
     THEN RETURN 
    END;
    VBT.PaintTint (wn, rect, PaintOp.FromRGB (0.5, 0.5, 0.5));
  END Hilite; 

PROCEDURE Move (it: T; delta: PointR.T) =
  BEGIN
    it.box := RectR.Add (it.box, delta);
  END Move;

BEGIN
END RuleItem. 
