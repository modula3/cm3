(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE TextItem EXPORTS TextItem;

IMPORT VBT, Rect, PaintOp,
  PointR, RectR, Trans, Focus, ItemClass, 
  ItemFont;

REVEAL T = Public BRANDED "TextItem" OBJECT
    OVERRIDES
      paint := Paint;
      hilite := Hilite;
      unhilite := Paint;
      move := Move;
    END;

PROCEDURE Paint (it: T; wn: VBT.T; focus: Focus.T) =
  BEGIN
    ColorPaint (it, wn, focus, Trans.Color2Op (it.color));
  END Paint;

PROCEDURE Hilite (it: T; wn: VBT.T; focus: Focus.T) =
  BEGIN
    ColorPaint (it, wn, focus, PaintOp.FromRGB (0.5, 0.5, 0.5));
  END Hilite; 


PROCEDURE ColorPaint (it: T; wn: VBT.T; focus: Focus.T; color: PaintOp.T) =
  VAR rect := Trans.RectB2W (it.box, focus);
  BEGIN
    IF NOT Rect.Overlap (VBT.Domain (wn), rect) THEN 
      RETURN 
    END;
    TRY
      VBT.PaintText (wn, rect, Trans.PointB2W (it.rp, focus), 
                     ItemFont.ToFont (it.font, focus.scale),
                     it.text, color);
    EXCEPT
    | ItemFont.Invisible => RETURN;
    | ItemFont.TooSmall => 
      (* VBT.PaintTint (wn, rect, color);*)
      VAR nw := Rect.NorthWest (rect);
          ne := Rect.NorthEast (rect);
      BEGIN
        VBT.Line (wn, rect, ne, nw, 
                  end := VBT.EndStyle.Butt, op := color);
      END;
    | ItemFont.TooBig => (* not raised ever *)
    END;
  END ColorPaint; 

PROCEDURE Move (it: T; delta: PointR.T) =
  BEGIN
    it.box := RectR.Add (it.box, delta);
    it.rp := PointR.Add (it.rp, delta);
  END Move;

BEGIN
END TextItem.
