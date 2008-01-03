MODULE Test24 EXPORTS Main;

IMPORT Axis, PaintOp, Point, Rect, Region, ScrnPixmap, Trestle, VBT;

PROCEDURE Shape (<*UNUSED*> ch: VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := 100, pref := 100, hi := 101}
  END Shape;


PROCEDURE Repaint1 (v: VBT.Leaf; <*UNUSED*> READONLY bad: Region.T) =
  BEGIN
    VBT.PaintTint (v, Rect.Full, PaintOp.Bg);
    VBT.PaintTint (v, Rect.T{10,50,10,90}, PaintOp.FromRGB (1.0, 0.0, 0.0));
    VBT.PaintTint (v, Rect.T{50,90,10,90}, PaintOp.FromRGB (0.0, 1.0, 0.0));
  END Repaint1;

PROCEDURE Repaint2 (v: VBT.Leaf; <*UNUSED*> READONLY bad: Region.T) =
  VAR
    pm: ScrnPixmap.T;
    br: Region.T;
  BEGIN
    pm := VBT.Capture (v1, VBT.Domain (v1), br);
    IF pm # NIL THEN
      VBT.PaintScrnPixmap (v, Rect.Full, PaintOp.Copy, pm, Point.Origin);
    ELSE
      VBT.PaintTint (v, Rect.Full, PaintOp.FromRGB (0.0, 0.0, 1.0));
    END;
  END Repaint2;


VAR
  v1 := NEW (VBT.Leaf, repaint := Repaint1, shape := Shape);
  v2 := NEW (VBT.Leaf, repaint := Repaint2, shape := Shape);
BEGIN
  Trestle.Install (v1);
  Trestle.Install (v2);
  Trestle.AwaitDelete (v1);
END Test24.
