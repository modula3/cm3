MODULE Test4 EXPORTS Main;

IMPORT PaintOp, Path, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  VAR
    path := NEW (Path.T);
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, pt := Point.T {20, 20}, t := "Testing PaintText");
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test4.
