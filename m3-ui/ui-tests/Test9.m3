MODULE Test9 EXPORTS Main;

IMPORT PaintOp, Point, Rect, Region, Trestle, VBT;


PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, pt := Point.T{20,20}, t := "Hello");
    VBT.Scroll (self, Rect.T {10, 100, 40, 60}, Point.T {0,30});
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test9.
