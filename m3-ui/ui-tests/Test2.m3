MODULE Test2 EXPORTS Main;

IMPORT PaintOp, Path, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  VAR
    path := NEW (Path.T);
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    Path.MoveTo (path, Point.T {10,10});
    Path.LineTo (path, Point.T {30,20});
    Path.LineTo (path, Point.T {50,10});
    Path.LineTo (path, Point.T {40,30});
    Path.LineTo (path, Point.T {50,50});
    Path.LineTo (path, Point.T {30,40});
    Path.LineTo (path, Point.T {10,50});
    Path.LineTo (path, Point.T {20,30});
    Path.Close (path);
    VBT.Stroke (self, Rect.Full, path);
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test2.
