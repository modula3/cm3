MODULE Test13 EXPORTS Main;

IMPORT PaintOp, Path, Pixmap, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {10,10});
      Path.LineTo (path, Point.T {30,20});
      Path.LineTo (path, Point.T {50,10});
      Path.LineTo (path, Point.T {40,30});
      Path.LineTo (path, Point.T {50,50});
      Path.LineTo (path, Point.T {30,40});
      Path.LineTo (path, Point.T {10,50});
      Path.LineTo (path, Point.T {20,30});
      Path.Close (path);
      VBT.Fill (self, Rect.Full, path, src := Pixmap.Solid);
    END;
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {110,10});
      Path.LineTo (path, Point.T {130,20});
      Path.LineTo (path, Point.T {150,10});
      Path.LineTo (path, Point.T {140,30});
      Path.LineTo (path, Point.T {150,50});
      Path.LineTo (path, Point.T {130,40});
      Path.LineTo (path, Point.T {110,50});
      Path.LineTo (path, Point.T {120,30});
      Path.Close (path);
      VBT.Fill (self, Rect.Full, path, src := Pixmap.Gray);
    END;
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {210,10});
      Path.LineTo (path, Point.T {230,20});
      Path.LineTo (path, Point.T {250,10});
      Path.LineTo (path, Point.T {240,30});
      Path.LineTo (path, Point.T {250,50});
      Path.LineTo (path, Point.T {230,40});
      Path.LineTo (path, Point.T {210,50});
      Path.LineTo (path, Point.T {220,30});
      Path.Close (path);
      VBT.Fill (self, Rect.Full, path, src := Pixmap.Empty);
    END;
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test13.
