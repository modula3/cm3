MODULE Test5 EXPORTS Main;

IMPORT PaintOp, Path, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  VAR
    path := NEW (Path.T);
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, pt := Point.T {20, 20}, t := "A little test program");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {10,50});
      Path.LineTo (path, Point.T {30,60});
      Path.LineTo (path, Point.T {50,50});
      Path.LineTo (path, Point.T {40,70});
      Path.LineTo (path, Point.T {50,90});
      Path.LineTo (path, Point.T {30,80});
      Path.LineTo (path, Point.T {10,90});
      Path.LineTo (path, Point.T {20,70});
      Path.Close (path);
      VBT.Fill (self, Rect.Full, path);
    END;
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {110,50});
      Path.LineTo (path, Point.T {130,60});
      Path.LineTo (path, Point.T {150,50});
      Path.LineTo (path, Point.T {140,70});
      Path.LineTo (path, Point.T {150,90});
      Path.LineTo (path, Point.T {130,80});
      Path.LineTo (path, Point.T {110,90});
      Path.LineTo (path, Point.T {120,70});
      Path.Close (path);
      VBT.Stroke (self, Rect.Full, path, width := 1);
    END;
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {210,50});
      Path.LineTo (path, Point.T {230,60});
      Path.LineTo (path, Point.T {250,50});
      Path.LineTo (path, Point.T {240,70});
      Path.LineTo (path, Point.T {250,90});
      Path.LineTo (path, Point.T {230,80});
      Path.LineTo (path, Point.T {210,90});
      Path.LineTo (path, Point.T {220,70});
      Path.Close (path);
      VBT.Stroke (self, Rect.Full, path, width := 5, 
                  join := VBT.JoinStyle.Miter);
    END;
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test5.
