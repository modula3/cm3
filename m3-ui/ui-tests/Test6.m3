MODULE Test6 EXPORTS Main;

IMPORT PaintOp, Path, Point, Rect, Region, Trestle, VBT;

CONST w = 10;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  VAR
    path := NEW (Path.T);
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, pt := Point.T {20, 20}, 
                   t := "Trestle EndStyles and JoinStyles for stroking paths");

    VBT.PaintText (self, pt := Point.T {10, 105}, t := "end = Round");
    VBT.PaintText (self, pt := Point.T {10, 120}, t := "join = Round");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {10,90});
      Path.LineTo (path, Point.T {10,50});
      Path.LineTo (path, Point.T {30,90});
      Path.MoveTo (path, Point.T {30,90});
      Path.LineTo (path, Point.T {50,50});
      Path.LineTo (path, Point.T {50,90});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Round, join := VBT.JoinStyle.Round);
    END;

    VBT.PaintText (self, pt := Point.T {110, 105}, t := "end = Butt");
    VBT.PaintText (self, pt := Point.T {110, 120}, t := "join = Round");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {110,90});
      Path.LineTo (path, Point.T {110,50});
      Path.LineTo (path, Point.T {130,90});
      Path.MoveTo (path, Point.T {130,90});
      Path.LineTo (path, Point.T {150,50});
      Path.LineTo (path, Point.T {150,90});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Butt, join := VBT.JoinStyle.Round);
    END;

    VBT.PaintText (self, pt := Point.T {210, 105}, t := "end = Square");
    VBT.PaintText (self, pt := Point.T {210, 120}, t := "join = Round");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {210,90});
      Path.LineTo (path, Point.T {210,50});
      Path.LineTo (path, Point.T {230,90});
      Path.MoveTo (path, Point.T {230,90});
      Path.LineTo (path, Point.T {250,50});
      Path.LineTo (path, Point.T {250,90});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Square, join := VBT.JoinStyle.Round);
    END;

    VBT.PaintText (self, pt := Point.T {10, 205}, t := "end = Round");
    VBT.PaintText (self, pt := Point.T {10, 220}, t := "join = Bevel");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {10,190});
      Path.LineTo (path, Point.T {10,150});
      Path.LineTo (path, Point.T {30,190});
      Path.MoveTo (path, Point.T {30,190});
      Path.LineTo (path, Point.T {50,150});
      Path.LineTo (path, Point.T {50,190});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Round, join := VBT.JoinStyle.Bevel);
    END;

    VBT.PaintText (self, pt := Point.T {110, 205}, t := "end = Butt");
    VBT.PaintText (self, pt := Point.T {110, 220}, t := "join = Bevel");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {110,190});
      Path.LineTo (path, Point.T {110,150});
      Path.LineTo (path, Point.T {130,190});
      Path.MoveTo (path, Point.T {130,190});
      Path.LineTo (path, Point.T {150,150});
      Path.LineTo (path, Point.T {150,190});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Butt, join := VBT.JoinStyle.Bevel);
    END;

    VBT.PaintText (self, pt := Point.T {210, 205}, t := "end = Square");
    VBT.PaintText (self, pt := Point.T {210, 220}, t := "join = Bevel");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {210,190});
      Path.LineTo (path, Point.T {210,150});
      Path.LineTo (path, Point.T {230,190});
      Path.MoveTo (path, Point.T {230,190});
      Path.LineTo (path, Point.T {250,150});
      Path.LineTo (path, Point.T {250,190});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Square, join := VBT.JoinStyle.Bevel);
    END;

    VBT.PaintText (self, pt := Point.T {10, 305}, t := "end = Round");
    VBT.PaintText (self, pt := Point.T {10, 320}, t := "join = Miter");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {10,290});
      Path.LineTo (path, Point.T {10,250});
      Path.LineTo (path, Point.T {30,290});
      Path.MoveTo (path, Point.T {30,290});
      Path.LineTo (path, Point.T {50,250});
      Path.LineTo (path, Point.T {50,290});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Round, join := VBT.JoinStyle.Miter);
    END;

    VBT.PaintText (self, pt := Point.T {110, 305}, t := "end = Butt");
    VBT.PaintText (self, pt := Point.T {110, 320}, t := "join = Miter");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {110,290});
      Path.LineTo (path, Point.T {110,250});
      Path.LineTo (path, Point.T {130,290});
      Path.MoveTo (path, Point.T {130,290});
      Path.LineTo (path, Point.T {150,250});
      Path.LineTo (path, Point.T {150,290});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Butt, join := VBT.JoinStyle.Miter);
    END;

    VBT.PaintText (self, pt := Point.T {210, 305}, t := "end = Square");
    VBT.PaintText (self, pt := Point.T {210, 320}, t := "join = Miter");
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Point.T {210,290});
      Path.LineTo (path, Point.T {210,250});
      Path.LineTo (path, Point.T {230,290});
      Path.MoveTo (path, Point.T {230,290});
      Path.LineTo (path, Point.T {250,250});
      Path.LineTo (path, Point.T {250,290});
      VBT.Stroke (self, Rect.Full, path, width := w, 
                  end := VBT.EndStyle.Square, join := VBT.JoinStyle.Miter);
    END;
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test6.
