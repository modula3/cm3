MODULE Test8 EXPORTS Main;

IMPORT PaintOp, Path, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =

  PROCEDURE Sub (y: INTEGER; op: PaintOp.T; t: TEXT) =
    BEGIN
      WITH path = NEW (Path.T) DO
        Path.MoveTo (path, Point.T{mid - 40, y});
        Path.LineTo (path, Point.T{mid + 40, y});
        VBT.Stroke (self, Rect.Full, path, width := 5, op := op);
        VBT.PaintText (self, pt := Point.T {mid + 50, y}, t := t);
      END;
    END Sub;

  VAR
    dom  := VBT.Domain(self);
    mid := dom.west + dom.east DIV 2;
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintTint (self, 
                   Rect.T{mid - 20, mid + 20, dom.north, dom.south},
                   PaintOp.Fg);

    Sub (20, PaintOp.BgBg, "BgBg");
    Sub (40, PaintOp.BgFg, "BgFg");
    Sub (60, PaintOp.BgTransparent, "BgTransparent");
    Sub (80, PaintOp.BgSwap, "BgSwap");

    Sub (120, PaintOp.FgFg, "FgFg");
    Sub (140, PaintOp.FgBg, "FgBg");
    Sub (160, PaintOp.FgTransparent, "FgTransparent");
    Sub (180, PaintOp.FgSwap, "FgSwap");

    Sub (220, PaintOp.TransparentTransparent, "TransparentTransparent");
    Sub (240, PaintOp.TransparentBg, "TransparentBg");
    Sub (260, PaintOp.TransparentFg, "TransparentFg");
    Sub (280, PaintOp.TransparentSwap, "TransparentSwap");

    Sub (320, PaintOp.SwapSwap, "SwapSwap");
    Sub (340, PaintOp.SwapBg, "SwapBg");
    Sub (360, PaintOp.SwapFg, "SwapFg");
    Sub (380, PaintOp.SwapTransparent, "SwapTransparent");
  
    Sub (420, PaintOp.Copy, "Copy");
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test8.
