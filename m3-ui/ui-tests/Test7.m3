MODULE Test7 EXPORTS Main;

IMPORT PaintOp, Point, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  BEGIN
    WITH full  = VBT.Domain(self),
         hmid  = full.west + full.east DIV 2,
         vmid  = full.north + full.south DIV 2,
         left  = Rect.T{full.west, hmid, full.north, full.south},
         right = Rect.T{hmid + 1, full.east, full.north, full.south} DO
      VBT.PaintTint (self, left,  PaintOp.Fg);
      VBT.PaintTint (self, right, PaintOp.Bg);

      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 20},
                     t  := "Testing PaintText with PaintOp.BgBg",
                     op := PaintOp.BgBg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 40},
                     t  := "Testing PaintText with PaintOp.BgFg",
                     op := PaintOp.BgFg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 60},
                     t  := "Testing PaintText with PaintOp.BgTransparent",
                     op := PaintOp.BgTransparent);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 80},
                     t  := "Testing PaintText with PaintOp.BgSwap",
                     op := PaintOp.BgSwap);

      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 120},
                     t  := "Testing PaintText with PaintOp.FgFg",
                     op := PaintOp.FgFg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 140},
                     t  := "Testing PaintText with PaintOp.FgBg",
                     op := PaintOp.FgBg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 160},
                     t  := "Testing PaintText with PaintOp.FgTransparent",
                     op := PaintOp.FgTransparent);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 180},
                     t  := "Testing PaintText with PaintOp.FgSwap",
                     op := PaintOp.FgSwap);

      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 220},
                     t  := "Testing PaintText with PaintOp.TransparentTransparent",
                     op := PaintOp.TransparentTransparent);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 240},
                     t  := "Testing PaintText with PaintOp.TransparentBg",
                     op := PaintOp.TransparentBg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 260},
                     t  := "Testing PaintText with PaintOp.TransparentFg",
                     op := PaintOp.TransparentFg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 280},
                     t  := "Testing PaintText with PaintOp.TransparentSwap",
                     op := PaintOp.TransparentSwap);

      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 320},
                     t  := "Testing PaintText with PaintOp.SwapSwap",
                     op := PaintOp.SwapSwap);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 340},
                     t  := "Testing PaintText with PaintOp.SwapBg",
                     op := PaintOp.SwapBg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 360},
                     t  := "Testing PaintText with PaintOp.SwapFg",
                     op := PaintOp.SwapFg);
      VBT.PaintText (self,
                     pt := Point.T {hmid - 100, 380},
                     t  := "Testing PaintText with PaintOp.SwapTransparent",
                     op := PaintOp.SwapTransparent);
  
      VBT.PaintText (self, 
                     pt := Point.T {hmid - 100, 420}, 
                     t  := "Testing PaintText with PaintOp.Copy",
                     op := PaintOp.Copy);
    END;
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test7.
