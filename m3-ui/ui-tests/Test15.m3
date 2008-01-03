MODULE Test15 EXPORTS Main;

IMPORT Font, PaintOp, Path, Point, Rect, Region, Trestle, VBT;

CONST 
  fname = "-Windows-Courier-Normal-R-Unknown-*-0-20-0-0-M-12-iso8859-ANSI";

VAR 
  font := Font.FromName (ARRAY OF TEXT {fname});

PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, 
                   pt := Point.T {20, 20}, 
                   t  := "Using the default font");
    VBT.PaintText (self, 
                   pt  := Point.T {20, 50}, 
                   t   := "Using 20 point Courier",
                   fnt := font);
    VBT.PaintText (self, 
                   pt := Point.T {20, 80}, 
                   t  := "Using the default font again");
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test15.
