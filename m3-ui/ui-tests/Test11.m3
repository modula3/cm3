MODULE Test11 EXPORTS Main;

IMPORT PaintOp, Pixmap, Rect, Region, Trestle, TrestleComm, VBT;

<* FATAL TrestleComm.Failure *>

PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint    (self, Rect.Full,                op  := PaintOp.Bg);
    VBT.PaintTexture (self, Rect.T{20, 100, 20, 100}, src := Pixmap.Gray);
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test11.
