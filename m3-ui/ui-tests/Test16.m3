(* Test16 paints a window with a red background *)

MODULE Test16 EXPORTS Main;

IMPORT PaintOp, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.FromRGB(1.0,0.0,0.0));
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test16.
