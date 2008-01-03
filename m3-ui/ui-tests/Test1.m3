(* This program tries out the four basic tints (Bg, Fg, Transparent, Swap) *)

MODULE Test1 EXPORTS Main;

IMPORT PaintOp, Rect, Region, Trestle, VBT;

PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full,              PaintOp.Bg);
    VBT.PaintTint (self, Rect.T{30, 70, 30, 70}, PaintOp.Fg);
    VBT.PaintTint (self, Rect.T{10, 50, 10, 50}, PaintOp.Transparent);
    VBT.PaintTint (self, Rect.T{50, 90, 50, 90}, PaintOp.Swap);
  END Repaint;

BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test1.
