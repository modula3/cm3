MODULE Test10 EXPORTS Main;

IMPORT PaintOp, Pixmap, Point, Rect, Region, Trapezoid, Trestle, VBT;


PROCEDURE Repaint (self: VBT.Leaf; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintTrapezoid (self, Rect.Full, 
                        Trapezoid.T {vlo := 10, 
                                     vhi := 50, 
                                     m1 := Trapezoid.Rational {-4, 1},
                                     m2 := Trapezoid.Rational { 4, 1},
                                     p1 := Point.T {20, 50}, 
                                     p2 := Point.T {80, 50}});
    VBT.PaintTrapezoid (self, Rect.Full, 
                        Trapezoid.T {vlo := 10, 
                                     vhi := 50, 
                                     m1 := Trapezoid.Rational {-4, 1},
                                     m2 := Trapezoid.Rational { 4, 1},
                                     p1 := Point.T {120, 50}, 
                                     p2 := Point.T {180, 50}},
                        src := Pixmap.Gray);
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test10.
