MODULE Test26 EXPORTS Main;

IMPORT Axis, PaintOp, Rect, Region, Thread, Trestle, VBT;

VAR size := 100;

PROCEDURE Shape (<*UNUSED*> ch: VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := size, pref := size, hi := size + 1}
  END Shape;


PROCEDURE Repaint (v: VBT.Leaf; <*UNUSED*> READONLY bad: Region.T) =
  BEGIN
    VBT.PaintTint (v, Rect.Full, PaintOp.Bg);
  END Repaint;

VAR
  v := NEW (VBT.Leaf, repaint := Repaint, shape := Shape);
BEGIN
  Trestle.Install (v);
  LOOP
    FOR i := 100 TO 200 DO
      LOCK VBT.mu DO
        size := i;
        VBT.NewShape(v);
        Thread.Pause (0.1d0);
      END;
    END;
    FOR i := 200 TO 100 BY - 1 DO
      LOCK VBT.mu DO
        size := i;
        VBT.NewShape(v);
        Thread.Pause (0.1d0);
      END;
    END;
  END;
END Test26.
