MODULE Main;

IMPORT PaintOp, Pixmap, Point, Rect, Region, ScrnPixmap, Trestle, TrestleComm,
       VBT;

<* FATAL TrestleComm.Failure *>

PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint    (self, Rect.Full,                op  := PaintOp.Bg);
    VBT.PaintTexture (self, Rect.T{20, 100, 20, 100}, src := src);
  END Repaint;

VAR src : Pixmap.T;

CONST n = 8;

BEGIN
  WITH raw = ScrnPixmap.NewRaw (1, Rect.T{0,n,0,n}) DO
    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO
        raw.set (Point.T{i, j}, (i+j) MOD 2);
      END;
    END;
    src := Pixmap.FromBitmap (raw);
  END;

  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Main.
