MODULE Main;

IMPORT Math, PaintOp, Pixmap, Point, Rect, Region, ScrnPixmap, Trestle, 
       TrestleComm, VBT;

<* FATAL TrestleComm.Failure *>

PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint    (self, Rect.Full,                op  := PaintOp.Bg);
    VBT.PaintTexture (self, Rect.T{20, 220, 20, 220}, src := src);
  END Repaint;


VAR src := Bullseye();


PROCEDURE Bullseye (): Pixmap.T =
  CONST rect = Rect.T{-15,16,-15,16};
  BEGIN
    WITH raw = ScrnPixmap.NewRaw (1, rect) DO
      FOR x := rect.west TO rect.east - 1 DO
        FOR y := rect.north TO rect.south - 1 DO
          raw.set (Point.T {x, y}, 0);
        END;
      END;
      FOR i := 0 TO 200 DO
        WITH ang = (FLOAT (i, LONGREAL) / 100.0d0) * FLOAT (Math.Pi, LONGREAL),
             sin = FLOAT (Math.sin (ang)),
             cos = FLOAT (Math.cos (ang)) DO
          FOR j := 0 TO 15 BY 3 DO
            WITH x = ROUND (FLOAT(j) * sin),
                 y = ROUND (FLOAT(j) * cos) DO
              raw.set (Point.T {x, y}, 1);
            END;
          END;
        END;
      END;
      RETURN Pixmap.FromBitmap (raw);
    END;
  END Bullseye;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Main.
