MODULE Test12 EXPORTS Main;

IMPORT PaintOp, Pixmap, Point, Rect, Region, ScrnPixmap, Trestle, TrestleComm,
       VBT;

<* FATAL TrestleComm.Failure *>

TYPE
  Pat = ARRAY [0..5] OF Row;
  Row = ARRAY [0..5] OF INTEGER;


CONST 
  pat = Pat {Row{0,0,0,0,0,0},
             Row{0,1,0,0,0,0},
             Row{0,0,1,0,0,0},
             Row{0,0,0,1,0,1},
             Row{0,0,0,0,1,1},
             Row{0,0,0,1,1,1}};

VAR
  pm := InstallPixmap ();


PROCEDURE InstallPixmap (): Pixmap.T =
  BEGIN
    WITH rect = Rect.T {1,7,1,7},
         raw  = ScrnPixmap.NewRaw (1, rect) DO 
      FOR h := rect.west TO rect.east - 1 DO
        FOR v := rect.north TO rect.south - 1 DO
          raw.set (Point.T {h, v}, pat[v - rect.north][h - rect.west]);
        END;
      END;
      RETURN Pixmap.FromBitmap (raw);
    END;
  END InstallPixmap;


PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint    (self, Rect.Full,                op  := PaintOp.Bg);
    VBT.PaintTexture (self, Rect.T{20, 100, 20, 100}, src := pm);
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO 
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END
END Test12.
