(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Jun  2 11:19:06 PDT 1995 by najork                   *)
(*       Created on Fri Jun  2 11:06:29 PDT 1995 by najork                   *)


MODULE Test28 EXPORTS Main;

IMPORT Image, Palette, PaintOp, Path, Pixmap, Point, Rd, Rect, Region, Rsrc, 
       ScreenType, TestBundle, Trestle, VBT;

<* FATAL ANY *>


PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY bad: Region.T) =
  CONST
    rect = Rect.T {75, 200, 10, 76};
    org1 = Point.T {20, 40};
  BEGIN
    VBT.PaintTint (self, Rect.Full, PaintOp.Bg);
    VBT.PaintText (self, pt := Point.T {20, 20}, 
                   t := "This program tests ScrnPixmap.T.localize");

    WITH st  = VBT.ScreenTypeOf (self),
         spm = Palette.ResolvePixmap (st, pm),
         lpm = st.pixmap.load (spm.localize (rect)) DO

      VBT.PaintScrnPixmap (self, src := spm, delta := org1);

      WITH r    = Rect.Add (rect, org1),
           path = NEW (Path.T) DO
        Path.MoveTo (path, Rect.NorthWest (r));
        Path.LineTo (path, Rect.NorthEast (r));
        Path.LineTo (path, Rect.SouthEast (r));
        Path.LineTo (path, Rect.SouthWest (r));
        Path.Close (path);
        VBT.Stroke (self, Rect.Full, path);
      END;

      WITH delta = Point.T {20 + rect.west - lpm.bounds.west, 140} DO
        VBT.PaintScrnPixmap (self, src := lpm, delta := delta);
      END;
    END;

  END Repaint;


VAR pm: Pixmap.T;

BEGIN
  WITH path = Rsrc.BuildPath(TestBundle.Get()),
       rd = Rsrc.Open("Juno2Logo.ppm", path) DO
    pm := Image.Unscaled (Image.FromRd(rd));
    Rd.Close(rd);
  END;

  WITH v = NEW (VBT.Leaf, repaint := Repaint) DO
    Trestle.Install (v);
    Trestle.AwaitDelete(v);
  END;
END Test28.
