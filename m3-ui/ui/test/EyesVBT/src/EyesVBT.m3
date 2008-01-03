(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Wed May 19 12:18:11 PDT 1993 by msm      *)
(*      modified on Mon Feb 24 14:01:25 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:09:42 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE EyesVBT;

IMPORT Axis, PaintOp, Pixmap, Point, Rect, Region, VBT, Palette,
  ScrnPixmap, ScreenType, TrestleComm;

REVEAL T = VBT.Leaf BRANDED OBJECT
    eyebox := Rect.Empty;
    eyeSet := FALSE;
    pupil := Point.Origin;
  OVERRIDES
    position := Position;
    reshape := Reshape;
    repaint := Repaint;
    shape := Shape;
  END;

CONST 
  EyeRadius = 10.5; Border = 2; MinSize = 2 * (Border + CEILING(EyeRadius));

PROCEDURE NewPixmap(): Pixmap.T =
  BEGIN
    RETURN Palette.FromPixmapClosure(NEW(MyPixmapClosure))
  END NewPixmap;

TYPE MyPixmapClosure = Palette.PixmapClosure 
  OBJECT OVERRIDES apply := MyPixmapApply END;

PROCEDURE MyPixmapApply(<*UNUSED*>cl: MyPixmapClosure; st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    TRY
      RETURN st.pixmap.load(Circle(EyeRadius))
    EXCEPT
      TrestleComm.Failure => 
        RETURN Palette.ResolvePixmap(st, Pixmap.Empty)
    END
  END MyPixmapApply;

PROCEDURE Circle(r: REAL): ScrnPixmap.Raw =
  VAR rf := FLOOR(-r); rc := FLOOR(r)+1;
   res := ScrnPixmap.NewRaw(1, Rect.FromEdges(rf, rc, rf, rc)); 
   r2 := FLOOR(r*r); h2: INTEGER;
  BEGIN
    FOR h := rf TO rc-1 DO
      h2 := h*h;
      FOR v := rf TO rc-1 DO
        IF h2 + v*v <= r2 THEN
          res.set(Point.T{h,v}, 1)
        ELSE
          res.set(Point.T{h,v}, 0)
        END
      END
    END;
    RETURN res
  END Circle;

PROCEDURE Shape(<*UNUSED*>v: T; <*UNUSED*>ax: Axis.T; 
  <*UNUSED*>n: CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := MinSize, pref := MinSize, hi := 999999}
  END Shape;

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  VAR dom := Rect.Inset(VBT.Domain(v), FLOOR(EyeRadius) + Border); 
    newbox: Rect.T; a: Rect.Partition; BEGIN
    IF Rect.IsEmpty(dom) THEN RETURN END;
    IF NOT cd.cp.offScreen THEN
      WITH focus = Rect.Project(dom, cd.cp.pt) DO
        IF NOT v.eyeSet OR NOT Point.Equal(v.pupil, focus) THEN
          newbox := Rect.Add(VBT.PixmapDomain(v, eyeball), focus);
	  v.pupil := focus;
	  v.eyeSet := TRUE;
	  Rect.Factor(v.eyebox, newbox, a, 0, 0);
	  a[2] := a[4];
          VBT.PolyTint(v, SUBARRAY(a, 0, 4), PaintOp.Bg);
          VBT.PaintPixmap(v, newbox, PaintOp.BgFg, eyeball, focus);
          v.eyebox := newbox
	END
      END
    END;
    VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE, FALSE))
  END Position;
  
PROCEDURE Repaint(v: T; READONLY rgn: Region.T) RAISES {} =
  BEGIN
    VBT.PaintRegion(v, rgn, PaintOp.Bg);
    VBT.PaintPixmap(v, Rect.Meet(rgn.r, v.eyebox), PaintOp.BgFg, 
      eyeball, v.pupil)
  END Repaint;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    v.eyebox := Rect.Empty;
    v.eyeSet := FALSE;
    IF Rect.IsEmpty(cd.new) THEN
      VBT.SetCage(v, VBT.EverywhereCage)
    ELSE
      VBT.SetCage(v, VBT.Cage{Rect.Empty, VBT.InOut{}, VBT.AllScreens});
      VBT.PaintTint(v, cd.new, PaintOp.Bg)
    END
  END Reshape;
  
VAR eyeball := NewPixmap();

BEGIN END EyesVBT.
