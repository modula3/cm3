MODULE Rubber EXPORTS Main;
IMPORT Trestle, VBT, RigidVBT, BorderedVBT, Point, Rect, Path, Region,
       PaintOp;
FROM VBT IMPORT ClickType;
TYPE
  T = VBT.Leaf OBJECT
        nw, se : Point.T;
        path   : Path.T;
        banding            := FALSE;
      OVERRIDES
        repaint  := Repaint;
        mouse    := Mouse;
        position := Position;
      END;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintRegion(v, rgn, PaintOp.Bg);
  END Repaint;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF (cd.clickType = ClickType.FirstDown) THEN
      v.banding := TRUE;
      v.nw := cd.cp.pt;
      Path.Reset(v.path);
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp));
    ELSIF (v.banding AND cd.clickType = ClickType.LastUp) THEN
      v.banding := FALSE;
      v.se := cd.cp.pt;
    ELSIF (v.banding) THEN       (* Chord Cancel *)
      v.banding := FALSE;
    END;
  END Mouse;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF NOT v.banding THEN
      VBT.SetCage(v, VBT.EverywhereCage);
    ELSIF cd.cp.gone THEN
      VBT.SetCage(v, VBT.GoneCage)
    ELSE
      VBT.Stroke(v, Rect.Full, v.path, op := PaintOp.Swap); (* Erase old *)
      Path.Reset(v.path);
      Path.MoveTo(v.path, v.nw);
      Path.LineTo(v.path, Point.T{v.nw.h, cd.cp.pt.v});
      Path.LineTo(v.path, cd.cp.pt);
      Path.LineTo(v.path, Point.T{cd.cp.pt.h, v.nw.v});
      Path.Close(v.path);
      VBT.Stroke(v, Rect.Full, v.path, op := PaintOp.Swap); (* Draw new *)
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp));
    END;
  END Position;

VAR
  leaf := NEW(T, path := NEW(Path.T));
  main := BorderedVBT.New(RigidVBT.FromHV(leaf, 35.0, 35.0));
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Rubber.
