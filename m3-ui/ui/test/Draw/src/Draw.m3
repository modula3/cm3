(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 14:01:26 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:40:08 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:54:46 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE Draw EXPORTS Main;

IMPORT VBT, Trestle, Point, Rect, Path, ButtonVBT, PaintOp,
  Region, HVSplit, TextVBT, Axis, TrestleComm;

FROM VBT IMPORT ClickType;

<*FATAL Path.Malformed*>

TYPE DrawVBT = VBT.Leaf OBJECT
    path: Path.T;
    drawing := FALSE;
    p, q: Point.T;
  OVERRIDES
    repaint := Repaint;
    reshape := Reshape;
    rescreen := Rescreen;
    mouse := Mouse;
    position := Position
  END;

PROCEDURE Repaint(v: DrawVBT; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintRegion(v, rgn, PaintOp.Bg);
    VBT.Stroke(v, rgn.r, v.path, op := PaintOp.Fg);
  END Repaint;

PROCEDURE Reshape(v: DrawVBT; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    InnerReshape(v, Rect.Middle(cd.new), Rect.Middle(cd.prev))
  END Reshape;
  
PROCEDURE InnerReshape(v: DrawVBT; READONLY new, old: Point.T) =
  BEGIN
    v.path := Path.Translate(v.path, Point.Sub(new, old));
    v.drawing := FALSE;
    Repaint(v, Region.Full)
  END InnerReshape;

PROCEDURE Rescreen(v: DrawVBT; READONLY cd: VBT.RescreenRec) =
  BEGIN
    InnerReshape(v, Point.Origin, Rect.Middle(cd.prev))
  END Rescreen;

PROCEDURE XorPQ(v: DrawVBT) =
  BEGIN VBT.Line(v, Rect.Full, v.p, v.q, op := PaintOp.Swap) END XorPQ;

PROCEDURE Mouse(v: DrawVBT; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = ClickType.FirstDown THEN
      v.drawing := TRUE;
      v.p := cd.cp.pt;
      v.q := v.p;
      XorPQ(v);
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE))
    ELSIF v.drawing AND cd.clickType = ClickType.LastUp THEN
      Path.MoveTo(v.path, v.p);
      Path.LineTo(v.path, v.q);
      VBT.Line(v, Rect.Full, v.p, v.q);
      v.drawing := FALSE
    ELSIF v.drawing THEN
      XorPQ(v);
      v.drawing := FALSE
    END
  END Mouse;

PROCEDURE Position(v: DrawVBT; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF NOT v.drawing THEN RETURN END;
    VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    IF NOT cd.cp.offScreen THEN
      XorPQ(v);
      v.q := cd.cp.pt;
      XorPQ(v);
    END
  END Position;

PROCEDURE DoErase(<*UNUSED*>b: ButtonVBT.T; 
  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    Path.Reset(drawVBT.path);
    drawVBT.drawing := FALSE;
    Repaint(drawVBT, Region.Full)
  END DoErase;

PROCEDURE DoExit(<*UNUSED*>b: ButtonVBT.T; 
  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END DoExit;

VAR 
  drawVBT := NEW(DrawVBT, path := NEW(Path.T));
  menuBar := ButtonVBT.MenuBar(
    ButtonVBT.New(TextVBT.New("Erase"), DoErase),
    ButtonVBT.New(TextVBT.New("Exit"), DoExit));
  main := HVSplit.Cons(Axis.T.Ver, menuBar, drawVBT, adjustable := FALSE);
  <*FATAL TrestleComm.Failure*>
BEGIN 
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Draw.
