(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:54:56 PST 1992 by muller   *)
(*      modified on Sun Nov 10 18:21:52 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:46:44 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE TextureVBT;

IMPORT VBT, PaintOp, Pixmap, Region, Rect, Point;

REVEAL T = Public BRANDED OBJECT
    op: PaintOp.T;
    txt: Pixmap.T;
    nwAlign: BOOLEAN
  OVERRIDES
    repaint := Repaint;
    reshape := Reshape;
    redisplay := Redisplay;
    init := Be
  END;

PROCEDURE Be(
  v: T; 
  op: PaintOp.T := PaintOp.BgFg; 
  txt: Pixmap.T := Pixmap.Solid; 
  nwAlign: BOOLEAN := FALSE): T =
  BEGIN
    v.op := op;
    v.txt := txt;
    v.nwAlign := nwAlign;
    RETURN v
  END Be;

PROCEDURE New(
  op: PaintOp.T  := PaintOp.BgFg; 
  txt: Pixmap.T := Pixmap.Solid; 
  nwAlign: BOOLEAN := FALSE): T RAISES {} =
  BEGIN
    RETURN Be(NEW(T), op, txt, nwAlign);
  END New;

PROCEDURE Set(
  v: T;   
  op: PaintOp.T := PaintOp.BgFg; 
  txt: Pixmap.T := Pixmap.Solid;
  nwAlign: BOOLEAN := FALSE)
  RAISES {} =
  BEGIN v.op := op; v.txt := txt; v.nwAlign := nwAlign; VBT.Mark(v) END Set;

PROCEDURE Get(
  v: T;   
  VAR op: PaintOp.T; 
  VAR txt: Pixmap.T;
  VAR nwAlign: BOOLEAN) =
  BEGIN op := v.op; txt := v.txt; nwAlign := v.nwAlign END Get;

PROCEDURE Repaint(v: T; READONLY rgn: Region.T) =
  BEGIN
    IF v.nwAlign THEN
      VBT.PaintTexture(v, rgn.r, v.op, v.txt, Rect.NorthWest(VBT.Domain(v)))
    ELSE
      VBT.PaintTexture(v, rgn.r, v.op, v.txt, Point.Origin)
    END
  END Repaint;

PROCEDURE Reshape(v: T; <*UNUSED*> READONLY cd: VBT.ReshapeRec) =
  BEGIN Repaint(v, Region.Full) END Reshape;

PROCEDURE Redisplay(v: T) =
  BEGIN Repaint(v, Region.Full) END Redisplay;

BEGIN END TextureVBT.
