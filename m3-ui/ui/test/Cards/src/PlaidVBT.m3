(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Apr 30 15:48:48 PDT 1993 by mjordan  *)
(*      modified on Sat Mar 21 09:24:26 PST 1992 by msm      *)
(*      modified on Mon Feb 24 14:01:22 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:10:55 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE PlaidVBT;

IMPORT Axis, PaintOp, Pixmap, Point, Rect, Region, Thread, VBT, Scheduler;

REVEAL T = VBT.Leaf BRANDED OBJECT
    startOver := TRUE;
    running := FALSE;
    shouldRun := FALSE
  OVERRIDES
    reshape := Reshape;
    mouse := Mouse;
    repaint := Repaint
  END;

VAR
  mu := NEW(MUTEX);

TYPE PlaidClosure = Thread.SizedClosure OBJECT v: T OVERRIDES apply := Animate END;

PROCEDURE Animate(cl: PlaidClosure): REFANY RAISES {} =
  VAR
    v := cl.v;
    oddCycle:            BOOLEAN;
    dom, prevRect, rect: Rect.T;
    deltaP, mid, p:      Point.T;
  BEGIN
    LOOP
      LOCK mu DO
        dom := VBT.Domain(v);
        IF NOT v.shouldRun OR Rect.IsEmpty(dom) THEN
          v.running := FALSE;
          EXIT
        ELSIF v.startOver THEN
          v.startOver := FALSE;
          p := Rect.Middle(dom);
          prevRect := Rect.Empty;
          oddCycle := FALSE;
          VBT.PaintTint(v, dom, PaintOp.Bg);
          deltaP := Point.FromCoords(1, 1)
        END
      END;

      p := Point.Add(p, deltaP);
      LOOP
        IF p.h < dom.west THEN
          p.h := 2 * dom.west - p.h;
          deltaP.h := -deltaP.h
        ELSIF p.h >= dom.east THEN
          p.h := 2 * dom.east - p.h - 2;
          deltaP.h := -deltaP.h
        ELSIF p.v < dom.north THEN
          p.v := 2 * dom.north - p.v;
          deltaP.v := -deltaP.v
        ELSIF p.v >= dom.south THEN
          p.v := 2 * dom.south - p.v - 2;
          deltaP.v := -deltaP.v
        ELSE
          EXIT
        END
      END;
      mid  := Rect.Middle(dom);
      rect := Rect.FromSize(2 * ABS(p.h - mid.h), 2 * ABS(p.v - mid.v));
      rect := Rect.Center(rect, mid);
      IF oddCycle THEN
        VBT.PaintRegion(
          v, Region.SymmetricDifference(
               Region.FromRect(rect), Region.FromRect(prevRect)),
          PaintOp.Swap, Pixmap.Solid, Point.Origin);
	Scheduler.Yield();
	VBT.Sync(v)
      END;
      oddCycle := NOT oddCycle;
      prevRect := rect
    END;
    RETURN NIL
  END Animate;
  
PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown 
       AND cd.whatChanged # VBT.Modifier.MouseL THEN
      LOCK mu DO
        v.startOver := TRUE;
        v.shouldRun := NOT v.shouldRun;
        IF v.shouldRun AND NOT v.running THEN
          v.running := TRUE;
          EVAL Thread.Fork(NEW(PlaidClosure, v := v, stackSize := 20000))
        END
      END
    END
  END Mouse;
  
PROCEDURE Repaint(v: T; <*UNUSED*>READONLY rgn: Region.T) RAISES {} =
  VAR
    dom:  Rect.T;
  BEGIN
    LOCK mu DO
      v.startOver := TRUE;
      IF NOT v.running THEN
        VBT.PaintTint(v, VBT.Domain(v), PaintOp.Bg);
          (* for debugging help, paint nested rectangles *)
        dom := VBT.Domain(v);
        FOR i := 1 TO MIN(Rect.HorSize(dom), Rect.VerSize(dom)) DIV 2 BY 2 DO
          VBT.PaintTint(v, Rect.FromEdges(dom.west + i, dom.west + i + 1,
                                          dom.north + i, dom.south - i),
                        PaintOp.Fg);
          VBT.PaintTint(v, Rect.FromEdges(dom.east - i - 1, dom.east - i,
                                          dom.north + i, dom.south - i),
                        PaintOp.Fg);
          VBT.PaintTint(v, Rect.FromEdges(dom.west + i, dom.east - i,
                                          dom.north + i, dom.north + i + 1),
                        PaintOp.Fg);
          VBT.PaintTint(v, Rect.FromEdges(dom.west + i, dom.east - i,
                                          dom.south - i - 1, dom.south - i),
                        PaintOp.Fg)
        END
      END
    END
  END Repaint;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    LOCK mu DO
      v.startOver := TRUE;
      IF NOT Rect.IsEmpty(cd.new) AND v.shouldRun AND NOT v.running
      THEN
        v.running := TRUE;
        EVAL Thread.Fork(NEW(PlaidClosure, v := v, stackSize := 20000))
      ELSE
        VBT.PaintTint(v, VBT.Domain(v), PaintOp.Bg)
      END
    END
  END Reshape;
  
BEGIN END PlaidVBT.
