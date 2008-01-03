(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun 22 10:25:34 PDT 1993 by steveg   *)
(*      modified on Tue Jul 21 16:31:35 PDT 1992 by harrison *)



(*      modified on Thu Jul  9 18:52:23 1992 by mhb      *)

MODULE MGV;

<* PRAGMA LL *>

IMPORT Animate, Axis, Filter, InstalledVBT, IntRefTbl, MG, PaintOp, Point,
       Pts, R2, R2Box, Rect, Region, ScrnPixmap, Thread, Trestle,
       TrestleComm, VBT, VBTClass;

REVEAL
  V = VPublic BRANDED OBJECT
      OVERRIDES
        rescreen        := RescreenV;
        reshape         := ReshapeSWOrigin;
        shape           := ShapeStretchy;
        repaint         := RepaintV;
        redisplay       := RedisplayV;
        mouse           := MouseV;
        init            := InitV;
        setNW           := SetNW;
        setDoubleBuffer := SetDoubleBuffer;
        setSelector     := SetSelector;
        setSelectee     := SetSelectee;
        mgRedisplay     := MGRedisplay;
      END;

PROCEDURE InitV(v: V): V =
  BEGIN
    v.mu := NEW(MUTEX);
    v.displayList := NEW(MG.Group).init();
    v.lookup := NEW(IntRefTbl.Default).init();
    v.paintVBT := v;
    IF v.selector = NIL THEN v.selector := selectorClosest END;
    IF v.selectee = NIL THEN v.selectee := selecteeDefault END;
    RETURN v;
  END InitV;

PROCEDURE RedisplayV(v: V) =
  BEGIN
    v.mgRedisplay(Region.Empty);
  END RedisplayV;

PROCEDURE SetSelector (v: V; selector: Selector) =
  BEGIN
    IF selector = NIL THEN selector := selectorClosest END;
    v.selector := selector;
  END SetSelector;

PROCEDURE SetSelectee (v: V; selectee: Selectee) =
  BEGIN
    IF selectee = NIL THEN selectee := selecteeDefault END;
    v.selectee := selectee;
  END SetSelectee;

PROCEDURE SetNW (v: V; nw := R2.Origin) =
  BEGIN
    v.nw := nw;
    v.displayList.dirty := TRUE;
    v.dirtyRegion := Rect.Full;
    VBT.Mark(v);
  END SetNW;

(* SCG July 28, 1992: Now there is always an offscreen VBT for the
   shapeVBT.  If doubleBuffering is on, then the paintVBT shares
   it.  Kinda backwards.... *)
PROCEDURE SetDoubleBuffer (v: V; yes: BOOLEAN) =
  VAR dom: Rect.T;
  BEGIN
    v.doubleBuffer := yes;
    IF v.paintVBT # v THEN
      Trestle.Delete(v.paintVBT);
      VBT.Discard(v.paintVBT);
      v.paintVBT := v;
    END;
    v.shapeVBT := NEW(VBT.Leaf);
    IF yes THEN v.paintVBT := v.shapeVBT; v.dirtyRegion := Rect.Full; END;
    dom := VBT.Domain(v);
    VAR
      tso := Trestle.ScreenOf(v, Point.Origin);
      trsl := tso.trsl;
      stInstall := VBT.ScreenTypeOf(InstalledVBT.Child(v));
      st   := VBT.ScreenTypeOf(v);
    <* FATAL TrestleComm.Failure *>
    BEGIN
      IF trsl # NIL AND st # NIL THEN
        WITH filter = NEW(Filter.T).init(v.shapeVBT) DO
          Trestle.Attach(filter, trsl);
          (* install a Filter above the shapeVBT so that it can have a
             ScreenType that Trestle likes in the case where shapeVBT has
             an "unusual" screen type (i.e.  there is a scale filter) *)
          Trestle.InstallOffscreen(
            filter, dom.east - dom.west, dom.south - dom.north, stInstall);
          IF filter.st # st THEN
            (* duke it out with trestle to set the screen type and
               domain *)
            VBTClass.Rescreen(v.shapeVBT, st);
            VBTClass.Reshape(v.shapeVBT, filter.domain, Rect.Empty);
          END;
        END;
      END;
    END;
  END SetDoubleBuffer;

PROCEDURE RepaintV (v: V; READONLY br: Region.T) =
  BEGIN
    VBT.PaintTint(v.paintVBT, br.r, PaintOp.Bg);
    v.mgRedisplay(br);
  END RepaintV;

<* LL <= VBT.mu *>
PROCEDURE MGRedisplay (v: V; READONLY br: Region.T) =
  VAR
    clip                    := Rect.Join(br.r, v.dirtyRegion);
    dom, dom2: Rect.T;
    brCap    : Region.T;
    pixmap   : ScrnPixmap.T;
  <* FATAL TrestleComm.Failure *>
  BEGIN
    IF Rect.IsEmpty(VBT.Domain(v)) THEN RETURN END;
    LOCK v.mu DO
      VBT.BeginGroup(v);
      VBT.PaintTint(v.paintVBT, v.dirtyRegion, PaintOp.Bg);
      v.displayList.render(v, clip);
      v.dirtyRegion := Rect.Empty;
      IF NOT v.doubleBuffer THEN VBT.EndGroup(v.paintVBT); RETURN END;
      dom := VBT.Domain(v);
      dom2 := VBT.Domain(v.paintVBT);
      IF NOT Rect.IsEmpty(dom) THEN
        pixmap := VBT.Capture(v.paintVBT, dom2, brCap);
        IF pixmap # NIL THEN
          VBT.PaintScrnPixmap(
            v, src := pixmap,
            delta := Point.Sub(Rect.NorthWest(dom), Rect.NorthWest(dom2)));
          VBT.EndGroup(v);
          VBT.Sync(v);
        END;
      END;
    END;
    IF pixmap # NIL THEN pixmap.free() END;
  END MGRedisplay;

PROCEDURE RescreenV (v: V; <* UNUSED *> READONLY cd: VBT.RescreenRec) =
  BEGIN
    LOCK v.mu DO v.dirtyRegion := Rect.Full; END;
  END RescreenV;

PROCEDURE ReshapeSWOrigin (v: V; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    LOCK v.mu DO
      v.setNW(R2.Origin);       (* don't let current nw get in way of
                                   conversion *)
      v.setNW(MG.ScreenPointToMGC(v, Point.T{0, -Rect.VerSize(cd.new)}));
    END;
    ReshapeLeaveOrigin(v, cd);
  END ReshapeSWOrigin;

PROCEDURE ReshapeLeaveOrigin (v: V; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    LOCK v.mu DO v.dirtyRegion := Rect.Full END;
    v.setDoubleBuffer(v.doubleBuffer);
    v.repaint(Region.FromRect(cd.new));
  END ReshapeLeaveOrigin;

PROCEDURE ShapeFixed (v: V; axis: Axis.T; <* UNUSED *> nn: CARDINAL):
  VBT.SizeRange =
  VAR
    bounds: R2Box.T;
    size  : R2.T;
  BEGIN
    LOCK v.mu DO
      bounds := v.displayList.bounds(v);
      size := R2Box.Size(bounds);
      IF axis = Axis.T.Hor THEN
        WITH p = Pts.ToScreenPixels(v, size[0], Axis.T.Hor) DO
          RETURN VBT.SizeRange{lo := p, pref := p, hi := p + 1};
        END;
      ELSE
        WITH p = Pts.ToScreenPixels(v, size[1], Axis.T.Ver) DO
          RETURN VBT.SizeRange{lo := p, pref := p, hi := p + 1};
        END;
      END;
    END
  END ShapeFixed;

PROCEDURE ShapeStretchy (v: V; axis: Axis.T; <* UNUSED *> nn: CARDINAL):
  VBT.SizeRange =
  VAR bounds: R2Box.T;
      size: R2.T;
  BEGIN
    LOCK v.mu DO
      bounds := v.displayList.bounds(v);
      size := R2Box.Size(bounds);
      size[0] := MAX(0.0, size[0]);
      size[1] := MAX(0.0, size[1]);
      IF axis = Axis.T.Hor THEN
        WITH p = Pts.ToScreenPixels(v, size[0], Axis.T.Hor) DO
          RETURN VBT.SizeRange{lo := 0, pref := p, hi :=
                               MAX(p + 1, VBT.DefaultShape.hi)};
        END;
      ELSE
        WITH p = Pts.ToScreenPixels(v, size[1], Axis.T.Ver) DO
          RETURN VBT.SizeRange{lo := 0, pref := p, hi :=
                               MAX(p + 1, VBT.DefaultShape.hi)};
        END;
      END;
    END
  END ShapeStretchy;

PROCEDURE MouseV (v: V; READONLY cd: VBT.MouseRec) =
  VAR sel: MG.T;
  BEGIN
    LOCK v.mu DO
      sel := v.selector.select(v, MG.ScreenPointToMGC(v, cd.cp.pt), cd);
    END;
    v.selectee.select(v, sel, cd);
  END MouseV;

REVEAL
  Selectee = SelecteePublic BRANDED OBJECT END;

PROCEDURE SelecteeDefault (<* UNUSED *>          s : Selectee;
                           <* UNUSED *>          v : V;
                           <* UNUSED *>          t : MG.T;
                           <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END SelecteeDefault;

VAR
  selecteeDefault := NEW(Selectee, select := SelecteeDefault);

REVEAL
  Selector = SelectorPublic BRANDED OBJECT END;

TYPE
  SelectorIter = MG.GroupIterator OBJECT
    closest: MG.T := NIL;
    dist   : REAL := 999999999.0;
    pos: R2.T;
  OVERRIDES
    proc := SelectorProc;
  END;

PROCEDURE SelectorClosest (<* UNUSED *>          s  : Selector;
                                                 v  : V;
                                        READONLY pos: R2.T;
                           <* UNUSED *> READONLY cd : VBT.MouseRec): MG.T =
  VAR iter := NEW(SelectorIter, v := v, pos := pos);
  BEGIN
    EVAL v.displayList.iterate(iter, TRUE, FALSE);
    RETURN iter.closest;
  END SelectorClosest;

PROCEDURE SelectorProc (iter: SelectorIter; t: MG.T): BOOLEAN =
  VAR
    dx, dy, dist: REAL;
    pos                            := iter.pos;
    bounds := t.appearance.boundingBox(t, iter.v);
  BEGIN
    IF pos[0] < bounds[0].lo THEN
      dx := bounds[0].lo - pos[0]
    ELSIF pos[0] > bounds[0].hi THEN
      dx := pos[0] - bounds[0].hi;
    ELSE
      dx := 0.0;
    END;
    IF pos[1] < bounds[1].lo THEN
      dy := bounds[1].lo - pos[1]
    ELSIF pos[1] > bounds[1].hi THEN
      dy := pos[1] - bounds[1].hi
    ELSE
      dy := 0.0;
    END;
    IF dx = 0.0 AND dy = 0.00 THEN
      iter.closest := t;
      iter.dist := 0.0;
      RETURN FALSE
    ELSE
      dist := dx * dx + dy * dy;
      IF dist < iter.dist THEN iter.closest := t; iter.dist := dist; END;
      RETURN TRUE;
    END;
  END SelectorProc;

PROCEDURE AddAnimation (v: V; anim: Animate.T; mg: MG.T) =
  BEGIN
    LOCK v.mu DO AddAnimationLocked(v, anim, mg); END;
  END AddAnimation;

PROCEDURE AddAnimationLocked (v: V; anim: Animate.T; mg: MG.T) =
  BEGIN
    IF v.animations = NIL THEN
      v.animations := NEW(Animate.Group).init();
    END;
    v.animations.add(v, NEW(Animate.Composite, t := anim, mg := mg));
  END AddAnimationLocked;

PROCEDURE Animation (v: V; duration := 1.0) RAISES {Thread.Alerted} =
  VAR a: Animate.Group;
  BEGIN
    LOCK v.mu DO a := v.animations; v.animations := NIL; END;
    IF a # NIL THEN
      Animate.Do(a, NIL, v, duration);
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END Animation;

BEGIN
  selectorClosest := NEW(Selector, select := SelectorClosest);
END MGV.
