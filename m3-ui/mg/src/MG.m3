(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Aug 27 11:00:39 PDT 1996 by najork   *)
(*      modified on Fri Aug 19 16:34:00 PDT 1994 by steveg   *)
(*      modified on Wed Jul 22 01:26:04 PDT 1992 by harrison *)



(*      modified on Fri Jul 10 20:45:45 1992 by mhb      *)

MODULE MG EXPORTS MG, MGPublic;

<* PRAGMA LL *>

IMPORT Axis, Color, ColorName, Font, IntRefTbl, Math, Matrix2D, PaintOp,
       Point, Pts, R2, R2Box, Rect, Region, VBT,
       CirclePixmapCache, Path, R2Path, R2PathExtra, Fuzzy, (* ShapePixmap, *)
       Pixmap, Text;

REVEAL
  T = TPublic BRANDED OBJECT
      OVERRIDES
        setColor     := SetColorT;
        setFont      := SetFontT;
        setHighlight := SetHighlightT;
        setVisible   := SetVisibleT;
        setWeight    := SetWeightT;
        setAlignment := SetAlignmentT;
        setLabel     := SetLabelT;

        (* copies fields of known subtypes: line, lineEnd, ellipse, shape,
           label, rectangle (not group) *)
        render    := RenderError;
        transform := TransformDefault;
        bounds    := BoundsError;
        rgn       := RgnError;

        init := InitT;
      END;

VAR circleCache := CirclePixmapCache.New();
(* MG's private circle pixmap cache *)

VAR                             (* CONST *)
  RealDefaultFont := Font.FromName(
                       ARRAY OF
                         TEXT{
                         "-adobe-helvetica-medium-r-normal--*-100-*-*-p-*-iso8859-1"});

PROCEDURE PixelsFromWeight (v: V; axis: Axis.T; weight: REAL): INTEGER =
  BEGIN
    RETURN Pts.ToScreenPixels(v, weight, axis);
  END PixelsFromWeight;

(* Return the number of pixels indicated by an MG.T's weight.  If the
   weight <= 0.0, return 0.  Otherwise calculate the number of pixels
   indicated by the weight (with a minimum of 1).  We take the average of
   both axes. *)
<* INLINE *> PROCEDURE BorderInPixels (v: V; t: T): INTEGER =
  BEGIN
    IF t.weight <= 0.0 THEN
      RETURN 0;
    ELSE
      RETURN MAX(1, (PixelsFromWeight(v, Axis.T.Hor, t.weight)
                       + PixelsFromWeight(v, Axis.T.Ver, t.weight)) DIV 2);
    END;
  END BorderInPixels;

<* INLINE *> PROCEDURE ScreenPointToMGC (v: V; READONLY pScreen: Point.T):
  R2.T =
  VAR
    dom          := VBT.Domain(v.paintVBT);
    ptsToPixelsX := Pts.ToPixels(v, 1.0, Axis.T.Hor);
    ptsToPixelsY := Pts.ToPixels(v, 1.0, Axis.T.Ver);
  BEGIN
    RETURN R2.T{v.nw[0] + FLOAT(pScreen.h - dom.west) / ptsToPixelsX,
                v.nw[1] + FLOAT(dom.north - pScreen.v) / ptsToPixelsY}
  END ScreenPointToMGC;

(* Convert a distance in MG space to pixels in screen space *)
<* INLINE *> PROCEDURE MGCDistToPixels (v: V; READONLY p: R2.T): Point.T =
  BEGIN
    RETURN Point.T{Pts.ToScreenPixels(v, p[0], Axis.T.Hor),
                   Pts.ToScreenPixels(v, p[1], Axis.T.Ver)};
  END MGCDistToPixels;

(* Convert a point in MG space to a point in screen space *)
<* INLINE *> PROCEDURE MGCToScreenPoint (v: V; READONLY p: R2.T): Point.T =
  VAR dom := VBT.Domain(v.paintVBT);
  BEGIN
    RETURN Point.T{
             dom.west + Pts.ToScreenPixels(v, p[0] - v.nw[0], Axis.T.Hor),
             dom.north + Pts.ToScreenPixels(v, v.nw[1] - p[1], Axis.T.Ver)}
  END MGCToScreenPoint;

<* INLINE *>
  PROCEDURE MGCToScreenRect (v: V; READONLY corner1, corner2: R2.T):
  Rect.T =
  BEGIN
    RETURN Rect.FromCorners(
             MGCToScreenPoint(v, corner1), MGCToScreenPoint(v, corner2));
  END MGCToScreenRect;

<* INLINE *>
  PROCEDURE MGCToScreenRectNonEmpty (v: V; READONLY corner1, corner2: R2.T):
  Rect.T =
  BEGIN
    RETURN RectFromCorners(
             MGCToScreenPoint(v, corner1), MGCToScreenPoint(v, corner2));
  END MGCToScreenRectNonEmpty;

<* INLINE *> PROCEDURE RectFromCorners (p1, p2: Point.T): Rect.T =
  BEGIN
    RETURN Rect.Join(Rect.FromPoint(p1), Rect.FromPoint(p2))
  END RectFromCorners;

(* Here is a special version of MGCToScreenRect that returns a Rect.T of
   constant dimensions.  In other words it does not depend on the absolute
   position of the two corners. *)
<* INLINE *>
  PROCEDURE MGCToStableScreenRect (v: V; READONLY corner1, corner2: R2.T):
  Rect.T =
  VAR
    size      := MGCDistToPixels(v, R2.Sub(corner2, corner1));
    reference := MGCToScreenPoint(v, corner1);
  BEGIN
    RETURN Rect.FromAbsEdges(reference.h, reference.h + size.h,
                             reference.v, reference.v - size.v);
  END MGCToStableScreenRect;

(* We could be smarter here and spot rotations MOD 2 Pi, but it's probably
   not going to come up the frequenty. <* UNUSED *>
PROCEDURE EquivalentRotationAndScaling (READONLY m1, m2: Matrix2D.T):
  BOOLEAN =
  BEGIN
    RETURN
      m1[0] = m2[0] AND m1[1] = m2[1] AND m1[2] = m2[2] AND m1[3] = m2[3];
  END EquivalentRotationAndScaling;
*)

EXCEPTION Fatal;
<* FATAL Fatal *>

PROCEDURE Register (v: V; id: ID; t: T) =
  BEGIN
    LOCK v.mu DO
      IF v.lookup.put(id, t) THEN RAISE Fatal END;
    END;
  END Register;

PROCEDURE Lookup (v: V; id: ID): T =
  VAR res: REFANY;
  BEGIN
    LOCK v.mu DO
      IF id # NoID AND v.lookup.get(id, res) THEN
        RETURN res;
      ELSE
        RETURN NIL;
      END;
    END;
  END Lookup;

PROCEDURE ResetLookupsLocked (v: V) =
  BEGIN
    v.lookup := NEW(IntRefTbl.Default).init();
  END ResetLookupsLocked;

PROCEDURE ResetLookups (v: V) =
  BEGIN
    LOCK v.mu DO ResetLookupsLocked(v); END;
  END ResetLookups;

PROCEDURE Clear (v: V) =
  BEGIN
    LOCK v.mu DO
      ResetLookupsLocked(v);
      v.animations := NIL;
      v.displayList := NEW(Group).init();
    END;
    VBT.NewShape(v);
    v.mgRedisplay(Region.Empty);
  END Clear;

PROCEDURE InitT (t    : T;
                 v    : V          := NIL;
                 group: Group      := NIL   ): T =
  BEGIN
    IF t.appearance = NIL THEN t.appearance := appearanceDefault END;
    IF t.color = NIL THEN t.color := PaintOp.bgFg END;
    IF t.font = DefaultFont THEN t.font := RealDefaultFont END;
    IF group # NIL THEN
      group.addBefore(v, t)
    ELSIF v # NIL THEN
      v.displayList.addBefore(v, t);
    END;
    IF t.id # NoID AND v # NIL THEN Register(v, t.id, t); END;
    RETURN t
  END InitT;

<* INLINE *> PROCEDURE Pos (t: T; v: V): R2.T =
  BEGIN
    LOCK v.mu DO RETURN R2.T{t.m[4], t.m[5]}; END;
  END Pos;

<* INLINE *> PROCEDURE PosLocked (t: T; <* UNUSED *> v: V := NIL): R2.T =
  BEGIN
    RETURN R2.T{t.m[4], t.m[5]};
  END PosLocked;

PROCEDURE RenderError (<* UNUSED *>     self: T;
                       <* UNUSED *>     v   : V;
                       <* UNUSED *> VAR r   : Rect.T) =
  BEGIN <* ASSERT FALSE *>
  END RenderError;

PROCEDURE TransformDefault (self: T; v: V; READONLY m: Matrix2D.T) =
  BEGIN
    v.dirtyRegion := Rect.Join(v.dirtyRegion,
                               self.appearance.region(self, v).r);
    self.m := Matrix2D.Concat(self.m, m);
    self.dirty := TRUE;
  END TransformDefault;

PROCEDURE BoundsError (<* UNUSED *> self: T; <* UNUSED *> v: V): R2Box.T =
  BEGIN <* ASSERT FALSE *>
  END BoundsError;

PROCEDURE RgnError (<* UNUSED *> self: T; <* UNUSED *> v: V): Region.T =
  BEGIN <* ASSERT FALSE *>
  END RgnError;

PROCEDURE PaintLabel (t: T; v: V; READONLY clip, rect: Rect.T) =
  VAR bnds: Rect.T;
  BEGIN
    VBT.PaintText(v.paintVBT, clip, LabelPoint(t, v, rect, bnds), t.font,
                  t.label, t.color.transparentFg);
  END PaintLabel;

PROCEDURE LabelRect (t: T; v: V; READONLY rect: Rect.T): Rect.T =
  VAR
    bnds: Rect.T;
    pt := LabelPoint(t, v, rect, bnds);
  BEGIN
    RETURN Rect.Move(bnds, pt);
  END LabelRect;

PROCEDURE RegionWithLabel (t: T; v: V; READONLY rect: Rect.T): Region.T =
  BEGIN
    IF Text.Equal(t.label, "") THEN
      RETURN Region.FromRect(rect)
    ELSE
      RETURN Region.FromRect(Rect.Join(rect, LabelRect(t, v, rect)));
    END
  END RegionWithLabel;
  
PROCEDURE LabelPoint (              t   : T;
                                    v   : V;
                      READONLY      rect: Rect.T;
                      VAR (* OUT *) bnds: Rect.T  ): Point.T =
  VAR b: Rect.T;
  BEGIN
    bnds := VBT.BoundingBox(v, t.label, t.font);
    b := Rect.Inset(bnds, -ROUND(0.15 * FLOAT(Rect.VerSize(bnds))));
    CASE t.alignment OF
    | Alignment.NE =>
        RETURN
          Point.Add(Rect.NorthEast(rect), Point.Minus(Rect.SouthWest(b)))
    | Alignment.N => RETURN Point.Add(RectN(rect), Point.Minus(RectS(b)))
    | Alignment.NW =>
        RETURN
          Point.Add(Rect.NorthWest(rect), Point.Minus(Rect.SouthEast(b)))
    | Alignment.W => RETURN Point.Add(RectW(rect), Point.Minus(RectE(b)))
    | Alignment.SW =>
        RETURN
          Point.Add(Rect.SouthWest(rect), Point.Minus(Rect.NorthEast(b)))
    | Alignment.S => RETURN Point.Add(RectS(rect), Point.Minus(RectN(b)))
    | Alignment.SE =>
        RETURN
          Point.Add(Rect.SouthEast(rect), Point.Minus(Rect.NorthWest(b)))
    | Alignment.E => RETURN Point.Add(RectE(rect), Point.Minus(RectW(b)))
    | Alignment.Center =>
        RETURN Point.Add(Rect.Middle(rect), Point.Minus(Rect.Middle(b)))
    END
  END LabelPoint;

PROCEDURE RectS(r: Rect.T): Point.T =
  BEGIN
    RETURN Point.T{(r.east + r.west) DIV 2, r.south}
  END RectS;

PROCEDURE RectN(r: Rect.T): Point.T =
  BEGIN
    RETURN Point.T{(r.east + r.west) DIV 2, r.north}
  END RectN;

PROCEDURE RectE (r: Rect.T): Point.T =
  BEGIN
    RETURN Point.T{r.east, (r.north + r.south) DIV 2}
  END RectE;

PROCEDURE RectW(r: Rect.T): Point.T =
  BEGIN
    RETURN Point.T{r.west, (r.north + r.south) DIV 2}
  END RectW;

REVEAL
  Group = GroupPublic BRANDED OBJECT
          OVERRIDES
            addAfter  := GroupAddAfter;
            addBefore := GroupAddBefore;
            remove    := GroupRemove;
            top       := GroupTop;
            bottom    := GroupBottom;
            init      := InitGroup;

            render       := RenderGroup;
            setColor     := SetColorOfGroup;
            setFont      := SetFontOfGroup;
            setHighlight := SetHighlightOfGroup;
            setVisible   := SetVisibleOfGroup;
            setWeight    := SetWeightOfGroup;
            setAlignment := SetAlignmentOfGroup;
            setLabel     := SetLabelOfGroup;
            bounds       := BoundsGroup;
            rgn          := RgnGroup;

            iterate   := GroupIterate;
            transform := TransformGroup;
          END;

PROCEDURE IterateGroup (group      : Group;
                        iter       : GroupIterator;
                        recursively                  := FALSE;
                        fromBack                     := TRUE   ): BOOLEAN =
  BEGIN
    LOCK iter.v.mu DO RETURN group.iterate(iter, recursively, fromBack) END
  END IterateGroup;

PROCEDURE GroupIterate (group      : Group;
                        iter       : GroupIterator;
                        recursively                  := FALSE;
                        fromBack                     := TRUE   ): BOOLEAN =
  BEGIN
    IF fromBack THEN
      FOR i := 0 TO group.cntElems - 1 DO
        WITH t = group.elems[i] DO
          IF recursively THEN
            TYPECASE t OF
            | Group (subgroup) =>
                IF NOT subgroup.iterate(iter, recursively) THEN
                  RETURN FALSE
                END;
            ELSE
            END;
          END;
          IF NOT iter.proc(t) THEN RETURN FALSE; END; (* if *)
        END;
      END;
    ELSE
      FOR i := group.cntElems - 1 TO 0 BY -1 DO
        WITH t = group.elems[i] DO
          IF recursively THEN
            TYPECASE t OF
            | Group (subgroup) =>
                IF NOT subgroup.iterate(iter, recursively) THEN
                  RETURN FALSE
                END;
            ELSE
            END;
          END;
          IF NOT iter.proc(t) THEN RETURN FALSE; END; (* if *)
        END;
      END;
    END;
    RETURN TRUE
  END GroupIterate;

PROCEDURE AddToGroup (group: Group; v: V; t: T; atTop := TRUE) =
  BEGIN
    LOCK v.mu DO
      IF atTop THEN group.addBefore(v, t) ELSE group.addAfter(v, t) END;
    END
  END AddToGroup;

PROCEDURE AddToGroupAfter (group: Group; v: V; t: T; prev: T := NIL) =
  BEGIN
    LOCK v.mu DO group.addAfter(v, t, prev) END
  END AddToGroupAfter;

PROCEDURE AddToGroupBefore (group: Group; v: V; t: T; next: T := NIL) =
  BEGIN
    LOCK v.mu DO group.addBefore(v, t, next) END
  END AddToGroupBefore;

PROCEDURE GroupFind (group: Group; t: T): INTEGER =
  BEGIN
    FOR i := 0 TO group.cntElems - 1 DO
      IF group.elems[i] = t THEN RETURN i END;
    END;
    <* ASSERT FALSE *>
  END GroupFind;

PROCEDURE GroupInsert (group: Group; v: V; at: INTEGER; t: T) =
  VAR
    cnt := group.cntElems;
    pos := PosLocked(t, v);
    deltaBack: R2.T;
  BEGIN
    IF cnt = NUMBER(group.elems^) THEN
      VAR new := NEW(REF ARRAY OF T, (3 * group.cntElems) DIV 2);
      BEGIN
        SUBARRAY(new^, 0, at) := SUBARRAY(group.elems^, 0, at);
        SUBARRAY(new^, at + 1, cnt - at) := SUBARRAY(group.elems^, at,
                                                     cnt - at);
        group.elems := new;
      END;
    ELSE
      SUBARRAY(group.elems^, at + 1, cnt - at) := SUBARRAY(group.elems^,
                                                           at, cnt - at)
    END;
    group.elems[at] := t;
    INC(group.cntElems);
    (* set the child's transformation, but don't move it (a good trick) so
       transform pos to get the new position (pos') calculate the delta
       back to the old pos (pos - pos') concat node.m and a translation
       back by delta *)
    deltaBack := R2.Sub(pos, Matrix2D.Transform(group.m, pos));
    t.transform(v, Matrix2D.Concat(group.m, Matrix2D.Translate(
                                              deltaBack[0], deltaBack[1])));

  END GroupInsert;

PROCEDURE GroupDelete (group: Group; v: V; at: INTEGER; t: T) =
  VAR
    cnt := group.cntElems;
    pos := PosLocked(t, v);
    inv := Matrix2D.Inverse(group.m);
    deltaBack: R2.T;
  BEGIN
    SUBARRAY(group.elems^, at, cnt - at - 1) :=
      SUBARRAY(group.elems^, at + 1, cnt - at - 1);
    DEC(group.cntElems);
    (* remove the parent's matrix transformation without moving child so
       add back in a translation to the current position after the *)
    deltaBack := R2.Sub(pos, Matrix2D.Transform(inv, pos));
    t.transform(v, Matrix2D.Concat(inv, Matrix2D.Translate(deltaBack[0],
                                                           deltaBack[1])));
  END GroupDelete;

PROCEDURE GroupAddAfter (group: Group; v: V; t: T; prev: T := NIL) =
  VAR i: INTEGER;
  BEGIN
    IF t = NIL THEN RETURN END;
    t.dirty := TRUE;
    IF prev = NIL THEN i := 0 ELSE i := GroupFind(group, prev) + 1; END;
    GroupInsert(group, v, i, t);
  END GroupAddAfter;

PROCEDURE GroupAddBefore (group: Group; v: V; t: T; next: T := NIL) =
  VAR i: INTEGER;
  BEGIN
    IF t = NIL THEN RETURN END;
    t.dirty := TRUE;
    IF next = NIL THEN
      i := group.cntElems
    ELSE
      i := GroupFind(group, next);
    END;
    GroupInsert(group, v, i, t);
  END GroupAddBefore;

PROCEDURE RemoveFromGroup (group: Group; v: V; t: T) =
  BEGIN
    LOCK v.mu DO group.remove(v, t); END;
  END RemoveFromGroup;

PROCEDURE GroupRemove (group: Group; v: V; t: T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    GroupDelete(group, v, GroupFind(group, t), t);
    v.dirtyRegion := Rect.Join(v.dirtyRegion, t.appearance.region(t, v).r)
  END GroupRemove;

PROCEDURE TopInGroup (group: Group; v: V; t: T) =
  BEGIN
    LOCK v.mu DO group.top(v, t) END;
  END TopInGroup;

PROCEDURE GroupTop (group: Group; v: V; t: T) =
  VAR
    i := GroupFind(group, t);
    last := group.cntElems - 1;
  BEGIN
    IF i # last THEN
      group.dirty := TRUE;
      GroupDelete(group, v, i, t);
      GroupInsert(group, v, last, t);
      v.dirtyRegion := Rect.Join(
                         v.dirtyRegion, t.appearance.region(t, v).r);
    END;
  END GroupTop;

PROCEDURE BottomInGroup (group: Group; v: V; t: T) =
  BEGIN
    LOCK v.mu DO group.bottom(v, t) END;
  END BottomInGroup;

PROCEDURE GroupBottom (group: Group; v: V; t: T) =
  VAR i := GroupFind(group, t);
  BEGIN
    IF i # 0 THEN
      group.dirty := TRUE;
      SUBARRAY(group.elems^, 1, i) := SUBARRAY(group.elems^, 0, i);
      group.elems[0] := t;
      v.dirtyRegion := Rect.Join(v.dirtyRegion, t.appearance.region(t, v).r);
    END;
  END GroupBottom;

PROCEDURE InitGroup (group      : Group;
                     elemsSize            := 4;
                     v          : V       := NIL;
                     parentGroup: Group   := NIL  ): Group =
  BEGIN
    group.elems := NEW(REF ARRAY OF T, elemsSize);
    EVAL T.init(group, v, parentGroup);
    RETURN group
  END InitGroup;

TYPE
  IterRender = GroupIterator OBJECT
                 <* LL = v.mu *>
                 clip: Rect.T;
               OVERRIDES
                 proc := Renderer;
               END;

PROCEDURE RenderGroup (group: Group; v: V; VAR clip: Rect.T) =
  VAR iter := NEW(IterRender, v := v, clip := clip);
  BEGIN
    IF group.visible # 0.0 THEN EVAL group.iterate(iter) END;
    group.dirty := FALSE;
    clip := iter.clip;
  END RenderGroup;

(* must make sure that render t doesn't overpaint any other object that is
   (visually) on top of it *)
PROCEDURE Renderer (iter: IterRender; t: T): BOOLEAN =
  BEGIN
    TYPECASE t OF
    | Group => t.appearance.paint(t, iter.v, iter.clip);
    ELSE
      (* render element if it is dirty, or it intersects the clip *)
      WITH rgn = t.appearance.region(t, iter.v) DO
        IF t.dirty THEN
          iter.clip := Rect.Join(iter.clip, rgn.r);
          t.appearance.paint(t, iter.v, iter.clip);
          t.dirty := FALSE;
        ELSIF Region.OverlapRect(iter.clip, rgn) THEN
          t.appearance.paint(t, iter.v, iter.clip);
        END;
      END;
    END;
    RETURN TRUE;
  END Renderer;

TYPE
  IterBB =
    GroupIterator OBJECT bounds := R2Box.Empty OVERRIDES proc := BBer END;

PROCEDURE BoundsGroup (group: Group; v: V): R2Box.T =
  VAR iter := NEW(IterBB, v := v);
  BEGIN
    EVAL group.iterate(iter);

    RETURN iter.bounds;
  END BoundsGroup;

PROCEDURE BBer (iter: IterBB; t: T): BOOLEAN =
  BEGIN
    iter.bounds := R2Box.Join(iter.bounds, t.bounds(iter.v));
    RETURN TRUE;
  END BBer;

TYPE
  IterRgn = GroupIterator OBJECT rgn: Region.T OVERRIDES proc := Rgner END;

PROCEDURE RgnGroup (group: Group; v: V): Region.T = <* LL = v.mu *>
  VAR iter := NEW(IterRgn, v := v, rgn := Region.Empty);
  BEGIN
    EVAL group.iterate(iter);
    RETURN iter.rgn;
  END RgnGroup;

PROCEDURE Rgner (iter: IterRgn; t: T): BOOLEAN =
  BEGIN
    IF t.visible # 0.0 THEN
      iter.rgn := Region.Join(iter.rgn, t.appearance.region(t, iter.v));
    END;
    RETURN TRUE;
  END Rgner;

TYPE
  IterColor = GroupIterator OBJECT
                color: PaintOp.ColorScheme
              OVERRIDES
                proc := Colorer
              END;

PROCEDURE SetColorOfGroup (group: Group; v: V; color: PaintOp.ColorScheme) = <* LL = v.mu *>

  BEGIN
    EVAL group.iterate(NEW(IterColor, v := v, color := color));
    group.color := color;
  END SetColorOfGroup;

PROCEDURE Colorer (iter: IterColor; t: T): BOOLEAN =
  BEGIN
    t.setColor(iter.v, iter.color);
    RETURN TRUE;
  END Colorer;

TYPE
  IterFont =
    GroupIterator OBJECT font: Font.T OVERRIDES proc := Fonter END;

PROCEDURE SetFontOfGroup (group: Group; v: V; font := DefaultFont) = <* LL = v.mu *>
  BEGIN
    IF font = DefaultFont THEN font := RealDefaultFont; END;
    EVAL group.iterate(NEW(IterFont, v := v, font := font));
    group.font := font;
  END SetFontOfGroup;

PROCEDURE Fonter (iter: IterFont; t: T): BOOLEAN =
  BEGIN
    t.setFont(iter.v, iter.font);
    RETURN TRUE;
  END Fonter;

TYPE
  IterHighlight =
    GroupIterator OBJECT highlight: REAL OVERRIDES proc := Highlighter END;

PROCEDURE SetHighlightOfGroup (group: Group; v: V; highlight := 0.0) =
  BEGIN
    EVAL group.iterate(NEW(IterHighlight, v := v, highlight := highlight));
    group.highlight := highlight;
  END SetHighlightOfGroup;

PROCEDURE Highlighter (iter: IterHighlight; t: T): BOOLEAN =
  BEGIN
    t.setHighlight(iter.v, iter.highlight);
    RETURN TRUE;
  END Highlighter;

TYPE
  IterVisible =
    GroupIterator OBJECT visible: REAL OVERRIDES proc := Visibleer END;

PROCEDURE SetVisibleOfGroup (group: Group; v: V; visible := 1.0) =

  BEGIN
    EVAL group.iterate(NEW(IterVisible, v := v, visible := visible));
    group.visible := visible;
  END SetVisibleOfGroup;

PROCEDURE Visibleer (iter: IterVisible; t: T): BOOLEAN =
  BEGIN
    t.setVisible(iter.v, iter.visible);
    RETURN TRUE;
  END Visibleer;

TYPE
  IterWeight =
    GroupIterator OBJECT weight: REAL OVERRIDES proc := Weighter END;

PROCEDURE SetWeightOfGroup (group: Group; v: V; weight := 1.0) =
  BEGIN
    EVAL group.iterate(NEW(IterWeight, v := v, weight := weight));
    group.weight := weight;
  END SetWeightOfGroup;

PROCEDURE Weighter (iter: IterWeight; t: T): BOOLEAN =
  BEGIN
    t.setWeight(iter.v, iter.weight);
    RETURN TRUE;
  END Weighter;

TYPE
  IterAlignment = GroupIterator OBJECT
                    alignment: Alignment
                  OVERRIDES
                    proc := Alignmenter
                  END;

PROCEDURE SetAlignmentOfGroup (group: Group;
                               v    : V;
                               alignment := Alignment.Center) =
  BEGIN
    EVAL group.iterate(NEW(IterAlignment, v := v, alignment := alignment));
    group.alignment := alignment;
  END SetAlignmentOfGroup;

PROCEDURE Alignmenter (iter: IterAlignment; t: T): BOOLEAN =
  BEGIN
    t.setAlignment(iter.v, iter.alignment);
    RETURN TRUE;
  END Alignmenter;

TYPE
  IterLabel =
    GroupIterator OBJECT label: TEXT OVERRIDES proc := Labeler END;

PROCEDURE SetLabelOfGroup (group: Group; v: V; label: TEXT) =
  BEGIN
    EVAL group.iterate(NEW(IterLabel, v := v, label := label));
  END SetLabelOfGroup;

PROCEDURE Labeler (iter: IterLabel; t: T): BOOLEAN =
  BEGIN
    t.setLabel(iter.v, iter.label);
    RETURN TRUE;
  END Labeler;

TYPE
  IterTransform = GroupIterator OBJECT
                    m: Matrix2D.T;
                  OVERRIDES
                    proc := Transformer
                  END;

PROCEDURE TransformGroup (group: Group; v: V; READONLY m: Matrix2D.T) =
  BEGIN
    EVAL group.iterate(NEW(IterTransform, v := v, m := m));
    group.m := Matrix2D.Concat(group.m, m);
    group.dirty := TRUE;
  END TransformGroup;

PROCEDURE Transformer (iter: IterTransform; t: T): BOOLEAN =
  BEGIN
    t.transform(iter.v, iter.m);
    RETURN TRUE;
  END Transformer;

REVEAL
  Line = LinePublic BRANDED OBJECT
         OVERRIDES
           init      := InitLine;
           render    := RenderLine;
           bounds    := BoundsLine;
           rgn       := RgnLine;
           reshape   := ReshapeLine;
           setStyle  := StyleLine;
         END;

PROCEDURE InitLine (         line    : Line;
                    READONLY from, to: R2.T;
                             v       : V      := NIL;
                             group   : Group  := NIL  ): Line =
  BEGIN
    EVAL T.init(line, v, group);
    line.m := Matrix2D.Translate(from[0], from[1]);
    line.to := R2.Sub(to, from);
    RETURN line;
  END InitLine;

PROCEDURE SetLineStyle(line: Line; v: V; style := VBT.EndStyle.Round) =
  BEGIN
    LOCK v.mu DO
      line.setStyle(v, style);
    END;
  END SetLineStyle;

PROCEDURE SetLineReshape(line: Line; v: V; READONLY from, to: R2.T) =
  BEGIN
    LOCK v.mu DO
      line.reshape(v, from, to);
    END;
  END SetLineReshape;

PROCEDURE StyleLine (self: Line; v: V; style := VBT.EndStyle.Round) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.style := style;
    self.dirty := TRUE;
  END StyleLine;

<* INLINE *> PROCEDURE LineFrom (line: Line; v: V): R2.T =
  BEGIN
    RETURN PosLocked(line, v)
  END LineFrom;

<* INLINE *> PROCEDURE LineTo (line: Line; v: V): R2.T =
  BEGIN
    LOCK v.mu DO RETURN Matrix2D.Transform(line.m, line.to); END;
  END LineTo;

<* INLINE *> PROCEDURE LineFromLocked (line: Line; v: V := NIL): R2.T =
  BEGIN
    RETURN PosLocked(line, v)
  END LineFromLocked;

<* INLINE *> PROCEDURE LineToLocked (line: Line; <* UNUSED *> v: V := NIL):
  R2.T =
  BEGIN
    RETURN Matrix2D.Transform(line.m, line.to);
  END LineToLocked;

PROCEDURE RenderLine (line: Line; v: V; VAR clip: Rect.T) =
  BEGIN
    IF line.visible > 0.0 AND line.weight > 0.0 THEN
      WITH from = MGCToScreenPoint(v, PosLocked(line)),
           to = MGCToScreenPoint(v, LineToLocked(line)) DO
        VBT.Line(v.paintVBT, clip, from, to,
                 PixelsFromWeight(v, Axis.T.Ver, line.weight),
                 op := line.color.fg, end := line.style);
        PaintLabel(line, v, clip, RectFromCorners(from, to));
      END;
    END;
  END RenderLine;

PROCEDURE BoundsLine (line: Line; <* UNUSED *> v: V): R2Box.T =
  BEGIN
    RETURN R2Box.FromCorners(PosLocked(line), LineToLocked(line));
  END BoundsLine;

PROCEDURE RgnLine (line: Line; v: V): Region.T =
  VAR
    dw := line.weight / 2.0;
    from := PosLocked(line);
    to := LineToLocked(line);
    n := MAX(from[1], to[1]) + dw;
    s := MIN(from[1], to[1]) - dw;
    w := MIN(from[0], to[0]) - dw;
    e := MAX(from[0], to[0]) + dw;
    ne := MGCToScreenPoint(v, R2.T{e, n});
    sw := MGCToScreenPoint(v, R2.T{w, s});
  BEGIN
    RETURN RegionWithLabel(line, v, RectFromCorners(ne, sw))
  END RgnLine;

<* INLINE *>
  PROCEDURE SetPosLocked (t: T; READONLY pos: R2.T; <* UNUSED *> v: V := NIL) =
  BEGIN
    t.m[4] := pos[0];
    t.m[5] := pos[1];
  END SetPosLocked;

PROCEDURE ReshapeLine (line: Line; v: V; READONLY from, to: R2.T) =
  BEGIN
    v.dirtyRegion := Rect.Join(v.dirtyRegion,
                               line.appearance.region(line, v).r);
    SetPosLocked(line, from);
    line.to := R2.Sub(to, from);
    line.dirty := TRUE;
  END ReshapeLine;

REVEAL
  LineEnd = LineEndPublic BRANDED OBJECT
            OVERRIDES
              render    := RenderLineEnd;
              bounds    := BoundsLineEnd;
              rgn       := RgnLineEnd;
              transform := TransformLineEnd;
            END;

PROCEDURE RenderLineEnd (le: LineEnd; v: V; VAR clip: Rect.T) =
  BEGIN
    IF le.visible > 0.0 AND (le.controlsFrom OR le.dirty) THEN
      (* SCG 2 Mar 1993.  If the non-controlling end of a line is
         animation moved in GraphVBT, then the line doesn't get
         repainted.  So repaint the line here even if the non-controlling
         end is dirty.  The worst that happens is an extra VBT.Line *)
      le.line.render(v, clip);
    END;
  END RenderLineEnd;

PROCEDURE BoundsLineEnd (le: LineEnd; v: V): R2Box.T =
  BEGIN
    RETURN le.line.bounds(v);
  END BoundsLineEnd;

PROCEDURE RgnLineEnd (le: LineEnd; v: V): Region.T =
  BEGIN
    RETURN le.line.appearance.region(le.line, v)
  END RgnLineEnd;

PROCEDURE TransformLineEnd (le: LineEnd; v: V; READONLY m: Matrix2D.T) =
  VAR
    to   := LineToLocked(le.line);
    from := PosLocked(le.line);
  BEGIN
    le.m := Matrix2D.Concat(le.m, m);
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, le.line.appearance.region(le.line, v).r);
    IF le.controlsFrom THEN
      from := Matrix2D.Transform(m, from);
      SetPosLocked(le.line, from);
    ELSE
      to := Matrix2D.Transform(m, to);
    END;
    le.line.to := R2.Sub(to, from);
    le.dirty := TRUE;
  END TransformLineEnd;

REVEAL
  Rectangle = RectanglePublic BRANDED OBJECT
              OVERRIDES
                init    := InitRectangle;
                render  := RenderRectangle;
                bounds  := BoundsRectangle;
                rgn     := RgnRectangle;
                reshape := ReshapeRectangle;
              END;

<* INLINE *> PROCEDURE RectangleSW (rectangle: Rectangle; v: V := NIL):
  R2.T =
  BEGIN
    RETURN PosLocked(rectangle, v)
  END RectangleSW;

<* INLINE *> PROCEDURE RectangleNE (rectangle: Rectangle; v: V := NIL):
  R2.T =
  BEGIN
    LOCK v.mu DO RETURN RectangleNELocked(rectangle, v); END;
  END RectangleNE;

<* INLINE *> PROCEDURE RectangleSWLocked (rectangle: Rectangle; v: V):
  R2.T =
  BEGIN
    RETURN PosLocked(rectangle, v)
  END RectangleSWLocked;

<* INLINE *> PROCEDURE RectangleNELocked (rectangle: Rectangle;
                                          <* UNUSED *> v: V := NIL): R2.T =
  BEGIN
    RETURN Matrix2D.Transform(rectangle.m, rectangle.ne);
  END RectangleNELocked;

PROCEDURE InitRectangle (         t               : Rectangle;
                         READONLY corner1, corner2: R2.T;
                                  v               : V           := NIL;
                                  group           : Group       := NIL  ):
  Rectangle =
  VAR bounds := R2Box.FromCorners(corner1, corner2);
  BEGIN
    EVAL T.init(t, v, group);
    t.m := Matrix2D.Translate(bounds[0].lo, bounds[1].lo);
    t.ne := R2Box.Size(bounds);
    RETURN t;
  END InitRectangle;

PROCEDURE RenderRectangle (rectangle: Rectangle; v: V; VAR clip: Rect.T) =
  BEGIN
    IF rectangle.visible <= 0.0 THEN RETURN END;

    VAR
      bounds := MGCToScreenRect(v, PosLocked(rectangle),
                                      RectangleNELocked(rectangle));
    BEGIN
      IF rectangle.weight > 0.0 THEN
        VAR
          border_pixels := BorderInPixels(v, rectangle);
        BEGIN
          IF border_pixels > 0 THEN
            VBT.PaintTint(v.paintVBT, Rect.Meet(clip, bounds), rectangle.color.fg);
            bounds := Rect.Inset(bounds, border_pixels);
          END;
        END;
      END;

      VAR
        clipped := Rect.Meet(clip, bounds);
      BEGIN
        VBT.PaintTint(v.paintVBT, clipped, rectangle.color.bg);
        PaintLabel(rectangle, v, clipped, bounds);
      END;
    END;
  END RenderRectangle;

PROCEDURE BoundsRectangle (rectangle: Rectangle; v: V): R2Box.T =
  BEGIN
    RETURN R2Box.Inset(R2Box.FromCorners(PosLocked(rectangle, v),
                                         RectangleNELocked(rectangle, v)),
                       -rectangle.weight);
  END BoundsRectangle;

PROCEDURE RgnRectangle (rectangle: Rectangle; v: V): Region.T =
  BEGIN
    RETURN
      RegionWithLabel(rectangle, v,
                      Rect.Inset(MGCToScreenRectNonEmpty(v, PosLocked(rectangle),
                                                 RectangleNELocked(rectangle)),
                                 -BorderInPixels(v, rectangle)));
  END RgnRectangle;

PROCEDURE ReshapeRectangle (         rectangle       : Rectangle;
                                     v               : V;
                            READONLY corner1, corner2: R2.T       ) =
  VAR bounds := R2Box.FromCorners(corner1, corner2);
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, rectangle.appearance.region(rectangle, v).r);
    SetPosLocked(rectangle, R2Box.SouthWest(bounds));
    rectangle.ne := R2Box.Size(bounds);
    rectangle.dirty := TRUE;
  END ReshapeRectangle;

PROCEDURE SetRectangleReshape (rect: Rectangle; v: V; READONLY c1, c2: R2.T) =
  BEGIN
    LOCK v.mu DO rect.reshape(v, c1, c2); END;
  END SetRectangleReshape;

REVEAL
  Ellipse = EllipsePublic BRANDED OBJECT
            OVERRIDES
              init    := InitEllipse;
              render  := RenderEllipse;
              bounds  := BoundsEllipse;
              rgn     := RgnEllipse;
              reshape := ReshapeEllipse;
            END;

<* INLINE *> PROCEDURE EllipseSW (ellipse: Ellipse; v: V := NIL): R2.T =
  BEGIN
    RETURN PosLocked(ellipse, v)
  END EllipseSW;

<* INLINE *> PROCEDURE EllipseNE (ellipse: Ellipse; v: V := NIL): R2.T =
  BEGIN
    LOCK v.mu DO RETURN EllipseNELocked(ellipse, v); END;
  END EllipseNE;

<* INLINE *> PROCEDURE EllipseSWLocked (ellipse: Ellipse; v: V): R2.T =
  BEGIN
    RETURN PosLocked(ellipse, v)
  END EllipseSWLocked;

<* INLINE *>
  PROCEDURE EllipseNELocked (ellipse: Ellipse; <* UNUSED *> v: V := NIL):
  R2.T =
  BEGIN
    RETURN Matrix2D.Transform(ellipse.m, ellipse.ne);
  END EllipseNELocked;

PROCEDURE InitEllipse (         ellipse         : Ellipse;
                       READONLY corner1, corner2: R2.T;
                                v               : V         := NIL;
                                group           : Group     := NIL  ):
  Ellipse =
  VAR bounds := R2Box.FromCorners(corner1, corner2);
  BEGIN
    EVAL T.init(ellipse, v, group);
    ellipse.m := Matrix2D.Translate(bounds[0].lo, bounds[1].lo);
    ellipse.ne := R2Box.Size(bounds);
    RETURN ellipse;
  END InitEllipse;

PROCEDURE RenderEllipse (ellipse: Ellipse; v: V; VAR clip: Rect.T) =
  BEGIN
    IF ellipse.visible <= 0.0 THEN RETURN END;

    VAR
      bounds := MGCToStableScreenRect(
                  v, PosLocked(ellipse), EllipseNELocked(ellipse));
      delta         := Rect.Middle(bounds);
      width         := Rect.HorSize(bounds);
      height        := Rect.VerSize(bounds);
      border_pixels := 0;
    BEGIN
      IF ellipse.weight > 0.0 THEN
        border_pixels := BorderInPixels(v, ellipse);
        IF border_pixels > 0 THEN
          VBT.PaintPixmap(v.paintVBT, clip, ellipse.color.transparentFg,
                          circleCache.create(
                            width, height, border_pixels, fill := FALSE),
                          delta := delta);
        END;
      END;

      VBT.PaintPixmap(v.paintVBT, clip, ellipse.color.transparentBg,
                      circleCache.create(
                        MAX(0, width - 2 * border_pixels),
                        MAX(0, height - 2 * border_pixels), border := 0,
                        fill := TRUE), delta := delta);

      PaintLabel(ellipse, v, clip, bounds);
    END;
  END RenderEllipse;

PROCEDURE BoundsEllipse (ellipse: Ellipse; <* UNUSED *> v: V): R2Box.T =
  BEGIN
    RETURN R2Box.Inset(R2Box.FromCorners(PosLocked(ellipse),
                                         EllipseNELocked(ellipse)),
                       -ellipse.weight);
  END BoundsEllipse;

PROCEDURE RgnEllipse (ellipse: Ellipse; v: V): Region.T =
  BEGIN
    RETURN
      RegionWithLabel(ellipse, v, Rect.Inset(MGCToStableScreenRect(
                                               v, PosLocked(ellipse),
                                               EllipseNELocked(ellipse)),
                                             -BorderInPixels(v, ellipse)));
  END RgnEllipse;

PROCEDURE ReshapeEllipse (         ellipse         : Ellipse;
                                   v               : V;
                          READONLY corner1, corner2: R2.T     ) =
  VAR bounds := R2Box.FromCorners(corner1, corner2);
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, ellipse.appearance.region(ellipse, v).r);
    SetPosLocked(ellipse, R2Box.SouthWest(bounds));
    ellipse.ne := R2Box.Size(bounds);
    ellipse.dirty := TRUE;
  END ReshapeEllipse;

REVEAL
  Shape = ShapePublic BRANDED OBJECT
            path         : R2Path.T := NIL;
            fill                      := TRUE;
            realBounds                := R2Box.Empty;
            integerBounds             := Rect.Empty;
            lastMatrix                := Matrix2D.Identity;
            lastNW                    := R2.Origin;
            lastRes                   := ARRAY Axis.T OF REAL{0.0, 0.0};
            filled_pixmap, stroke_pixmap           := Pixmap.Solid;
            invalid                                := TRUE;
            integerPath                 : Path.T;  (* !!!  SCG 29 July *)
          OVERRIDES
            init    := InitShape;
            render  := RenderShape;
            bounds  := BoundsShape;
            rgn     := RgnShape;
            reshape := ReshapeShape;
          END;

PROCEDURE InitShape (         shape : Shape;
                     READONLY origin: R2.T;
                     READONLY path  : R2Path.T;
                     READONLY fill                 := TRUE;
                              v     : V            := NIL;
                              group : Group        := NIL   ): Shape =
  BEGIN
    shape.m := Matrix2D.Translate(origin[0], origin[1]);
    shape.path := path;
    shape.fill := fill;
    shape.invalid := TRUE;
    EVAL T.init(shape, v, group); (* must be after shape path is set *)
    RETURN shape;
  END InitShape;

<* INLINE *> PROCEDURE ShapeOrigin (shape: Shape; v: V): R2.T =
  BEGIN
    RETURN ShapeOriginLocked(shape, v)
  END ShapeOrigin;

<* INLINE *> PROCEDURE ShapePath (shape: Shape; v: V := NIL): R2Path.T =
  BEGIN
    LOCK v.mu DO RETURN ShapePathLocked(shape, v); END;
  END ShapePath;

<* INLINE *> PROCEDURE ShapeOriginLocked (shape: Shape; v: V := NIL):
  R2.T =
  BEGIN
    RETURN PosLocked(shape, v)
  END ShapeOriginLocked;

<* INLINE *>
  PROCEDURE ShapePathLocked (shape: Shape; <* UNUSED *> v: V := NIL):
  R2Path.T =
  BEGIN
    RETURN shape.path;
  END ShapePathLocked;

(*| Pixmap caching version
PROCEDURE ValidateShape (shape: Shape; v: V) =
  BEGIN
    IF shape.invalid
         OR NOT EquivalentRotationAndScaling(shape.lastMatrix, shape.m)
         OR v.nw # shape.lastNW OR VBT.ScreenTypeOf(v).res # shape.lastRes THEN
      VAR
        border_pixels := BorderInPixels(v, shape);
        origin_matrix := shape.m;
        integer_path: Path.T;
      BEGIN
        origin_matrix[4] := 0.0;
        origin_matrix[5] := 0.0;
        shape.realBounds := shape.path.bbox(origin_matrix);
        integer_path := R2PathExtra.R2PathToPath(v, shape.path,
                                                 origin_matrix);

        (* shape.integerBounds are the bounds of the original path *)
        shape.integerBounds := R2PathExtra.PathBounds(integer_path);
        (* locate shape.integerPath at the origin.  This is because the
           v.shapeVBT must be origin aligned (it's installed off-screen)
           and we cannot easily translate a Path.T on the fly (in
           VBT.Fill/Stroke). *)
        integer_path :=
          R2PathExtra.TranslatePath(
            integer_path, Point.Minus(Rect.NorthWest(shape.integerBounds)));

        shape.lastMatrix := origin_matrix;
        shape.lastNW := v.nw;
        shape.lastRes := VBT.ScreenTypeOf(v).res;

        (* Now create the pixmaps for the filled and unfilled paths aligned
           to the origin *)
        shape.filled_pixmap := ShapePixmap.New(integer_path, 0, fill := TRUE);
        shape.stroke_pixmap :=
          ShapePixmap.New(integer_path, border_pixels, fill := FALSE);

        shape.invalid := FALSE;
      END;
    END;
  END ValidateShape;

*)

(* Here we cache some information about a shape.  The pixmaps and bounds
   are normalized to the origin. *)
PROCEDURE ValidateShape (shape: Shape; v: V) =
  VAR st := VBT.ScreenTypeOf(v);
  BEGIN
    IF (st # NIL) AND (shape.invalid OR (shape.m # shape.lastMatrix)
                         OR (st.res # shape.lastRes)) THEN
      shape.realBounds := shape.path.bbox(shape.m);
      shape.integerPath :=
        R2PathExtra.R2PathToPath(v, shape.path, shape.m);
      shape.integerBounds := R2PathExtra.PathBounds(shape.integerPath);
      shape.lastMatrix := shape.m;
      shape.lastNW := v.nw;
      shape.lastRes := st.res;
      shape.invalid := FALSE;
    END;
  END ValidateShape;

(* This is a convenience procedure for calculating the shape's current
   bounds based on the cached shape's and the current transformation
   matrix. *)
PROCEDURE GetShapeRealBounds (shape: Shape): R2Box.T =
  BEGIN
    RETURN R2Box.Translate(shape.realBounds, R2.T{shape.m[4], shape.m[5]});
  END GetShapeRealBounds;

PROCEDURE RenderShape (shape: Shape; v: V; VAR clip: Rect.T) =
  BEGIN
    IF shape.visible <= 0.0 THEN RETURN END;

    ValidateShape(shape, v);

    WITH realBounds = GetShapeRealBounds(shape),
         bounds = MGCToScreenRectNonEmpty(v, R2Box.SouthWest(realBounds),
                                  R2Box.NorthEast(realBounds)),
         borderPixels = BorderInPixels(v, shape) DO

      IF shape.fill THEN
        VBT.Fill(v.paintVBT, clip, shape.integerPath,
                 wind := VBT.WindingCondition.Odd,
                 op := shape.color.transparentBg);
        IF shape.weight > 0.0 THEN
          VBT.Stroke(
            v.paintVBT, clip, shape.integerPath, width := borderPixels,
            op := shape.color.transparentFg);
        END;
      ELSIF shape.weight > 0.0 THEN
        VBT.Stroke(v.paintVBT, clip, shape.integerPath,
                   width := borderPixels, op := shape.color.transparentFg);
      END;

      (*| !!! SCG 29 July
          WITH delta = Rect.NorthWest(bounds) DO
            IF shape.fill THEN
              VBT.PaintPixmap(
                v.paintVBT, clip, shape.color.transparentBg,
                shape.filled_pixmap,
                delta := delta);
              IF shape.weight > 0.0 THEN
                VBT.PaintPixmap(v.paintVBT, clip, shape.color.transparentFg,
                                shape.stroke_pixmap,
                                delta := delta);
              END;
            ELSE
              VBT.PaintPixmap(v.paintVBT, clip, shape.color.transparentBg,
                              shape.stroke_pixmap,
                              delta := delta);
            END;
          END;
      !!! *)

      PaintLabel(shape, v, clip, bounds);
    END;
  END RenderShape;

PROCEDURE BoundsShape (shape: Shape; v: V): R2Box.T =
  BEGIN
    ValidateShape(shape, v);

    RETURN R2Box.Inset(GetShapeRealBounds(shape), -shape.weight);
  END BoundsShape;

PROCEDURE RgnShape (shape: Shape; v: V): Region.T =
  BEGIN
    (* Done in BoundsShape ValidateShape(shape, v); *)
    (* !!!  SCG 28 July WITH realBounds = BoundsShape(shape, v) DO RETURN
       RegionWithLabel(shape, v, MGCToScreenRectNonEmpty(v,
       R2Box.SouthWest(realBounds), R2Box.NorthEast(realBounds))); END; *)
    ValidateShape(shape, v);
    RETURN RegionWithLabel(shape, v, Rect.Inset(shape.integerBounds,
                                                -BorderInPixels(v, shape)))
  END RgnShape;

PROCEDURE ReshapeShape (         shape : Shape;
                                 v     : V;
                        READONLY origin: R2.T;
                        READONLY path  : R2Path.T;
                        READONLY fill := TRUE) =
  BEGIN
    ValidateShape(shape, v);

    v.dirtyRegion := Rect.Join(v.dirtyRegion,
                               shape.appearance.region(shape, v).r);
    SetPosLocked(shape, origin);
    shape.path := path;
    shape.fill := fill;
    shape.invalid := TRUE;
    shape.dirty := TRUE;
  END ReshapeShape;

REVEAL
  Label = LabelPublic BRANDED OBJECT
          OVERRIDES
            render := RenderLabel;
            bounds := BoundsLabel;
            rgn    := RgnLabel;
          END;

PROCEDURE RenderLabel (label: Label; v: V; VAR clip: Rect.T) = <* LL = v.mu *>
  BEGIN
    IF label.visible > 0.0 THEN
      PaintLabel(label, v, clip,
                 Rect.FromPoint(MGCToScreenPoint(v, PosLocked(label))));
    END;
  END RenderLabel;

PROCEDURE BoundsLabel (l: Label; <* UNUSED *> v: V): R2Box.T =
  VAR
    pos := PosLocked(l);
    w := pos[0];
    e := w + 1.0;
    s := pos[1];
    n := s + 1.0;
  BEGIN
    RETURN R2Box.T{Fuzzy.T{w, e}, Fuzzy.T{s, n}};
  END BoundsLabel;

PROCEDURE RgnLabel (label: Label; v: V): Region.T =
  BEGIN
    RETURN
      Region.FromRect(
        LabelRect(
          label, v, Rect.FromPoint(MGCToScreenPoint(v, PosLocked(label)))));
  END RgnLabel;

(* !!!  Compare old values before setting dirty bit?  !!!*)

PROCEDURE SetColorT (self: T; <* UNUSED *> v: V; color: PaintOp.ColorScheme) =
  BEGIN
    IF color = NIL THEN color := PaintOp.bgFg END;
    self.color := color;
    self.dirty := TRUE;
  END SetColorT;

PROCEDURE SetFontT (self: T; v: V; font: Font.T := DefaultFont) =
  BEGIN
    IF font = DefaultFont THEN font := RealDefaultFont END;
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.font := font;
    self.dirty := TRUE;
  END SetFontT;

PROCEDURE SetHighlightT (self: T; v: V; highlight := 0.0) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.highlight := highlight;
    self.dirty := TRUE;
  END SetHighlightT;

PROCEDURE SetVisibleT (self: T; v: V; visible := 1.0) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.visible := visible;
    self.dirty := TRUE;
  END SetVisibleT;

PROCEDURE SetWeightT (self: T; v: V; weight := 1.0) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.weight := weight;
    self.dirty := TRUE;
  END SetWeightT;

PROCEDURE SetAlignmentT (self: T; v: V; alignment := Alignment.Center) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.alignment := alignment;
    self.dirty := TRUE;
  END SetAlignmentT;

PROCEDURE SetLabelT (self: T; v: V; label := "") =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    self.label := label;
    self.dirty := TRUE;
  END SetLabelT;

PROCEDURE SetAppearance (self: T; v: V; appearance: Appearance := NIL) =
  BEGIN
    v.dirtyRegion :=
      Rect.Join(v.dirtyRegion, self.appearance.region(self, v).r);
    IF appearance = NIL THEN
      appearance := appearanceDefault;
    ELSE
      appearance.prevAppearance := self.appearance;
    END;
    self.appearance := appearance;
    self.dirty := TRUE;
  END SetAppearance;

PROCEDURE SetColor (t: T; v: V; color: PaintOp.ColorScheme) =
  BEGIN
    IF color = NIL THEN color := PaintOp.bgFg END;
    LOCK v.mu DO t.setColor(v, color) END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetColor;

PROCEDURE SetFont (t: T; v: V; font: Font.T) =
  BEGIN
    LOCK v.mu DO t.setFont(v, font) END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetFont;

PROCEDURE SetHighlight (t: T; v: V; highlight := 1.0) =
  BEGIN
    LOCK v.mu DO t.setHighlight(v, highlight) END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetHighlight;

PROCEDURE SetVisible (t: T; v: V; visible := 1.0) =
  BEGIN
    LOCK v.mu DO t.setVisible(v, visible); END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetVisible;

PROCEDURE SetWeight (t: T; v: V; weight := 1.0) =
  BEGIN
    LOCK v.mu DO t.setWeight(v, weight); END;
    v.mgRedisplay(Region.Empty);
  END SetWeight;

PROCEDURE SetAlignment (t: T; v: V; alignment := Alignment.Center) =
  BEGIN
    LOCK v.mu DO t.setAlignment(v, alignment) END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetAlignment;

PROCEDURE SetLabel (t: T; v: V; label := "") =
  BEGIN
    LOCK v.mu DO t.setLabel(v, label) END;
    IF v.nonOverlappingElements THEN
      VAR r := Rect.Full; BEGIN t.appearance.paint(t, v, r) END;
    ELSE
      v.mgRedisplay(Region.Empty);
    END;
  END SetLabel;

PROCEDURE Scale (         t     : T;
                          v     : V;
                 READONLY factor      := R2.Ones;
                 READONLY wrt         := R2.Origin) =
  BEGIN
    LOCK v.mu DO ScaleLocked(t, v, factor, wrt); END;
    v.mgRedisplay(Region.Empty);
  END Scale;

PROCEDURE ScaleLocked (         t     : T;
                                v     : V;
                       READONLY factor      := R2.Ones;
                       READONLY wrt         := R2.Origin) =
  BEGIN
    t.transform(v, Matrix2D.Concat3(Matrix2D.Translate(-wrt[0], -wrt[1]),
                                    Matrix2D.Scale(factor[0], factor[1]),
                                    Matrix2D.Translate(wrt[0], wrt[1])))
  END ScaleLocked;

PROCEDURE BoundingBox (t: T; v: V): R2Box.T =
  BEGIN
    LOCK v.mu DO RETURN BoundingBoxLocked(t, v); END
  END BoundingBox;

<* INLINE *> PROCEDURE BoundingBoxLocked (t: T; v: V): R2Box.T =
  BEGIN
    RETURN t.appearance.boundingBox(t, v)
  END BoundingBoxLocked;

PROCEDURE Transform (t: T; v: V; READONLY m: Matrix2D.T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    LOCK v.mu DO t.transform(v, m) END;
    v.mgRedisplay(Region.Empty);
  END Transform;

PROCEDURE TransformLocked (t: T; v: V; READONLY m: Matrix2D.T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    t.transform(v, m);
  END TransformLocked;

PROCEDURE RTranslate (t: T; v: V; READONLY by: R2.T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    LOCK v.mu DO t.transform(v, Matrix2D.Translate(by[0], by[1])) END;
    v.mgRedisplay(Region.Empty);
  END RTranslate;

PROCEDURE RTranslateLocked (t: T; v: V; READONLY by: R2.T) =
  BEGIN
    IF t = NIL THEN RETURN END;
    t.transform(v, Matrix2D.Translate(by[0], by[1]))
  END RTranslateLocked;

PROCEDURE TranslateTo (t: T; v: V; READONLY dest: R2.T; center := FALSE) =
  BEGIN
    IF t = NIL THEN RETURN END;
    LOCK v.mu DO
      TranslateToLocked(t, v, dest, center);
    END;
    v.mgRedisplay(Region.Empty);
  END TranslateTo;

PROCEDURE TranslateToLocked (         t     : T;
                                      v     : V;
                             READONLY dest  : R2.T;
                                      center         := FALSE) =
  BEGIN
    IF t = NIL THEN RETURN END;

    IF center THEN
      VAR
	bounds := t.bounds(v);
      BEGIN
	t.transform(v, Matrix2D.Translate(dest[0] - (bounds[0].lo + bounds[0].hi) / 2.0,
					  dest[1] - (bounds[1].lo + bounds[1].hi) / 2.0));
      END;
    ELSE
      VAR
	pos := PosLocked(t, v);
      BEGIN
	t.transform(
	    v, Matrix2D.Translate(dest[0] - pos[0], dest[1] - pos[1]));
      END;
    END;
  END TranslateToLocked;

PROCEDURE Rotate(t: T; v: V; angle: REAL; READONLY origin:= R2.Origin) =
  BEGIN
    LOCK v.mu DO RotateLocked(t, v, angle, origin) END;
    v.mgRedisplay(Region.Empty);
  END Rotate;

CONST
  DToR = Math.Pi / 180.0;

PROCEDURE RotateLocked (         t     : T;
                                 v     : V;
                                 angle : REAL;
                        READONLY origin         := R2.Origin) =
  BEGIN
    IF t = NIL THEN RETURN END;
    t.transform(v, Matrix2D.Concat3(
                     Matrix2D.Translate(-origin[0], -origin[1]),
                     Matrix2D.Rotate(DToR * angle),
                     Matrix2D.Translate(origin[0], origin[1])));
  END RotateLocked;

PROCEDURE PaintOpFromRGB(rgb: Color.T; mode: PaintOp.Mode): PaintOp.T =
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, mode);
  END PaintOpFromRGB;

PROCEDURE OpFromName (name: TEXT; mode := PaintOp.Mode.Accurate): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  BEGIN
    RETURN PaintOpFromRGB(ColorName.ToRGB(name), mode := mode);
  END OpFromName;

PROCEDURE ColorFromText (bg, fg: TEXT := NIL; mode := PaintOp.Mode.Accurate):
  PaintOp.ColorScheme =
  VAR bgOp, fgOp: PaintOp.T;
  <* FATAL ColorName.NotFound *>
  BEGIN
    IF bg = NIL THEN
      bgOp := PaintOp.Bg;
    ELSE
      bgOp := PaintOpFromRGB(ColorName.ToRGB(bg), mode := mode);
    END;
    IF fg = NIL THEN
      fgOp := PaintOp.Fg;
    ELSE
      fgOp := PaintOpFromRGB(
                ColorName.ToRGB(fg), mode := mode);
    END;
    RETURN PaintOp.MakeColorScheme(bgOp, fgOp)
  END ColorFromText;

PROCEDURE ColorFromRGB (bg, fg: Color.T; mode := PaintOp.Mode.Accurate):
  PaintOp.ColorScheme =
  VAR bgOp, fgOp: PaintOp.T;
  BEGIN
    bgOp := PaintOpFromRGB(bg, mode := mode);
    fgOp := PaintOpFromRGB(fg, mode := mode);
    RETURN PaintOp.MakeColorScheme(bgOp, fgOp)
  END ColorFromRGB;

REVEAL
  Appearance = AppearancePublic BRANDED OBJECT
  OVERRIDES
    paint := AppPaintDefault;
    boundingBox := AppBBDefault;
    region := AppRegionDefault;
  END;
  AppearanceDefault = Appearance BRANDED OBJECT END;

PROCEDURE AppPaintDefault (         app : Appearance;
                                    t   : T;
                                    v   : V;
                           VAR clip: Rect.T             ) =
  BEGIN
    IF app.prevAppearance = NIL THEN
      t.render(v, clip);
    ELSE
      app.prevAppearance.paint(t, v, clip);
    END;
  END AppPaintDefault;

PROCEDURE AppBBDefault (app: Appearance; t: T; v: V): R2Box.T =
  BEGIN
    IF app.prevAppearance = NIL THEN
      RETURN t.bounds(v);
    ELSE
      RETURN app.prevAppearance.boundingBox(t, v);
    END;
  END AppBBDefault;

PROCEDURE AppRegionDefault (app: Appearance; t: T; v: V): Region.T =
  BEGIN
    IF app.prevAppearance = NIL THEN
      RETURN t.rgn(v);
    ELSE
      RETURN app.prevAppearance.region(t, v);
    END;
  END AppRegionDefault;

BEGIN
  defaultColor := PaintOp.bgFg;
  appearanceDefault := NEW(AppearanceDefault);
END MG.
