(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb 29 12:13:14 PST 1996 by mhb     *)
(*      modified on Mon Jan 30 14:50:50 PST 1995 by kalsow  *)
(*      modified on Mon Oct 11 15:41:54 PDT 1993 by steveg  *)
(*      modified on Wed Jun  2 16:53:18 PDT 1993 by meehan  *)
(*      modified on Wed Aug 12 17:19:05 PDT 1992 by guarino *)
(*      modified on Tue Jun 16 13:08:03 PDT 1992 by muller *)

MODULE ViewportVBT;

(*

Viewports have gotten kind of hairy, so I'll document them.

A Viewport is a cross of an HVSplit and JoinedVBT with scroll bars
and Multis thrown in.  

The structure is as follows:

There is a single child provided by the client.  It is the Multi child
of the Viewport filter.

The child is wrapped in a JoinedVBT so that it can support multiple views
(handled by the join's parents).

The viewport consists of multiple views.  Each view consists of a 
single join parent glued together with scroll bars.  Logically, each
view is a filter for the join parent.

A view's structure is as follows (depending on the scroll bars):

horizontal and vertical scroll bars:
(ViewRoot (HVSplitReshape (HSplit VScroller Bar) JoinParent) 
          Bar 
          (MyHSplit Reset Bar HScroller))

horizontal scroll bars
(ViewRoot JoinParent Bar (HSplit Reset Bar HScroller))

vertical scroll bars:
(ViewRoot (HVSplitReshape (HSplit VScroller Bar) JoinParent))

no scroll bars:
(ViewRoot JoinParent)

*)

IMPORT Axis, Filter, FilterClass, FlexVBT, HVBar, HVSplit,
       JoinedVBT, JoinParent, MultiClass, OffsetVBT, PaintOp,
       Pixmap, Point, Rect, Region, ScrollerVBT,
       ScrollerVBTClass, Shadow, ShadowedFeedbackVBT, Split,
       SwitchVBT, TextVBT, TextureVBT, Thread, VBT, VBTClass,
       VBTRep;

TYPE
  Views = REF ARRAY OF
                RECORD
                  offset                     := Point.Origin;
                  viewRoot  : ViewRoot;
                  hscroller : Scroller;
                  vscroller : Scroller;
                  joinParent: MyJoinParent;
                  offsetVBT : OffsetVBT.T;
                END;
(* viewRoot = NIL => unoccupied/removed view *)

REVEAL
  T = Public BRANDED "ViewportVBT.T" OBJECT
        multiChild     : VBT.T;
        views          : Views;
        join           : JoinedVBT.T;
        shadow         : Shadow.T;
        step           : CARDINAL;
        adjustableViews: BOOLEAN;
        (* TRUE => there is an HVBar between views for adjusting their
           height *)
        scrollStyle: ScrollStyle;
        shapeStyle : ShapeStyle;
      OVERRIDES
        reshape   := ReshapeT;
        init      := Init;
      END;

TYPE
  MC = MultiClass.Filter BRANDED "ViewportVBT.MC" OBJECT
       OVERRIDES
         pred    := Succ;
         succ    := Succ;
         replace := Replace;
       END;

PROCEDURE Init (v              : T;
                ch             : VBT.T;
                axis                      := Axis.T.Ver;
                shadow         : Shadow.T := NIL;
                step           : CARDINAL := 10;
                adjustableViews           := TRUE;
                scrollStyle     := ScrollStyle.AlaViewport;
                shapeStyle      := ShapeStyle.Related): T = 
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None END;
    MultiClass.Be (v, NEW(MC));
    MultiClass.BeChild (v, ch);
    EVAL HVSplit.T.init (v, axis);
    v.join := JoinedVBT.New (NEW (JoinChild).init (v, ch));
    v.views := NEW (Views, 0);
    v.shadow := shadow;
    v.step := step;
    v.adjustableViews := adjustableViews;
    v.multiChild := ch;
    IF scrollStyle = ScrollStyle.AlaViewport THEN
      IF axis = Axis.T.Hor THEN
        scrollStyle := ScrollStyle.HorOnly;
      ELSE
        scrollStyle := ScrollStyle.VerOnly;
      END;
    END;
    v.scrollStyle := scrollStyle;
    v.shapeStyle := shapeStyle;
    EVAL AddView (v, -1);
    RETURN v
  END Init;

PROCEDURE ReshapeT (v: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    sr: VBT.SizeRange;
    scrollerSize := ScrollerSize(v, Axis.Other[HVSplit.AxisOf(v)]);
  BEGIN
    IF v.shapeStyle = ShapeStyle.Related THEN
      IF HVSplit.AxisOf(v) = Axis.T.Ver THEN
        WITH n = MAX(0, Rect.HorSize(cd.new) - scrollerSize) DO
          sr := v.multiChild.shape(Axis.T.Ver, n);
          VBTClass.Reshape(v.join, Rect.FromSize(n, sr.pref), Rect.Empty);
        END;
      ELSE
        WITH n = MAX(0, Rect.HorSize(cd.new) - scrollerSize) DO
          sr := v.multiChild.shape(Axis.T.Hor, n);
          VBTClass.Reshape(v.join, Rect.FromSize(sr.pref, n), Rect.Empty);
        END;
      END;
    END;
    HVSplit.T.reshape(v, cd);
  END ReshapeT;

(* Return the size of a scroller and a bar (if any) *)
PROCEDURE ScrollerSize (v: T; ax: Axis.T; bothAxes := FALSE): INTEGER =
  VAR
    sr     : VBT.SizeRange;
    barSize                := BarSize(v, ax);
  BEGIN
    IF (bothAxes OR (HVSplit.AxisOf(v) # ax)) AND NUMBER(v.views^) > 0 THEN
      WITH vv = v.views[0] DO
        (* return the size of the scroller (if any) and bar *)
        IF ax = Axis.T.Hor AND vv.vscroller # NIL THEN
          sr := VBTClass.GetShape(vv.vscroller.parent, ax, 0)
        ELSIF ax = Axis.T.Ver AND vv.hscroller # NIL THEN
          sr := VBTClass.GetShape(vv.hscroller.parent, ax, 0)
        ELSE
          barSize := 0;
        END;
        RETURN sr.pref + barSize;
      END;
    END;
    RETURN 0;
  END ScrollerSize;

(* --------------------- JoinChild ------------------ *)

TYPE
  JoinChild = Filter.T OBJECT
                vp: T;
              METHODS
                init (vp: T; ch: VBT.T): JoinChild := InitJoinChild;
              OVERRIDES
                shape := ShapeJoinChild;
              END;

PROCEDURE InitJoinChild (v: JoinChild; vp: T; ch: VBT.T): JoinChild =
  BEGIN
    v.vp := vp;
    RETURN Filter.T.init(v, ch);
  END InitJoinChild;

PROCEDURE ShapeJoinChild (v: JoinChild; ax: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    IF v.vp # NIL AND v.vp.shapeStyle = ShapeStyle.Related
         AND ax # HVSplit.AxisOf(v.vp) THEN
      WITH pref = MAX(
                    0, Rect.Size(ax, v.vp.domain) - ScrollerSize(v.vp, ax)) DO
        RETURN VBT.SizeRange{lo := pref, pref := pref, hi := pref + 1}
      END;
    ELSE
      VAR sz: VBT.SizeRange := Filter.T.shape(v, ax, n);
      BEGIN
        RETURN sz
      END;
    END;
  END ShapeJoinChild;

(* ----------------- Scrollers ------------------------- *)

PROCEDURE ScrollerGet (v: ScrollerVBT.T): INTEGER =
  BEGIN
    IF v # NIL THEN RETURN ScrollerVBT.Get(v) ELSE RETURN 0 END;
  END ScrollerGet;

PROCEDURE ScrollerPut (v: ScrollerVBT.T; i: INTEGER) =
  BEGIN
    IF v # NIL THEN ScrollerVBT.Put(v, i) END;
  END ScrollerPut;

PROCEDURE ScrollerPutBounds (v       : ScrollerVBT.T;
                             min, max: INTEGER;
                             thumb   : CARDINAL              ) =
  BEGIN
    IF v # NIL THEN ScrollerVBT.PutBounds(v, min, max, thumb) END;
  END ScrollerPutBounds;

PROCEDURE ScrollTo (                      v    : T;
                                 READONLY r    : Rect.T;
                                          i    : INTEGER  := 0;
                    <* UNUSED *>          force: BOOLEAN  := TRUE) =

  PROCEDURE NewOffset (sb: ScrollerVBT.T; lo, hi: INTEGER): INTEGER =
    VAR val, min, max, thumb: INTEGER;
    BEGIN
      IF sb = NIL THEN RETURN 0 END;
      min := ScrollerVBT.GetMin(sb);
      max := ScrollerVBT.GetMax(sb);
      thumb := ScrollerVBT.GetThumb(sb);

      (* bias range to lo portion that fits *)
      lo := MAX(lo, min);
      hi := MIN(MIN(hi, max - 1), lo + thumb - 1);

      (* put center of range at center of view *)
      val := (lo + hi) DIV 2 - thumb DIV 2;
      val := MIN(MAX(val, min), max - thumb);
      ScrollerVBT.Put(sb, val);

      RETURN val;
    END NewOffset;

  VAR
    dom   : Rect.T;
    offset: Point.T;
    vv              := v.views[i];
  BEGIN
    (*
      IF NOT force THEN
        FOR j := 0 TO LAST(v.views^) DO
          WITH vvv = v.views[j] DO
            IF vvv.viewRoot # NIL THEN
              WITH vDom = VBT.Domain(vvv.offsetVBT) DO
                IF Rect.Subset(vvv.joinParent.translate(r), vDom) THEN
                  RETURN
                END
              END
            END;
          END;
        END;
      END;
   *)
    dom := VBT.Domain(v.join);
    offset.h :=
      NewOffset(vv.hscroller, r.west - dom.west, r.east - dom.west);
    offset.v :=
      NewOffset(vv.vscroller, r.north - dom.north, r.south - dom.north);
    Move(v, i, offset);
  END ScrollTo;

PROCEDURE Normalize (v: T; w: VBT.T; i: INTEGER := 0) =
  BEGIN
    IF Rect.IsEmpty(VBT.Domain(w)) THEN
      EVAL Thread.Fork(NEW(NormalizeCl, v := v, w := w, i := i))
    ELSE
      DoNormalize(v, w, i)
    END
  END Normalize;

TYPE
  NormalizeCl = Thread.Closure OBJECT
                  v: T;
                  w: VBT.T;
                  i: INTEGER;
                OVERRIDES
                  apply := NormalizeBg
                END;

PROCEDURE NormalizeBg (arg: NormalizeCl): REFANY RAISES {} =
  BEGIN
    LOCK VBT.mu DO
      VBTRep.Redisplay();
      IF NOT Rect.IsEmpty(VBT.Domain(arg.w)) THEN
        DoNormalize(arg.v, arg.w, arg.i);
        VBTRep.Redisplay();
      END
    END;
    RETURN NIL;
  END NormalizeBg;

PROCEDURE DoNormalize (v: T; w: VBT.T; i: INTEGER) =
  BEGIN
    ScrollTo(v, VBT.Domain(w), i, FALSE)
  END DoNormalize;

(********** Views ***********)

CONST
  HasHScroller = SET OF
                   ScrollStyle{ScrollStyle.HorAndVer, ScrollStyle.HorOnly,
                               ScrollStyle.Auto};
  HasVScroller = SET OF
                   ScrollStyle{ScrollStyle.HorAndVer, ScrollStyle.VerOnly,
                               ScrollStyle.Auto};

PROCEDURE AddView (v: T; after: INTEGER := -1; split := TRUE): INTEGER =
  VAR
    cntViews                         := NUMBER(v.views^);
    reset, flexReset: VBT.T;
    h1              : HVSplitReshape;
    h2              : HVSplit.T;
    v1, v2          : VBT.T;
    iNew            : INTEGER;
    vscroll                          := v.scrollStyle IN HasVScroller;
    hscroll                          := v.scrollStyle IN HasHScroller;
  BEGIN
    <* ASSERT(after >= -1 AND after < cntViews) *>
    iNew := 0;
    WHILE iNew < cntViews AND v.views[iNew].viewRoot # NIL DO
      INC(iNew);
    END;
    IF iNew = cntViews THEN
      VAR old := v.views;
      BEGIN
        v.views := NEW(Views, cntViews + 1);
        SUBARRAY(v.views^, 0, cntViews) := SUBARRAY(old^, 0, cntViews);
      END;
    END;

    WITH vv = v.views[iNew] DO
      vv.joinParent := NEW(MyJoinParent, vp := v, view := iNew).init(v.join);
      vv.offsetVBT := NEW(MyOffset).init(vv.joinParent, bg:=v.shadow.bg);
      IF vscroll THEN
        vv.vscroller := NEW(Scroller, vp := v, view := iNew).init(
                          Axis.T.Ver, 0, 0, v.shadow, v.step);

        (* !!! Jan. 1992 compiler bug: proc call in NEW *)
        VAR
          newshapeChild := NewHSplit(vv.vscroller, Axis.T.Ver,
                                     vv.vscroller, NewBar(v.shadow));
        BEGIN
          h1 := NEW(HVSplitReshape, newshapeChild := newshapeChild).init(
                  Axis.T.Hor);
        END;
        (* !!! compiler bug *)

        Split.AddChild(h1, h1.newshapeChild, vv.offsetVBT);
        v1 := h1;
      ELSE
        v1 := vv.offsetVBT;
      END;

      IF hscroll THEN
        vv.hscroller := NEW(Scroller, vp := v, view := iNew).init(
                          Axis.T.Hor, 0, 0, v.shadow, v.step);
      END;

      IF hscroll AND vscroll THEN
        reset := NEW(ResetSwitch, vp := v, view := iNew).init(
                   NEW(ShadowedFeedbackVBT.T).init(
                     TextVBT.New("R"), Shadow.None));
        WITH a = ScrollerVBTClass.GetAttributes(vv.vscroller) DO
          flexReset := FlexVBT.FromAxis(
                         reset, Axis.T.Hor,
                         FlexVBT.RigidRange(a.stripeWidth + 2.0 * a.margin));
        END;

        h2 := NewHSplit(vv.hscroller, Axis.T.Hor, flexReset,
                        NewBar(v.shadow), vv.hscroller);
        v2 := h2;
      ELSIF hscroll THEN
        v2 := vv.hscroller;
      END;

      vv.viewRoot := NEW(ViewRoot, newshapeChild := v2,
                         vp := v).init(Axis.T.Ver);
      IF v2 = NIL THEN
        Split.AddChild(vv.viewRoot, v1)
      ELSE
        Split.AddChild(vv.viewRoot, v1, NewBar(v.shadow), v2);
      END;

      InsertView(v, vv.viewRoot, after, split);
    END;
    RETURN iNew;
  END AddView;

PROCEDURE InsertView (v: T; view: VBT.T; after: INTEGER; split: BOOLEAN) =
  VAR
    bar      : VBT.T;
    afterRoot: HVSplit.T;
    stretchyChild := NEW(VBT.Leaf); (* To make HVSplit.Adjust work, need a child with 0 pref and stretchy *)
  <* FATAL Split.NotAChild *>
  BEGIN
    IF after = -1 THEN
      Split.Insert(v, NIL, stretchyChild);
      IF v.adjustableViews AND Split.NumChildren(v) > 1 THEN
        Split.Insert(v, stretchyChild, NEW(BorderedHVBar).init());
      END;
    ELSE
      afterRoot := v.views[after].viewRoot;
      IF v.adjustableViews THEN
        bar := NEW(BorderedHVBar).init();
        Split.Insert(v, afterRoot, bar);
        Split.Insert(v, bar, stretchyChild);
      ELSE
        Split.Insert(v, afterRoot, stretchyChild);
      END;
      IF split THEN
        VAR
          min, max, adj0, adj1, adj2: INTEGER;
          pred                                := Split.Pred(v, afterRoot);
          splitDom                            := afterRoot.domain;
        BEGIN
          IF HVSplit.AxisOf(v) = Axis.T.Ver THEN
            min := v.domain.north;
            max := v.domain.south;
            adj0 := splitDom.north - min;
            adj1 := ((splitDom.south + splitDom.north) DIV 2) - min;
            adj2 := splitDom.south - min;
          ELSE
            min := v.domain.west;
            max := v.domain.east;
            adj0 := splitDom.west - min;
            adj1 := ((splitDom.east + splitDom.west) DIV 2) - min;
            adj2 := splitDom.east - min;
          END;
          IF pred # NIL THEN HVSplit.Adjust(v, pred, adj0) END;
          IF v.adjustableViews THEN HVSplit.Adjust(v, bar, adj1); END;
          HVSplit.Adjust(v, stretchyChild, adj2);
        END;
      END;
    END;
    Split.Replace(v, stretchyChild, view);
  END InsertView;

PROCEDURE RemoveView (v: T; view: INTEGER) =
  VAR
    cntViews        := NUMBER(v.views^);
    bar     : VBT.T;
  <* FATAL Split.NotAChild *>
  BEGIN
    WITH vv = v.views[view] DO
      <* ASSERT(view >= 0 AND view < cntViews) *>
      IF v.adjustableViews THEN
        bar := Split.Succ(v, vv.viewRoot);
        IF bar = NIL THEN bar := Split.Pred(v, vv.viewRoot); END;
        IF bar # NIL THEN Split.Delete(v, bar); VBT.Discard(bar); END;
      END;

      Split.Delete(v, vv.viewRoot);
      JoinParent.Rem(vv.joinParent);
      VBT.Discard(vv.viewRoot);

      vv.viewRoot := NIL;
      vv.hscroller := NIL;
      vv.vscroller := NIL;
      vv.joinParent := NIL;
      vv.offsetVBT := NIL;
    END;
  END RemoveView;

(* ---------------- View HVSplit ---------------- *)

TYPE
  HSplit = HVSplit.T OBJECT
             scroller: Scroller;
             axis    : Axis.T;
           OVERRIDES
             shape := HSplitShape;
             redisplay := HSplitRedisplay;
           END;

PROCEDURE NewHSplit (scroller     : Scroller;
                    axis         : Axis.T;
                    ch0, ch1, ch2: VBT.T      := NIL): HSplit =
  VAR
    hs := NEW(HSplit, scroller := scroller, axis := axis).init(Axis.T.Hor);
  BEGIN
    Split.AddChild(hs, ch0, ch1, ch2);
    RETURN hs;
  END NewHSplit;

PROCEDURE HSplitShape (v: HSplit; axis: Axis.T; n: CARDINAL):
  VBT.SizeRange =
  VAR
    vs := v.scroller;
    vp := vs.vp;
    vv := vp.views[vs.view];
  BEGIN
    IF vp.scrollStyle # ScrollStyle.Auto OR axis = v.axis THEN
      RETURN HVSplit.T.shape(v, axis, n);
    ELSE
      IF vv.offsetVBT # NIL AND Rect.Size(v.axis, vp.join.domain)
                               <= Rect.Size(v.axis, vv.offsetVBT.domain) THEN
        VBT.Mark(v);
        RETURN VBT.SizeRange{lo := 0, pref := 0, hi := 1};
      ELSE
        RETURN HVSplit.T.shape(v, axis, n);
      END;
    END;
  END HSplitShape;

PROCEDURE HSplitRedisplay (v: HSplit) =
  VAR
    vs := v.scroller;
    vp := vs.vp;
    vv := vp.views[vs.view];
  BEGIN
    IF vp.scrollStyle = ScrollStyle.Auto AND vv.offsetVBT # NIL
         AND Rect.Size(v.axis, vp.join.domain)
               <= Rect.Size(v.axis, vv.offsetVBT.domain) THEN
      ScrollTo(vp, vp.join.domain, vs.view);
    END;
    HVSplit.T.redisplay(v);
  END HSplitRedisplay;

(* --------------------------- HVSplit Reshape ------------- *)

TYPE
  HVSplitReshape = HVSplit.T OBJECT
                     newshapeChild: VBT.T;
                   OVERRIDES
                     reshape  := HVSplitReshapeMethod;
                     newShape := HVSplitNewshapeMethod;
                   END;

PROCEDURE HVSplitReshapeMethod (         v : HVSplitReshape;
                                READONLY cd: VBT.ReshapeRec  ) =
  BEGIN
    IF v.newshapeChild # NIL THEN VBT.NewShape(v.newshapeChild); END;
    HVSplit.T.reshape(v, cd);
  END HVSplitReshapeMethod;

TYPE
  NewshapeClosure = Thread.Closure OBJECT
                      v: VBT.T;
                    OVERRIDES
                      apply := ForkedNewshapeChild;
                    END;

PROCEDURE HVSplitNewshapeMethod (v: HVSplitReshape; ch: VBT.T) =
  BEGIN
    IF ch # v.newshapeChild AND v.newshapeChild # NIL THEN
      EVAL Thread.Fork(NEW(NewshapeClosure, v := v.newshapeChild));
    END;
    HVSplit.T.newShape(v, ch);
  END HVSplitNewshapeMethod;

PROCEDURE ForkedNewshapeChild (cl: NewshapeClosure): REFANY =
  BEGIN
    LOCK VBT.mu DO VBT.NewShape(cl.v) END;
    RETURN NIL;
  END ForkedNewshapeChild;

(* ------------------- ViewRoot ------------------- *)

TYPE
  ViewRoot = HVSplitReshape OBJECT
     vp: T;
  OVERRIDES
     axisOrder := AxisOrderView;
     shape := ShapeView;
  END;

PROCEDURE AxisOrderView (v: ViewRoot): Axis.T =
  BEGIN
    RETURN v.vp.multiChild.axisOrder()
  END AxisOrderView;

(* Use the multiChild's shape in the non-axis direction *)
PROCEDURE ShapeView (v: ViewRoot; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR
    sr          : VBT.SizeRange;
    scrollerSize: CARDINAL;
  BEGIN
    IF ax = HVSplit.AxisOf(v) THEN
      RETURN VBT.DefaultShape;
    ELSE
      scrollerSize := ScrollerSize(v.vp, ax);
      sr := v.vp.multiChild.shape(
              ax, MAX(0, n - ScrollerSize(v.vp, Axis.Other[ax], TRUE)));
      IF v.vp.shapeStyle = ShapeStyle.Related THEN
        RETURN VBT.SizeRange{sr.lo + scrollerSize, sr.pref + scrollerSize,
                             sr.hi + scrollerSize};
      ELSE
        RETURN
          VBT.SizeRange{0, sr.pref + scrollerSize,
                        MAX(sr.hi + scrollerSize, VBT.DefaultShape.hi)}
      END;
    END;
  END ShapeView;

(* -------------------- BorderedHVBar --------------------- *)

TYPE
  BorderedHVBar = HVBar.T OBJECT
                  METHODS
                    init (): BorderedHVBar := BorderedHVBarInit;
                  OVERRIDES
                    repaint := BorderedHVBarRepaint;
                    reshape := BorderedHVBarReshape;
                  END;

PROCEDURE BorderedHVBarInit (v: BorderedHVBar): BorderedHVBar =
  BEGIN
    RETURN HVBar.T.init(v)
  END BorderedHVBarInit;

PROCEDURE BorderedHVBarReshape (         v : BorderedHVBar;  <*UNUSED*>
                                READONLY cd: VBT.ReshapeRec            ) =
  BEGIN
    BorderedHVBarRepaint(v, Region.Full);
  END BorderedHVBarReshape;

PROCEDURE BorderedHVBarRepaint (         v: BorderedHVBar;
                                READONLY r: Region.T       ) =
  VAR
    dh    := ROUND(VBT.MMToPixels(v, 0.5, Axis.T.Hor));
    dv    := ROUND(VBT.MMToPixels(v, 0.5, Axis.T.Ver));
    chDom := Rect.Change(v.domain, dh, -dh, dv, -dv);
    a: Rect.Partition;
  BEGIN
    Rect.Factor(Rect.Meet(v.domain, r.r), chDom, a, 0, 0);
    a[2] := a[4];
    VBT.PolyTexture(
      v, SUBARRAY(a, 0, 4), PaintOp.Fg, Pixmap.Solid);
    VBT.PaintTexture(v, chDom, PaintOp.BgFg, Pixmap.Gray);
  END BorderedHVBarRepaint;

(********** Bar ***********)

TYPE
  Bar = TextureVBT.T OBJECT OVERRIDES shape := BarShape; END;

PROCEDURE NewBar (shadow: Shadow.T): Bar =
  BEGIN
    WITH v = NEW(Bar) DO
      EVAL TextureVBT.T.init(v, shadow.fg, Pixmap.Solid);
      RETURN v
    END
  END NewBar;

PROCEDURE BarSize(v: VBT.T; ax: Axis.T): INTEGER =
  BEGIN RETURN ROUND(VBT.MMToPixels(v, 0.5, ax)) END BarSize;

PROCEDURE BarShape (v: Bar; ax: Axis.T;  <* UNUSED *>n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    IF ax = HVSplit.AxisOf(VBT.Parent(v)) THEN
      WITH lo = BarSize(v, ax) DO
        RETURN VBT.SizeRange{lo := lo, pref := lo, hi := lo + 1}
      END
    ELSE
      RETURN VBT.DefaultShape
    END
  END BarShape;


(********** Callback for scrolllbars, reset button: ***********)

TYPE
  ResetSwitch = SwitchVBT.T OBJECT
                  vp  : T;
                  view: INTEGER
                OVERRIDES
                  callback := ResetAction
                END;

PROCEDURE ResetAction (self: ResetSwitch;
                       <* UNUSED *>
                       READONLY cd: VBT.MouseRec) =
  BEGIN
    Move(self.vp, self.view, Point.Origin)
  END ResetAction;

TYPE
  Scroller = ScrollerVBT.T OBJECT
               vp  : T;
               view: INTEGER;
             OVERRIDES
               mouse    := ScrollerMouse;
               callback := ScrollerAction
             END;

PROCEDURE CountViews (v: T): INTEGER =
  VAR cnt := 0;
  BEGIN
    FOR i := 0 TO LAST(v.views^) DO
      IF v.views[i].viewRoot # NIL THEN INC(cnt) END;
    END;
    RETURN cnt;
  END CountViews;

PROCEDURE ScrollerMouse (v: Scroller; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF VBT.Modifier.Option IN cd.modifiers THEN
      IF cd.clickType = VBT.ClickType.FirstDown THEN
        CASE cd.whatChanged OF
        | VBT.Modifier.MouseL => EVAL AddView(v.vp, v.view);
        | VBT.Modifier.MouseR =>
            IF CountViews(v.vp) > 1 THEN RemoveView(v.vp, v.view) END;
        ELSE
        END;
      END;
    ELSE
      ScrollerVBT.T.mouse(v, cd);
    END;
  END ScrollerMouse;

PROCEDURE PixelsToMM (v: VBT.T; ax: Axis.T; pix: INTEGER): REAL =
  BEGIN
    RETURN FLOAT(pix) / VBT.MMToPixels(v, 1.0, ax)
  END PixelsToMM;

PROCEDURE ScrollerAction (self: Scroller;
                          <* UNUSED *>
                          READONLY cd: VBT.MouseRec) =
  VAR vv := self.vp.views[self.view];
  BEGIN
    Move(self.vp, self.view,
         Point.T{ScrollerGet(vv.hscroller), ScrollerGet(vv.vscroller)});
  END ScrollerAction;

PROCEDURE Move (v: T; i: INTEGER; READONLY offset: Point.T) =
  BEGIN
    WITH vv = v.views[i] DO
      vv.offsetVBT.move(PixelsToMM(v, Axis.T.Ver, offset.v),
                       PixelsToMM(v, Axis.T.Hor, offset.h));
      vv.offset := offset;
      ScrollerPut(vv.hscroller, offset.h);
      ScrollerPut(vv.vscroller, offset.v);
    END;
  END Move;

(*********** OffsetVBT methods ***************)

TYPE
  (* child must be MyJoinParent *)
  MyOffset = OffsetVBT.T OBJECT OVERRIDES reshape := OffsetReshape; END;

PROCEDURE OffsetReshape (off: MyOffset; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    OffsetVBT.T.reshape(off, cd);
    EVAL AdjustShape(off.ch);
  END OffsetReshape;

(********** JoinVBTParent methods: ***********)

TYPE
  MyJoinParent = JoinParent.T OBJECT
                 vp  : T;
                 view: INTEGER;
               OVERRIDES
                 shape   := JoinParentShape;
                 reshape := JoinParentReshape;
               END;

PROCEDURE AdjustDelta (pLo, pHi, cLo, cHi, oldDelta: INTEGER): INTEGER =
  BEGIN
    IF pHi - pLo > cHi - cLo THEN
      RETURN cLo - pLo
    ELSIF cHi >= pHi + oldDelta THEN
      RETURN oldDelta
    ELSE
      RETURN cHi - pHi
    END;
  END AdjustDelta;

PROCEDURE AdjustShape (prntP: MyJoinParent):
  ARRAY Axis.T OF VBT.SizeRange =
  VAR
    shapes: ARRAY Axis.T OF VBT.SizeRange;
    offset: Point.T;
    pDom  : Rect.T;
  BEGIN
    IF prntP.parent = NIL THEN
      RETURN
        ARRAY Axis.T OF VBT.SizeRange{VBT.DefaultShape, VBT.DefaultShape};
    ELSE
      (* msm 7/26/95: shapes := VBTClass.GetShapes(JoinParent.Child(prntP)); *)
      shapes := VBTClass.GetShapes(Filter.Child(prntP));
      pDom := VBT.Domain(prntP.parent);
      WITH v  = prntP.vp,
           vv = v.views[prntP.view] DO
        ScrollerPutBounds(
          vv.hscroller, 0, shapes[Axis.T.Hor].pref, Rect.HorSize(pDom));
        ScrollerPut(vv.hscroller, vv.offset.h);
        ScrollerPutBounds(
          vv.vscroller, 0, shapes[Axis.T.Ver].pref, Rect.VerSize(pDom));
        ScrollerPut(vv.vscroller, vv.offset.v);
        offset.h := AdjustDelta(0, pDom.east - pDom.west, 0,
                                shapes[Axis.T.Hor].pref, vv.offset.h);
        offset.v := AdjustDelta(0, pDom.south - pDom.north, 0,
                                shapes[Axis.T.Ver].pref, vv.offset.v);
        Move(v, prntP.view, offset);
      END;
      RETURN shapes
    END;
  END AdjustShape;

PROCEDURE JoinParentShape (prntP: MyJoinParent;
                           axis : Axis.T;      <* UNUSED *>
                           n    : CARDINAL                 ):
  VBT.SizeRange =
  VAR sr := AdjustShape(prntP)[axis];
  BEGIN
    sr.lo := 0;
    sr.hi := MAX(sr.pref + 1, VBT.DefaultShape.hi);
    RETURN sr;
  END JoinParentShape;

PROCEDURE JoinParentReshape (prntP: MyJoinParent; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    JoinParent.T.reshape(prntP, cd);
    EVAL AdjustShape(prntP);
  END JoinParentReshape;

(********** Multi methods: ***********)

PROCEDURE Replace (m: MC; ch: VBT.T; new: VBT.T) =
  BEGIN
    WITH v = NARROW(m.vbt, T) DO
      <*ASSERT(ch = v.multiChild) *>
      EVAL Filter.Replace(v.multiChild.parent, new);
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  BEGIN
    WITH v = NARROW(m.vbt, T) DO
      IF ch = NIL THEN RETURN v.multiChild ELSE RETURN NIL END
    END
  END Succ;


(********** Global initialization: ***********)

BEGIN
END ViewportVBT.

