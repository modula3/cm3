(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Mar 10 19:08:24 1992 by steveg   *)
(*      modified on Mon Feb 24 13:53:22 PST 1992 by muller   *)
(*      modified on Sun Nov 10 18:20:43 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:31:39 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE HVBar;

IMPORT Axis, Cursor, HighlightVBT, HVSplit, Interval, PaintOp, Pixmap, Point,
  Rect, Split, TextureVBT, VBT;

REVEAL T = Public BRANDED OBJECT
     highlighter: HighlightVBT.T := NIL;
     sizeMM: REAL;
     adjusting := FALSE;
     range: Interval.T;
     offset: INTEGER;
     (* The distance from the top (or left) of the barOutline to the cursor
         position *)
     barOutline: Rect.T;
  OVERRIDES
    pre := PreDefault;
    post := PostDefault;
    during := DuringDefault;
    position := Position;
    mouse := Mouse;
    shape := Shape;
    reshape := Reshape;
    init := Be
  END;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    hv := HVSplit.AxisOf(VBT.Parent(v));
  BEGIN
    TextureVBT.T.reshape(v, cd);
    VBT.SetCursor(v, cursors[hv])
  END Reshape;

PROCEDURE Be(
  v: T;
  size: REAL;
  op: PaintOp.T;
  src: Pixmap.T): T RAISES {} =
  BEGIN
    EVAL TextureVBT.T.init(v, op, src);
    v.sizeMM := size;
    VBT.SetCursor(v, Cursor.TextPointer);
    RETURN v
  END Be;

PROCEDURE New(
    size: REAL;
    op: PaintOp.T := PaintOp.BgFg;
    texture: Pixmap.T := Pixmap.Gray)
    : T =
  BEGIN
    RETURN Be(NEW(T), size, op, texture)
  END New;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR
    hv: Axis.T;
    sr: VBT.SizeRange;
  BEGIN
    hv := HVSplit.AxisOf(VBT.Parent(v));
    IF hv = ax THEN
      sr.lo := ROUND(VBT.MMToPixels(v, v.sizeMM, hv));
      sr.pref := sr.lo;
      sr.hi := sr.lo + 1;
      RETURN sr
    ELSE
      RETURN VBT.Leaf.shape(v, ax, n)
    END
  END Shape;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  <*FATAL Split.NotAChild*>
  VAR
    parent: HVSplit.T := VBT.Parent(v);
    hv := HVSplit.AxisOf(parent);
    dom := VBT.Domain(v);
    pdom := VBT.Domain(parent);
    adjust:    BOOLEAN;
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      v.adjusting := TRUE;
      IF hv = Axis.T.Hor THEN
        v.range := Interval.Move(
          HVSplit.FeasibleRange(parent, Split.Pred(parent, v)), pdom.west);
        IF Interval.IsEmpty(v.range) THEN
          v.range := Interval.FromBound(dom.west, 1)
        END;
        v.offset := cd.cp.pt.h - dom.west;
      ELSE
        v.range := Interval.Move(
          HVSplit.FeasibleRange(parent, Split.Pred(parent, v)), pdom.north);
        IF Interval.IsEmpty(v.range) THEN
          v.range := Interval.FromBound(dom.north, 1)
        END;
        v.offset := cd.cp.pt.v - dom.north;
      END;
      v.barOutline := dom;
      v.pre(cd);
      Position2(v, cd.cp)
    ELSE
      IF v.adjusting THEN
        v.adjusting := FALSE;
        adjust := (cd.clickType = VBT.ClickType.LastUp) AND 
          NOT Rect.Equal(v.barOutline, dom);
        IF adjust THEN
          IF hv = Axis.T.Hor THEN
            HVSplit.Adjust(parent, v, v.barOutline.east - pdom.west)
          ELSE
            HVSplit.Adjust(parent, v, v.barOutline.south - pdom.north)
          END
        END;
        v.post(cd)
      END
    END
  END Mouse;

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  BEGIN Position2(v, cd.cp)  END Position;

PROCEDURE Position2(v: T; READONLY cp: VBT.CursorPosition) =
  VAR
    parent: HVSplit.T := VBT.Parent(v);
    lo: INTEGER;
  BEGIN
    IF v.adjusting AND NOT cp.offScreen THEN
      IF HVSplit.AxisOf(parent) = Axis.T.Hor THEN
        lo := MAX(MIN(cp.pt.h - v.offset, v.range.hi - 1), v.range.lo);
        v.barOutline := Rect.MoveH(v.barOutline, lo - v.barOutline.west);
        v.during(v.barOutline.west)
      ELSE
        lo := MAX(MIN(cp.pt.v - v.offset, v.range.hi - 1), v.range.lo);
        v.barOutline := Rect.MoveV(v.barOutline, lo - v.barOutline.north);
        v.during(v.barOutline.north)
      END
    END;
    IF v.adjusting THEN
      VBT.SetCage(v, VBT.CageFromPosition(cp, trackOutside := TRUE))
    ELSE
      VBT.SetCage(v, VBT.EverywhereCage)
    END
  END Position2;
  
PROCEDURE PreDefault(v: T; <*UNUSED*> READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    v.highlighter := HighlightVBT.Find(v);
    HighlightVBT.SetTexture(v.highlighter, Pixmap.Gray, Point.T{0,1})
  END PreDefault;

PROCEDURE PostDefault(v: T; <*UNUSED*> READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    HighlightVBT.SetRect(v.highlighter, Rect.Empty, 0);
    v.highlighter := NIL
  END PostDefault;

CONST BorderThickness = 2;

PROCEDURE DuringDefault(v: T; <*UNUSED*> lo: INTEGER) RAISES {} =
  BEGIN
    HighlightVBT.SetRect(v.highlighter, Rect.Meet(VBT.Domain(VBT.Parent(v)), 
      Rect.Inset(v.barOutline, -BorderThickness)), BorderThickness)
  END DuringDefault;

VAR cursors: ARRAY Axis.T OF Cursor.T;

BEGIN 
  cursors[Axis.T.Hor] := 
    Cursor.FromName(ARRAY OF TEXT {"XC_sb_h_double_arrow"});
  cursors[Axis.T.Ver] :=
    Cursor.FromName(ARRAY OF TEXT {"XC_sb_v_double_arrow"})
END HVBar.
