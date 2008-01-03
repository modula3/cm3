(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:53:33 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:21:22 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 16:36:39 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE HighlightVBT;

IMPORT Batch, BatchUtil, BatchRep, FilterClass,
  PaintOp, Pixmap, Point, Rect, Region, ScrnPixmap,
  VBT, VBTClass, Filter;

REVEAL T = Public BRANDED OBJECT 
    txt: Pixmap.T;
    op: PaintOp.T;
    delta: Point.T;
    rect: Rect.T;
    border: CARDINAL
  OVERRIDES
    reshape := Reshape;
    capture := Capture;
    paintbatch := PaintBatch;
    init := Be
  END;

PROCEDURE Be(
  v: T;
  ch: VBT.T;
  op: PaintOp.T := PaintOp.TransparentSwap;
  texture: Pixmap.T := Pixmap.Gray;
  READONLY offset: Point.T): T =
  BEGIN
    v.op := op;
    v.txt := texture;
    v.delta := offset;
    v.rect := Rect.Empty;
    v.border := 0;
    EVAL Filter.T.init(v, ch);
    RETURN v
  END Be;

PROCEDURE New(
  ch: VBT.T;
  op: PaintOp.T := PaintOp.TransparentSwap;
  texture: Pixmap.T := Pixmap.Gray;
  READONLY delta:Point.T): T =
  BEGIN
    RETURN Be(NEW(T), ch, op, texture, delta)
  END New;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR cdP := cd;
  BEGIN
    IF NOT Rect.Subset(Rect.Meet(v.rect, cd.saved), cd.new) THEN
      (* Can't allow scrolling of saved into new, since the saved pixels
         might be highlighted, and we can't unhighlight them without
         generating bad rectangles, so: *)
      cdP.saved := Rect.Meet(cd.new, cd.saved)
    END;
    Filter.T.reshape(v, cdP) 
  END Reshape;

PROCEDURE Capture(
    self: T;
    <*UNUSED*> ch: VBT.T;
    READONLY rect: Rect.T;
    VAR (* OUT *) br: Region.T)
    : ScrnPixmap.T
  RAISES {} =
  VAR res: ScrnPixmap.T;
  BEGIN
    InvertHighlight(self, rect);
    res := VBT.Capture(self, rect, br);
    InvertHighlight(self, rect);
    RETURN res;
  END Capture;

PROCEDURE InvertHighlight(v: T; READONLY clip: Rect.T) =
  (* Invert all highlighted bits contained in clip.  LL = v.ch *)
  VAR a: Rect.Partition; 
      rect := Rect.Meet(v.rect, clip); 
      inset := Rect.Inset(v.rect, v.border);
  BEGIN
    IF Rect.Subset(rect, inset) THEN RETURN END;
    Rect.Factor(rect, inset, a, 0, 0);
    a[2] := a[4];
    VBT.PolyTexture(v, SUBARRAY(a, 0, 4), v.op, v.txt, v.delta)
  END InvertHighlight;

PROCEDURE PaintBatch(v: T; <*UNUSED*> ch: VBT.T; ba: Batch.T) RAISES {} =
  BEGIN
    IF Rect.IsEmpty(v.rect)
       OR Rect.IsEmpty(ba.scrollSource) AND NOT Rect.Overlap(v.rect, ba.clip)
    THEN
      VBTClass.PaintBatch(v, ba)
    ELSE
      VAR
        rect: Rect.T; 
        inset := Rect.Inset(v.rect, v.border);
        a: Rect.Partition;
      BEGIN
        BatchUtil.Tighten(ba);
        rect := Rect.Meet(v.rect, Rect.Join(ba.clip, ba.scrollSource));
        IF Rect.Subset(rect, inset) THEN
          VBTClass.PaintBatch(v, ba)
        ELSE
          Rect.Factor(rect, inset, a, 0, 0);
          a[2] := a[4];
	  VBT.BeginGroup(v);
          VBT.PolyTexture(v, SUBARRAY(a, 0, 4), v.op, v.txt, v.delta);
          VBTClass.PaintBatch(v, ba);
          VBT.PolyTexture(v, SUBARRAY(a, 0, 4), v.op, v.txt, v.delta);
	  VBT.EndGroup(v)
        END
      END
    END
  END PaintBatch;

PROCEDURE SetTexture(
  w: VBT.T; 
  txt: Pixmap.T; 
  READONLY delta := Point.Origin;
  op := PaintOp.TransparentSwap) =
  VAR v := Find(w); BEGIN
    IF v = NIL OR (v.txt = txt) AND Point.Equal(v.delta, delta)
    AND op = v.op THEN 
      RETURN 
    ELSIF v.ch = NIL THEN
      v.txt := txt;
      v.delta := delta;
      v.op := op
    ELSE
      LOCK v.ch DO
        InvertHighlight(v, Rect.Full);
        v.txt := txt;
        v.delta := delta;
        v.op := op;
        InvertHighlight(v, Rect.Full)
      END
    END
  END SetTexture;

PROCEDURE SetRect(w: VBT.T; READONLY r: Rect.T; border: CARDINAL) =
  VAR v := Find(w); BEGIN
    IF v = NIL OR Rect.Equal(v.rect, r) AND v.border = border THEN 
      RETURN 
    ELSIF v.ch = NIL THEN
      v.rect := r;
      v.border := border
    ELSE
      InnerSetRect(v, r, border)
    END
  END SetRect;

PROCEDURE Get(
  v: VBT.T;
  VAR rect: Rect.T;
  VAR inset: CARDINAL;
  VAR txt: Pixmap.T;
  VAR delta: Point.T;
  VAR op: PaintOp.T): BOOLEAN =
  VAR w := Find(v); BEGIN
    IF w = NIL THEN RETURN FALSE END;
    rect := w.rect;
    inset := w.border;
    txt := w.txt;
    delta := w.delta;
    op := w.op;
    RETURN TRUE
  END Get;

PROCEDURE Find(v: VBT.T): T = 
  BEGIN
    LOOP
      TYPECASE v OF
        T(v) => RETURN v
      ELSE v := v.parent
      END
    END
  END Find;

PROCEDURE InnerSetRect(v: T; READONLY r: Rect.T; bd: CARDINAL) =
  VAR
    oldb, newb: Rect.Partition;

  PROCEDURE PaintDiff(READONLY r1, r2: Rect.T) =
    (* Add the nonempty rectangles in the difference of r1 and r2 to the
       batch. *)
    VAR a: Rect.Partition;
    BEGIN
      IF Rect.Subset(r1, r2) THEN RETURN END;
      Rect.Factor(r1, r2, a, 0, 0);
      a[2] := a[4];
      VBT.PolyTexture(v, SUBARRAY(a, 0, 4), v.op, v.txt, v.delta)
    END PaintDiff;

  BEGIN
    LOCK v.ch DO
      Rect.Factor(Rect.Meet(v.rect, v.domain),
        Rect.Inset(v.rect, v.border), oldb, 1, 1);
      v.border := bd;
      IF bd = 0 THEN v.rect := Rect.Empty ELSE v.rect := r END;
      Rect.Factor(Rect.Meet(v.rect, v.domain),
        Rect.Inset(v.rect, v.border), newb, 1, 1);
      FOR i := 0 TO 4 DO
        IF (i # 2) THEN
          (* paint symmetric difference of edge i *)
          PaintDiff(newb[i], oldb[i]);
          PaintDiff(oldb[i], newb[i]);
        END
      END
    END
  END InnerSetRect;

PROCEDURE Invert(v: VBT.T; READONLY r: Rect.T; border: CARDINAL) =
  VAR h := Find(v); BEGIN
    SetTexture(h, Pixmap.Solid, Point.Origin);
    SetRect(h, r, border)
  END Invert;

BEGIN END HighlightVBT.
