(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE DrawContextClass;
IMPORT Point;
IMPORT Rect;
IMPORT Region;
IMPORT Transform;
IMPORT TransformOther;
IMPORT Line;
IMPORT LinoText;
IMPORT TextBounder;
REVEAL
  T = Public BRANDED OBJECT
    clipRects: REF ARRAY OF Rect.T;
  OVERRIDES
    line := LLine;
    text := LText;
    boundText := LBoundText;
    regionVisible := LRegionVisible;
    gLine := GLine;
    gText := GText;
    grText := GRText;
    grBoundText := GRBoundText;
    setClip := SetClip;
    setTransform := SetTransform;
    preTransform := PreTransform;
    getClipRects := GetClipRects;
    setTextBounder := SetTextBounder;
  END;
  
PROCEDURE LLine(self: T; l: Line.T) =
  BEGIN
    self.gLine(TransformOther.ApplyToLine(self.transform, l));
  END LLine;

PROCEDURE LText(self: T; t: LinoText.T) =
  BEGIN
    self.grText(TransformOther.ApplyToText(self.transform, t));
  END LText;

PROCEDURE LBoundText(self: T; t: LinoText.T): Rect.T =
  VAR
    tx := TransformOther.ApplyToText(self.transform, t);
    dummy: Point.T;
    grect := GRBoundText(self, tx, dummy);
    inv := TransformOther.Inverse(self.transform);
  BEGIN
    RETURN TransformOther.ApplyToRect(inv, grect);
  END LBoundText;

PROCEDURE LRegionVisible(self: T; r: Region.T): BOOLEAN =
  BEGIN
    RETURN Region.Overlap(TransformOther.ApplyToRegion(self.transform, r),
                          self.clip);
  END LRegionVisible;

  (* intermediate text procs: after conversion to global coords, but
     before final LinoText.Attach translation *)
PROCEDURE GRBoundText(self: T; t: LinoText.T; VAR penOffset: Point.T): Rect.T =
  VAR
    twest := t;
    r: Rect.T;
    p: Point.T;
  BEGIN
    twest.attach := LinoText.Attach.West;
    r := self.textBounder.bound(twest);
    p := LinoText.AttachPoint(r, t.a.v, t.attach);
    penOffset := Point.Sub(t.a, p);
    r := Rect.Move(r, penOffset);
    RETURN r;
  END GRBoundText;
  
PROCEDURE GRText(self: T; t: LinoText.T) =
  VAR
    penOffset: Point.T;
    tx := t;
  BEGIN
    EVAL GRBoundText(self, t, penOffset);
    tx.a := Point.Add(t.a, penOffset);
    tx.attach := LinoText.Attach.West;
    self.gText(tx);
  END GRText;

PROCEDURE SetClip(self: T; r: Region.T) =
  BEGIN
    self.clip := r;
    self.clipRects := NIL;
  END SetClip;

PROCEDURE GetClipRects(self: T): REF ARRAY OF Rect.T =
  BEGIN
    IF self.clipRects = NIL THEN
      self.clipRects := Region.ToRects(self.clip);
    END;
    RETURN self.clipRects;
  END GetClipRects;

PROCEDURE PreTransform(self: T; t: Transform.T): Transform.T =
  VAR
    old := self.transform;
  BEGIN
    self.setTransform(Transform.Compose(t, old));
    RETURN old;
  END PreTransform;

PROCEDURE SetTransform(self: T; t: Transform.T) =
  BEGIN self.transform := t; END SetTransform;
PROCEDURE SetTextBounder(self: T; tb: TextBounder.T) =
  BEGIN self.textBounder := tb; END SetTextBounder;

PROCEDURE GLine(<*UNUSED*>self: T; <*UNUSED*>l: Line.T) =
  BEGIN END GLine;
PROCEDURE GText(<*UNUSED*>self: T; <*UNUSED*>t: LinoText.T) =
  BEGIN END GText;

BEGIN
END DrawContextClass.
