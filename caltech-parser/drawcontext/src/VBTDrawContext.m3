(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE VBTDrawContext;
IMPORT DrawContextClass;
IMPORT VBT, PaintOp;
IMPORT Line;
IMPORT LinoText;
IMPORT Rect;
IMPORT OneFont;
IMPORT VBTTextBounder;

(*IMPORT Text, Wr, Stdio, Region;*)

REVEAL
  T = Public BRANDED OBJECT
    v: VBT.Leaf;
    bgColor: PaintOp.T;
  OVERRIDES
    init := Init;
    erase := Erase;
    gLine := GLine;
    gText := GText;
  END;

PROCEDURE Init(self: T; v: VBT.Leaf; bgColor: PaintOp.T): T =
  BEGIN
    self.v := v;
    self.bgColor := bgColor;
    self.textBounder := NEW(VBTTextBounder.T).init(v);
    RETURN self;
  END Init;

PROCEDURE Erase(self: T) =
  BEGIN
    VBT.PaintRegion(self.v, self.clip, self.bgColor);
  END Erase;

PROCEDURE GLine(self: T; l: Line.T) =
  VAR
    rects := self.getClipRects();
    bound := Line.GetBoundRect(l);
  BEGIN
    FOR i := FIRST(rects^) TO LAST(rects^) DO
      IF Rect.Overlap(bound, rects[i]) THEN
        VBT.Line(self.v, rects[i], l.a, l.b,
                 l.s.thick, VBT.EndStyle.Round, l.s.color);
      END;
    END;
  END GLine;

PROCEDURE GText(self: T; t: LinoText.T) =
  VAR
    rects := self.getClipRects();
    bound := self.textBounder.bound(t);
    font := OneFont.FromSize(t.size);
  BEGIN
    FOR i := FIRST(rects^) TO LAST(rects^) DO
      IF Rect.Overlap(bound, rects[i]) THEN
        (*IF Text.Equal(t.t, "vertex #3") THEN
          Wr.PutText(Stdio.stdout, "Vertex3\n");
          Wr.Flush(Stdio.stdout);
          VBT.PaintRegion(self.v, Region.FromRect(rects[i]));
        END;*)
          
        VBT.PaintText(self.v, rects[i], t.a, font, t.t, t.color);
      END;
    END;
  END GText;

BEGIN
END VBTDrawContext. 
