(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE PSReaderGuts;
IMPORT Transform;
IMPORT TransformList;
IMPORT TransformOther;
IMPORT RealPoint;
IMPORT CacheDrawContext;
IMPORT DrawContext;
IMPORT DrawContextClass;
IMPORT LinoText;
IMPORT Line;
IMPORT LineStyle;
IMPORT PaintOp;

FROM Debug IMPORT S;
IMPORT Fmt;

CONST
  DebugLevel = 19;

REVEAL
  T = Public BRANDED "PSReaderGuts" OBJECT
    trans: TransformList.T;
    pen, pathStart: RealPoint.T;
    path: CacheDrawContext.T;
    lineWidth: REAL;
    textSize: REAL;
    to: DrawContext.T;
    freshPath: BOOLEAN;
    color := PaintOp.Fg;
  OVERRIDES
    init := Init;
    save := Save;
    restore := Restore;
    newPath := NewPath;
    closePath := ClosePath;
    lineTo := LineTo;
    moveTo := MoveTo;
    stroke := Stroke;
    scale := Scale;
    translate := Translate;
    setLineWidth := SetLineWidth;
    scaleFont := ScaleFont;
    show := Show;
  END;

PROCEDURE Init(self: T; to: DrawContext.T): T =
  VAR
    scale := FLOAT(to.resDPI)/72.0;
  BEGIN
    self.trans := TransformList.List1(TransformOther.FlipV);
    self.pen := RealPoint.T{0.0,0.0};
    self.path := NIL;
    self.lineWidth := 1.0;
    self.textSize := 1.0;
    self.to := to;
    self.scale(scale, scale);
    self.freshPath := FALSE;
    SC(self);
    RETURN self;
  END Init;

PROCEDURE SC(self: T) =
  BEGIN
    S("Scale is " & Fmt.Real(self.trans.head.a11), DebugLevel);
  END SC;

PROCEDURE Save(self: T) =
  BEGIN
    S("Save", DebugLevel);
    self.trans := TransformList.Cons(self.trans.head, self.trans);
    SC(self);
  END Save;

PROCEDURE Restore(self: T) =
  BEGIN
    S("Restore", DebugLevel);
    self.trans := self.trans.tail;
    <* ASSERT self.trans # NIL *>
    SC(self);
  END Restore;

PROCEDURE Scale(self: T; h, v: REAL) =
  BEGIN
    S("Scale: " & Fmt.Real(h) & "," & Fmt.Real(v), DebugLevel);
    (* h := 1.0; v := -1.0; *)
    self.trans.head := Transform.Compose(
                           TransformOther.AnIsoScaleAboutReal(h, v, self.pen),
                           self.trans.head);
    SC(self);
  END Scale;

PROCEDURE Translate(self: T; h, v: REAL) =
  BEGIN
    self.trans.head := Transform.Compose(
                           Transform.Translate(h, v, TransformOther.Identity),
                           self.trans.head);
  END Translate;

PROCEDURE SetLineWidth(self: T; w: REAL) =
  BEGIN
    self.lineWidth := w;
  END SetLineWidth;

PROCEDURE NewPath(self: T) =
  BEGIN
    self.path := NEW(CacheDrawContext.T, resDPI := self.to.resDPI);
    self.moveTo(0.0, 0.0);
    self.freshPath := TRUE;
    (* it's gotta be either the "newpath" or the "clip"
       after the bounding-box path creation which resets the pen.
       anyone know the PS specs? *)
  END NewPath;

PROCEDURE MoveTo(self: T; h, v: REAL) =
  BEGIN
    self.pen := TransformOther.ApplyToRealPoint(self.trans.head,
                                                RealPoint.T{h,v});
    IF self.freshPath THEN
      self.pathStart := self.pen;
      self.freshPath := FALSE;
    END;
  END MoveTo;

PROCEDURE RealScale(self: T; r: REAL): INTEGER =
  BEGIN
    RETURN TransformOther.RoundApplyToReal(self.trans.head, r);
  END RealScale;

PROCEDURE LineFrom(self: T; old: RealPoint.T) =
  VAR
    s: LineStyle.T;
  BEGIN
    IF self.path # NIL THEN
      s := LineStyle.T{color := self.color,
                       thick := RealScale(self, self.lineWidth)};
      self.path.line(Line.T{a := RealPoint.Round(old),
                            b := RealPoint.Round(self.pen),
                            s := s});
    END;
  END LineFrom; 

PROCEDURE LineTo(self: T; h, v: REAL) =
  VAR
    old := self.pen;
  BEGIN
    MoveTo(self, h, v);
    LineFrom(self, old);
  END LineTo;

PROCEDURE ClosePath(self: T) =
  VAR
    old := self.pen;
  BEGIN
    self.pen := self.pathStart;
    LineFrom(self, old);
  END ClosePath;

PROCEDURE Stroke(self: T) =
  BEGIN
    self.path.recall(self.to);
    self.path := NIL;
  END Stroke;

PROCEDURE ScaleFont(self: T; s: REAL) =
  BEGIN
    self.textSize := s;
  END ScaleFont;

PROCEDURE Show(self: T; t: TEXT; attach: LinoText.Attach) =
  BEGIN
    SC(self);
    S("Show, size = " & Fmt.Real(self.textSize) & " -> " &
      Fmt.Int(RealScale(self, self.textSize)), DebugLevel);
    self.to.text(LinoText.T{a := RealPoint.Round(self.pen),
                            t := t,
                            size := RealScale(self, self.textSize),
                            attach := attach,
                            color := self.color});
  END Show;

BEGIN
END PSReaderGuts.
