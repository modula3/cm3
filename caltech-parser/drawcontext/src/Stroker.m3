(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE Stroker;
IMPORT DrawContext;
IMPORT Point;
IMPORT Rect;
IMPORT Line;
IMPORT LineStyle;

REVEAL
  T = Public BRANDED OBJECT
    dc: DrawContext.T;
    ls: LineStyle.T;
    pen: Point.T;
  OVERRIDES
    init := Init;
    moveTo := MoveTo;
    lineTo := LineTo;
    frameRect := FrameRect;
  END;

PROCEDURE Init(self: T; dc: DrawContext.T; ls := LineStyle.Default): T =
  BEGIN
    self.dc := dc;
    self.ls := ls;
    self.pen := Point.Origin;
    RETURN self;
  END Init;

PROCEDURE MoveTo(self: T; p: Point.T) =
  BEGIN
    self.pen := p;
  END MoveTo;

PROCEDURE LineTo(self: T; p: Point.T) =
  BEGIN
    self.dc.line(Line.T{self.pen, p, self.ls});
    self.moveTo(p);
  END LineTo;

PROCEDURE FrameRect(self: T; r: Rect.T) =
  BEGIN
    self.moveTo(Rect.NorthWest(r));
    self.lineTo(Rect.NorthEast(r));
    self.lineTo(Rect.SouthEast(r));
    self.lineTo(Rect.SouthWest(r));
    self.lineTo(Rect.NorthWest(r));
  END FrameRect;

BEGIN
END Stroker.
