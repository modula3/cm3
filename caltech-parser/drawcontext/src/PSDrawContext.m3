(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE PSDrawContext;
IMPORT WYSIWYGify;
IMPORT DrawContextClass;
IMPORT Line;
IMPORT LineList;
IMPORT LinoText;
IMPORT LinoTextList;
IMPORT Rect;
IMPORT Point;
IMPORT Transform;
IMPORT Bundle;
IMPORT PSTextBounder;
IMPORT TextSubs;
IMPORT Fmt;
IMPORT Wr;
IMPORT PSFormBundle;
IMPORT FmtTime;
IMPORT Time;
IMPORT Text;
IMPORT TextWr;
IMPORT Thread;

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL
  T = Public BRANDED "PSDrawContext" OBJECT
    lines: LineList.T;
    texts: LinoTextList.T;
    bbox: Rect.T;
    captureRes: REAL;
  OVERRIDES
    init := Init;
    write := Write;
    gLine := GLine;
    gText := GText;
  END;

PROCEDURE Init(self: T; captureResDPI: INTEGER): T =
  BEGIN
    self.textBounder := NEW(PSTextBounder.T).init();
    self.lines := NIL;
    self.texts := NIL;
    self.bbox := Rect.Empty;
    self.captureRes := FLOAT(captureResDPI);
    self.setTransform(Transform.IsoScale(self.captureRes/FLOAT(self.resDPI)));
    RETURN self;
  END Init;

PROCEDURE Write(self: T; wr: Wr.T; title: TEXT) =
  VAR
    subs := NEW(TextSubs.T).init();
    form := Bundle.Get(PSFormBundle.Get(), "form.ps");
    cap2points := 72.0 / FLOAT(self.captureRes);
    cpt := Fmt.Real(cap2points, style := Fmt.Style.Fix, prec := 6);
  BEGIN
    subs.add("#title#", title);
    subs.add("#cpt#", cpt);
    subs.add("#lines#", FmtLines(self));
    subs.add("#texts#", FmtTexts(self));
    subs.add("#date#", FmtTime.Long(Time.Now()));
    WITH b = self.bbox DO
      subs.int("#width#", TRUNC(FLOAT(b.east - b.west) * cap2points)+1);
      subs.int("#height#", TRUNC(FLOAT(b.south - b.north) * cap2points )+1);
      subs.int("#xoff#", ROUND(FLOAT(-b.west) * cap2points));
      subs.int("#yoff#", ROUND(FLOAT(b.south) * cap2points));
    END;
    Wr.PutText(wr, subs.apply(form));
  END Write;

PROCEDURE BBox(self: T; r: Rect.T) =
  BEGIN
    self.bbox := Rect.Join(self.bbox, r);
  END BBox;

PROCEDURE GLine(self: T; l: Line.T) =
  BEGIN
    BBox(self, Line.GetBoundRect(l));
    self.lines := LineList.Cons(l, self.lines);
  END GLine;

PROCEDURE GText(self: T; t: LinoText.T) =
  BEGIN
    BBox(self, self.textBounder.bound(t));
    self.texts := LinoTextList.Cons(t, self.texts);
  END GText;

PROCEDURE FmtPoint(p: Point.T): TEXT =
  BEGIN
    RETURN Fmt.Int(p.h) & " " & Fmt.Int(p.v);
  END FmtPoint;

PROCEDURE FmtText(t: TEXT): TEXT =
  VAR
    wr := TextWr.New();
    c: CHAR;
  BEGIN
    FOR i := 0 TO Text.Length(t) - 1 DO
      c := Text.GetChar(t, i);
      CASE c OF
      | '(', ')' => Wr.PutChar(wr, '\134');
      ELSE
      END;
      Wr.PutChar(wr, c);
    END;
    RETURN TextWr.ToText(wr);
  END FmtText;

PROCEDURE FmtLines(self: T): TEXT =
  VAR
    wr := TextWr.New();
    cur := self.lines;
    l: Line.T;
  BEGIN
    WHILE cur # NIL DO
      l := cur.head;
      Wr.PutText(wr, Fmt.Int(l.s.thick));
      Wr.PutText(wr, " slw n ");
      Wr.PutText(wr, FmtPoint(l.a));
      Wr.PutText(wr, " m ");
      Wr.PutText(wr, FmtPoint(l.b));
      Wr.PutText(wr, " l gs col0 s gr\n");
      cur := cur.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FmtLines;

PROCEDURE FmtTexts(self: T): TEXT =
  VAR
    wr := TextWr.New();
    cur := self.texts;
    t: LinoText.T;
  BEGIN
    WHILE cur # NIL DO
      t := cur.head;
      Wr.PutText(wr, "tff ");
      Wr.PutText(wr, WYSIWYGify.FormatTextSizeForPS(FLOAT(t.size)));
      Wr.PutText(wr, " scf sf ");
      Wr.PutText(wr, FmtPoint(t.a));
      Wr.PutText(wr, " tx (");
      Wr.PutText(wr, FmtText(t.t));
      Wr.PutText(wr, ") col0 sh gr\n");
      cur := cur.tail;
    END;
    RETURN TextWr.ToText(wr);
  END FmtTexts;

BEGIN
END PSDrawContext.
