(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: CacheDrawContext.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE CacheDrawContext;
IMPORT PaintOp;
IMPORT DrawContext;
IMPORT DrawContextClass;
IMPORT VBTDrawContext;
IMPORT Rect;
IMPORT Region;
IMPORT TransformOther;
IMPORT Line;
IMPORT Text;
IMPORT LinoText;
IMPORT LineList;
IMPORT LinoTextList;
IMPORT TextSubs;
IMPORT Point;
IMPORT RealPoint;
IMPORT Transform;
FROM Debug IMPORT S;

REVEAL
  T = Public BRANDED "CacheDrawContext" OBJECT
    lines: LineList.T := NIL;
    texts: LinoTextList.T := NIL;
  OVERRIDES
    gLine := GLine;
    grText := GText;
    recall := Recall;
    diffRecall := DiffRecall;
  END;

CONST
  DebugLevel = 60;

PROCEDURE GLine(self: T; l: Line.T) =
  BEGIN
(*    S("GLine: " & Line.Format(l), DebugLevel); *)
    self.lines := LineList.Cons(l, self.lines);
  END GLine;

PROCEDURE GText(self: T; t: LinoText.T) =
  BEGIN
(*    S("GText: " & LinoText.Format(t), DebugLevel); *)
    self.texts := LinoTextList.Cons(t, self.texts);
  END GText;

PROCEDURE PrepareTarget(self: T;
                        to: DrawContext.T;
                        where: Point.T): Transform.T =
  VAR
    scaleFactor := FLOAT(to.resDPI) / FLOAT(self.resDPI);
    scale := Transform.IsoScale(scaleFactor);
    p := RealPoint.FromPoint(where);
  BEGIN
    RETURN to.preTransform(Transform.Translate(p.h, p.v, scale));    
  END PrepareTarget;

PROCEDURE Recall(self: T;
                 to: DrawContext.T;
                 where: Point.T;
                 subs: TextSubs.T := NIL) =
  VAR
    restore := PrepareTarget(self, to, where);
  BEGIN
    (* lines *)
    VAR
      cur := self.lines;
    BEGIN
      WHILE cur # NIL DO
        to.line(cur.head);
        cur := cur.tail;
      END;
    END;

    (* text *)
    VAR
      cur := self.texts;
      l: LinoText.T;
    BEGIN
      IF subs = NIL THEN
        WHILE cur # NIL DO
          to.text(cur.head);
          cur := cur.tail;
        END;
      ELSE
        WHILE cur # NIL DO
          l := cur.head;
          ApplySubs(subs, l);
          to.text(l);
          cur := cur.tail;
        END;
      END;
    END;
    to.setTransform(restore);
  END Recall;

PROCEDURE UnNilSubs(VAR subs: TextSubs.T) =
  BEGIN
    IF subs = NIL THEN
      subs := NEW(TextSubs.T).init();
    END;
  END UnNilSubs;

PROCEDURE ApplySubs(subs: TextSubs.T; VAR result: LinoText.T) =
  BEGIN
    result.t := subs.apply(result.t);
    IF Text.Length(result.t) > 0 THEN
      IF Text.GetChar(result.t, 0) = '#' THEN
        result.t := Text.Sub(result.t, 1);
        result.color := PaintOp.FromRGB(0.5,0.5,0.5);
      END;
    END;
  END ApplySubs;

PROCEDURE GlobalSubsText(to: DrawContext.T; 
                         t: LinoText.T;
                         subs: TextSubs.T): LinoText.T =
  VAR
    result := TransformOther.ApplyToText(to.transform, t);
  BEGIN
    ApplySubs(subs, result);
    RETURN result;
  END GlobalSubsText;

PROCEDURE DiffRecall(self: T;
                     to: VBTDrawContext.T;
                     where := Point.Origin;
                     oldSubs, newSubs: TextSubs.T := NIL) =
  BEGIN
    <* ASSERT oldSubs # newSubs *>
    IF oldSubs # newSubs THEN
      VAR
        restore := PrepareTarget(self, to, where);
        cur := self.texts;
        old, new: LinoText.T;
        dummy: Point.T;
        oldRect: Rect.T;
      BEGIN
        UnNilSubs(oldSubs);
        UnNilSubs(newSubs);
        WHILE cur # NIL DO
          old := GlobalSubsText(to, cur.head, oldSubs);
          new := GlobalSubsText(to, cur.head, newSubs);
          IF NOT Text.Equal(old.t, new.t) THEN
            oldRect := to.grBoundText(old, dummy);
            to.setClip(Region.FromRect(oldRect));
            to.erase();
            to.setClip(Region.Full);
            to.grText(new);
            S("oldText = " & LinoText.Format(old), DebugLevel);
            S("newText = " & LinoText.Format(new), DebugLevel);
          END;
          cur := cur.tail;
        END;
        to.setTransform(restore);
      END;
    END;
  END DiffRecall;

BEGIN
END CacheDrawContext.
