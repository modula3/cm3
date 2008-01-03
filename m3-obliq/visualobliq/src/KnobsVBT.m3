(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Sep  1 19:10:50 PDT 1993 by bharat *)
(* modified on Tue Jul 6 13:50:37 PDT 1993 by mhb *)

MODULE KnobsVBT;

IMPORT Axis, Filter,  Pixmap, Point, Rect, Region,  VBT,
       VBTClass;

REVEAL
  T = Public BRANDED "VO-KnobsVBT" OBJECT
        knobs  := FALSE;
        single := TRUE;
      OVERRIDES
        init    := Init;
        reshape := Reshape;
        repaint := Repaint;
      END;

PROCEDURE Init (v: T; ch: VBT.T): T =
  BEGIN
    RETURN Filter.T.init(v, ch)
  END Init;

PROCEDURE Add (v: T) =
  BEGIN
    IF NOT v.knobs THEN v.knobs := TRUE; DrawKnobs(v, Rect.Full) END
  END Add;

PROCEDURE Remove (v: T) =
  BEGIN
    IF v.knobs THEN v.knobs := FALSE; VBT.Mark(v) END
  END Remove;

PROCEDURE Inside (v: T; READONLY pt: Point.T): BOOLEAN =
  BEGIN
    RETURN Rect.Member(pt, VBT.Domain(v))
             AND NOT Rect.Member(pt, InnerDomain(v, 1.0))
  END Inside;

PROCEDURE SetSingleMode (v: T; READONLY tv: BOOLEAN) =
  BEGIN
    v.single := tv
  END SetSingleMode;

PROCEDURE InnerDomain (v: T; READONLY Size: REAL): Rect.T =
  VAR
    dx := ROUND(VBT.MMToPixels(v, Size, Axis.T.Hor));
    dy := ROUND(VBT.MMToPixels(v, Size, Axis.T.Ver));
  BEGIN
    RETURN Rect.Change(VBT.Domain(v), dx, -dx, dy, -dy)
  END InnerDomain;

PROCEDURE DrawKnobs (v: T; READONLY clip: Rect.T) =
  VAR
    hdom                             := InnerDomain(v, 0.0);
    idom                             := InnerDomain(v, 1.0);
    jdom                             := InnerDomain(v, 2.0);
    blips: ARRAY [1 .. 12] OF Rect.T;
    nw                               := Rect.NorthWest(idom);
    sw                               := Rect.SouthWest(idom);
    ne                               := Rect.NorthEast(idom);
    se                               := Rect.SouthEast(idom);
    n                                := Point.Div(Point.Add(nw, ne), 2);
    s                                := Point.Div(Point.Add(sw, se), 2);
    e                                := Point.Div(Point.Add(ne, se), 2);
    w                                := Point.Div(Point.Add(nw, sw), 2);
    d := Point.Div(
           Point.Sub(Rect.NorthWest(jdom), Rect.NorthWest(hdom)), 2);
    dd := Point.FromCoords(ROUND(VBT.MMToPixels(v, 0.25, Axis.T.Hor)),
                           ROUND(VBT.MMToPixels(v, 0.25, Axis.T.Ver)));

  BEGIN
    blips[1] := Rect.FromCorners(Point.Sub(nw, d), Point.Add(nw, d));
    blips[2] := Rect.FromCorners(Point.Sub(sw, d), Point.Add(sw, d));
    blips[3] := Rect.FromCorners(Point.Sub(ne, d), Point.Add(ne, d));
    blips[4] := Rect.FromCorners(Point.Sub(se, d), Point.Add(se, d));

    blips[5] := Rect.FromCorners(Point.Sub(n, d), Point.Add(n, d));
    blips[6] := Rect.FromCorners(Point.Sub(s, d), Point.Add(s, d));
    blips[7] := Rect.FromCorners(Point.Sub(e, d), Point.Add(e, d));
    blips[8] := Rect.FromCorners(Point.Sub(w, d), Point.Add(w, d));

    blips[9] := Rect.FromCorners(Point.Sub(Rect.NorthWest(idom), dd),
                                 Point.Add(Rect.NorthEast(idom), dd));
    blips[10] := Rect.FromCorners(Point.Sub(Rect.NorthEast(idom), dd),
                                  Point.Add(Rect.SouthEast(idom), dd));
    blips[11] := Rect.FromCorners(Point.Sub(Rect.SouthWest(idom), dd),
                                  Point.Add(Rect.SouthEast(idom), dd));
    blips[12] := Rect.FromCorners(Point.Sub(Rect.NorthWest(idom), dd),
                                  Point.Add(Rect.SouthWest(idom), dd));
    FOR i := FIRST(blips) TO LAST(blips) DO
      blips[i] := Rect.Meet(blips[i], clip)
    END;
    IF v.single THEN
      VBT.PolyTexture(v, blips, src := Pixmap.Solid);
    ELSE
      VBT.PolyTexture(v, blips, src := Pixmap.Gray);
    END;
 
  END DrawKnobs;

PROCEDURE Repaint (v: T; READONLY badR: Region.T) RAISES {} =
  BEGIN
    Filter.T.repaint(v, badR);
    IF v.knobs THEN DrawKnobs(v, badR.r) END
  END Repaint;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    Filter.T.reshape(v, cd);
    IF v.knobs THEN DrawKnobs(v, Rect.Full) END
  END Reshape;

BEGIN
END KnobsVBT.







