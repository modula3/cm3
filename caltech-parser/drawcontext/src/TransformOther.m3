(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TransformOther.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE TransformOther;
IMPORT RealPoint;
IMPORT Transform;
FROM Transform IMPORT Translate;
IMPORT Point;
IMPORT Rect;
IMPORT Region;
IMPORT Line;
IMPORT LineStyle;
IMPORT LinoText;

PROCEDURE ApplyToRect(t: Transform.T; r: Rect.T): Rect.T =
  VAR
    p1 := Transform.Apply(t, Rect.NorthWest(r));
    p2 := Transform.Apply(t, Rect.SouthEast(r));
  BEGIN
    RETURN Rect.FromCorners(p1, p2);
  END ApplyToRect;

PROCEDURE ApplyToRegion(t: Transform.T; r: Region.T): Region.T =
  VAR
    rects := Region.ToRects(r);
  BEGIN
    FOR i := FIRST(rects^) TO LAST(rects^) DO
      rects[i] := ApplyToRect(t, rects[i]);
    END;
    RETURN Region.FromRects(rects^);
  END ApplyToRegion;

PROCEDURE ApplyToLine(t: T; l: Line.T): Line.T =
  BEGIN
    RETURN Line.T{a := Transform.Apply(t, l.a),
                  b := Transform.Apply(t, l.b),
                  s := ApplyToLineStyle(t, l.s)};
  END ApplyToLine;

PROCEDURE ApplyToLineStyle(t: T; s: LineStyle.T): LineStyle.T =
  BEGIN
    s.thick := ApplyToInt(t, s.thick);
    RETURN s;
  END ApplyToLineStyle;

PROCEDURE LeftTurns(t: T): [0..3] =
  VAR
    h := t.a11; v := t.a12; (* image of (0,1) vector *)
    code := ORD(v > h) * 10 + ORD(v > -h);
  BEGIN
    CASE code OF
    | 01 => RETURN 0;
    | 00 => RETURN 1;
    | 10 => RETURN 2;
    | 11 => RETURN 3;
    ELSE
      <* ASSERT FALSE *>
    END;
  END LeftTurns;

PROCEDURE ApplyToAttach(t: T; a: LinoText.Attach): LinoText.Attach =
  VAR
    turns := LeftTurns(t);
  BEGIN
    IF turns = 0 THEN
      RETURN a;
    ELSE
      IF a = LinoText.Attach.CenterBase THEN
        a := LinoText.Attach.South;
      END;
      IF a = LinoText.Attach.Center THEN
        RETURN a;
      ELSE
        RETURN VAL((ORD(a) + turns) MOD 4, LinoText.Attach);
      END;
    END;
  END ApplyToAttach;

PROCEDURE ApplyToInt(t: T; i: INTEGER): INTEGER =
  BEGIN
    RETURN RoundApplyToReal(t, FLOAT(i));
  END ApplyToInt;

PROCEDURE RoundApplyToReal(t: T; r: REAL): INTEGER =
  VAR
    scale := MAX(ABS(t.a11), ABS(t.a12));
  BEGIN
    RETURN MAX(TRUNC(scale * ABS(r) + 0.5), 1);
  END RoundApplyToReal;

PROCEDURE ApplyToText(t: T; tx: LinoText.T): LinoText.T =
  VAR
    result := tx;
  BEGIN
    result.a := Transform.Apply(t, tx.a);
    result.size := ApplyToInt(t, tx.size);
    result.attach := ApplyToAttach(t, tx.attach);
    RETURN result;
  END ApplyToText;

PROCEDURE Inverse(t: T): T =
  VAR
    unrot := Transform.T{a11 := t.a22, a22 := t.a11,
                         a12 := -t.a12, a21 := -t.a21,
                         a31 := 0.0, a32 := 0.0};
    det := t.a11 * t.a22 - t.a12 * t.a21;
    idet := 1.0 / det;
    result := Transform.Compose(Transform.Translate(-t.a31, -t.a32, Identity),
                                Transform.Scale(idet, idet, unrot));
  BEGIN
    (* ASSERT Transform.Equal(Transform.Compose(t, result), Identity) *)
    RETURN result;
  END Inverse;

PROCEDURE IsoScaleAbout(scale: REAL; p: Point.T): T =
  BEGIN
    RETURN Transform.Compose(Translate(FLOAT(-p.h), FLOAT(-p.v), Identity),
                             Translate(FLOAT(p.h), FLOAT(p.v),
                                                 Transform.IsoScale(scale)));
  END IsoScaleAbout;

PROCEDURE ApplyToRealPoint(t: T; p: RealPoint.T): RealPoint.T =
  BEGIN
    RETURN RealPoint.T{p.h * t.a11 + p.v * t.a21 + t.a31,
                       p.h * t.a12 + p.v * t.a22 + t.a32};
  END ApplyToRealPoint;

PROCEDURE AnIsoScaleAboutReal(hScale, vScale: REAL;
                              p: RealPoint.T): T =
  BEGIN
    RETURN Transform.Compose(Translate(-p.h, -p.v, Identity),
                             Translate(p.h, p.v,
                                       Transform.AnIsoScale(hScale, vScale)));
  END AnIsoScaleAboutReal;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a=b;
  END Equal;

BEGIN
  <* ASSERT ORD(LinoText.Attach.North) = 0 *>
  <* ASSERT ORD(LinoText.Attach.West) = 1 *>
  <* ASSERT ORD(LinoText.Attach.South) = 2 *>
  <* ASSERT ORD(LinoText.Attach.East) = 3 *>
  (* See ApplyToAttach *)
END TransformOther. 
