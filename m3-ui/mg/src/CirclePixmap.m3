(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison and Greg Nelson *)
(* *)
(* Last modified on Tue Jul 20 11:20:42 PDT 1993 by steveg   *)
(*      modified on Tue Jul 21 20:28:24 PDT 1992 by harrison *)

MODULE CirclePixmap;

IMPORT Pixmap, ScrnPixmap, Palette, ScreenType, Rect, Point, TrestleComm;

TYPE
  Closure = Palette.PixmapClosure OBJECT
              width, height, border: CARDINAL;
              fill: BOOLEAN;
            OVERRIDES
              apply := Apply;
            END;

<* FATAL TrestleComm.Failure *>

(* Uses the midpoint ellipse scan conversion routine from Foley & van Dam,
   2nd ed., pp.  88--90. *)

PROCEDURE DrawEllipse (VAR raw  : ScrnPixmap.Raw;
                           a, b : CARDINAL;
                           value: BOOLEAN         ) =

  PROCEDURE ellipsePoints (h, v: INTEGER) =
    BEGIN
      FOR i := raw.bounds.west TO raw.bounds.east - 1 DO
        IF -h <= i AND i <= h THEN
          raw.set(Point.T{i, v}, ORD(value));
          IF v # 0 THEN raw.set(Point.T{i, -v}, ORD(value)); END;
        END;
      END;
    END ellipsePoints;

  VAR
    x, y: INTEGER;
    d1, d2: REAL;
  BEGIN
    x := 0;
    y := b;
    d1 := FLOAT(b * b - a * a * b) + FLOAT(a * a) / 4.0;
    ellipsePoints(x, y);

    WHILE FLOAT(a * a) * (FLOAT(y) - 0.5) > FLOAT(b * b) * (FLOAT(x) + 1.0) DO
      IF d1 < 0.0 THEN
        d1 := d1 + FLOAT(b * b * (2 * x + 3));
        INC(x);
      ELSE
        d1 := d1 + FLOAT(b * b * (2 * x + 3) + a * a * (2 - 2 * y));
        INC(x);
        DEC(y);
      END;
      ellipsePoints(x, y);
    END;

    d2 := FLOAT(b * b) * (FLOAT(x) + 0.5) * (FLOAT(x) + 0.5)
            + FLOAT(a * a * (y - 1) * (y - 1) - a * a * b * b);
    WHILE y > 0 DO
      IF d2 < 0.0 THEN
        d2 := d2 + FLOAT(b * b * (2 * x + 2) + a * a * (3 - 2 * y));
        INC(x);
        DEC(y);
      ELSE
        d2 := d2 + FLOAT(a * a * (3 - 2 * y));
        DEC(y);
      END;
      ellipsePoints(x, y);
    END;
  END DrawEllipse;

PROCEDURE SetValue (VAR raw: ScrnPixmap.Raw; value: BOOLEAN):
  ScrnPixmap.Raw =
  BEGIN
    FOR i := raw.bounds.west TO raw.bounds.east - 1 DO
      FOR j := raw.bounds.north TO raw.bounds.south - 1 DO
        raw.set(Point.T{i, j}, ORD(value));
      END;
    END;

    RETURN raw;
  END SetValue;

(* Return the smallest odd number <= n and > 0 *)
PROCEDURE MakePositiveAndOdd(n: INTEGER): CARDINAL =
  BEGIN
    RETURN MAX(1, ((n + 1) DIV 2) * 2 - 1);
  END MakePositiveAndOdd;

PROCEDURE Apply (self: Closure; st: ScreenType.T): ScrnPixmap.T =
  VAR
    widthOutsideBorder := MakePositiveAndOdd(self.width);
    heightOutsideBorder := MakePositiveAndOdd(self.height);
    widthInsideBorder := MakePositiveAndOdd(self.width - 2 * self.border);
    heightInsideBorder := MakePositiveAndOdd(self.height - 2 * self.border);
    outsideBorder := Rect.FromSize(widthOutsideBorder,
                                   heightOutsideBorder);
    insideBorder := Rect.FromSize(widthInsideBorder,
                                  heightInsideBorder);
    centered := Rect.Center(outsideBorder, Point.Origin);
    raw := ScrnPixmap.NewRaw(1, centered);
  BEGIN
    raw := SetValue(raw, FALSE);
    DrawEllipse(raw, Rect.HorSize(outsideBorder) DIV 2,
                Rect.VerSize(outsideBorder) DIV 2, TRUE);
    IF NOT self.fill THEN
      DrawEllipse(raw, Rect.HorSize(insideBorder) DIV 2,
                  Rect.VerSize(insideBorder) DIV 2, FALSE);
    END;
    RETURN st.pixmap.load(raw);
  END Apply;

PROCEDURE New (width, height: CARDINAL; border: CARDINAL := 0; fill := TRUE):
  Pixmap.T =
  BEGIN
    RETURN Palette.FromPixmapClosure(NEW(Closure, width := width,
                                         height := height,
                                         border := border, fill := fill));
  END New;

BEGIN
END CirclePixmap.

