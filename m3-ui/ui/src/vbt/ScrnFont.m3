(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Mar  5 19:50:35 PST 1993 by msm      *)
(*      modified on Mon Feb 24 13:58:05 PST 1992 by muller   *)
(*      modified on Thu Jun  6  0:38:12 PDT 1991 by gnelson  *)
(*      modified on Tue May 15 14:00:16 PDT 1990 by steveg   *)
<*PRAGMA LL*>

MODULE ScrnFont;

IMPORT TextF, Rect, Interval, Text;

REVEAL T = Public BRANDED OBJECT END; Private = BRANDED OBJECT END;

PROCEDURE BoundingBox(txt: TEXT; fnt: T): Rect.T =
  VAR junk: BOOLEAN;
  BEGIN
    RETURN BoundingBoxSubValid(SUBARRAY(txt^, 0, LAST(txt^)), fnt, junk)
  END BoundingBox;

PROCEDURE BoundingBoxSub (READONLY txt: ARRAY OF CHAR; fnt: T): Rect.T =
  VAR junk: BOOLEAN;
  BEGIN
    RETURN BoundingBoxSubValid(txt, fnt, junk)
  END BoundingBoxSub;

PROCEDURE BoundingBoxSubValid (READONLY    txt  : ARRAY OF CHAR;
                                           fnt  : T;
                               VAR (*OUT*) valid: BOOLEAN        ):
  Rect.T =
  VAR
    res: Rect.T;
    len             := NUMBER(txt);
    we : Interval.T;
    rp : INTEGER;
  BEGIN
    valid := TRUE;
    IF (fnt = NIL) OR (len = 0) THEN RETURN Rect.FromSize(len, 1) END;
    WITH m = fnt.metrics DO
      IF m = NIL THEN RETURN Rect.FromSize(len, 1) END;
      res := m.maxBounds.boundingBox;
      IF m.charMetrics = NIL THEN
        VAR
          fc            := m.firstChar;
          lc            := m.lastChar;
          dc            := m.defaultChar;
          len2          := 0;
          ch  : INTEGER;
        BEGIN
          IF (ORD(FIRST(CHAR)) < fc OR lc < ORD(LAST(CHAR)))
               AND (dc < fc OR lc < dc) THEN
            FOR i := 0 TO len - 1 DO
              ch := ORD(txt[i]);
              IF fc <= ch AND ch <= lc THEN INC(len2) END
            END;
            valid := len = len2;
            len := len2
          END
        END;
        IF len = 0 THEN RETURN Rect.Empty END;
        WITH pw = m.maxBounds.printWidth DO
          IF pw >= 0 THEN
            res.west := MIN(res.west, 0);
            res.east := MAX(res.east, pw);
            INC(res.east, pw * (len - 1))
          ELSE
            res.east := MAX(res.east, 0);
            res.west := MIN(res.west, pw);
            INC(res.west, pw * (len - 1))
          END
        END
      ELSE
        we := Interval.Empty;
        rp := 0;
        IF m.selfClearing THEN
          FOR i := 0 TO LAST(txt) DO
            WITH pw = GetCM(m, txt[i], valid).printWidth DO
              IF pw > 0 THEN
                we.hi := MAX(we.hi, rp + pw)
              ELSE
                we.lo := MIN(we.lo, rp + pw)
              END;
              INC(rp, pw)
            END
          END
        ELSE
          FOR i := 0 TO LAST(txt) DO
            WITH cm = GetCM(m, txt[i], valid),
                 pw = cm.printWidth            DO
              IF pw > 0 THEN
                we := Interval.Join(
                        we, Interval.T{rp + MIN(0, cm.boundingBox.west),
                                       rp + MAX(pw, cm.boundingBox.east)})
              ELSE
                we := Interval.Join(
                        we, Interval.T{rp + MIN(pw, cm.boundingBox.west),
                                       rp + MAX(0, cm.boundingBox.east)})
              END;
              INC(rp, cm.printWidth)
            END
          END;
        END;
        IF Interval.IsEmpty(we) THEN RETURN Rect.Empty END;
        res.west := we.lo;
        res.east := we.hi
      END;
      RETURN res
    END
  END BoundingBoxSubValid;

CONST EmptyCM = CharMetric{boundingBox := Rect.Empty, printWidth := 0};

PROCEDURE GetCM (m: Metrics; ch: CHAR; VAR (*INOUT*) valid: BOOLEAN):
  CharMetric =
  VAR c := ORD(ch);
  BEGIN
    IF c < m.firstChar OR c > m.lastChar THEN
      c := m.defaultChar;
      IF c < m.firstChar OR c > m.lastChar THEN
        valid := FALSE;
        RETURN EmptyCM
      END
    END;
    RETURN m.charMetrics[c - m.firstChar]
  END GetCM;

PROCEDURE TextWidth (txt: TEXT; fnt: T): INTEGER =
  VAR
    len           := Text.Length(txt);
    rp  : INTEGER;
    junk: BOOLEAN;
  BEGIN
    IF (fnt = NIL) OR (len = 0) THEN RETURN len END;
    WITH m = fnt.metrics DO
      IF m = NIL THEN
        RETURN len
      ELSIF m.charMetrics = NIL THEN
        VAR
          fc            := m.firstChar;
          lc            := m.lastChar;
          dc            := m.defaultChar;
          len2          := 0;
          ch  : INTEGER;
        BEGIN
          IF (ORD(FIRST(CHAR)) < fc OR lc < ORD(LAST(CHAR)))
               AND (dc < fc OR lc < dc) THEN
            FOR i := 0 TO len - 1 DO
              ch := ORD(txt[i]);
              IF fc <= ch AND ch <= lc THEN INC(len2) END
            END;
            len := len2
          END
        END;
        RETURN len * m.maxBounds.printWidth
      ELSE
        rp := 0;
        FOR i := 0 TO len - 1 DO
          WITH cm = GetCM(m, txt[i], junk) DO INC(rp, cm.printWidth) END
        END;
        RETURN rp
      END
    END
  END TextWidth;

BEGIN END ScrnFont.
