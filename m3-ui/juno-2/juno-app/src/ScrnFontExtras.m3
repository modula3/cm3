(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 15 13:59:24 PST 1997 by heydon                   *)

MODULE ScrnFontExtras;

IMPORT ScrnFont, Rect, Text;

(* Most of this was copied from the implementation in "ScrnFont.m3". *)

PROCEDURE TightBoundingBox(txt: TEXT; fnt: ScrnFont.T): Rect.T =
  CONST BufSize = 64;
  VAR junk: BOOLEAN; len := Text.Length (txt); BEGIN
    IF (len <= BufSize) THEN
      VAR buf: ARRAY [0..BufSize-1] OF CHAR; BEGIN
      	Text.SetChars(buf, txt);
      	RETURN TightBoundingBoxSubValid(SUBARRAY(buf, 0, len), fnt, junk)
      END
    ELSE
      VAR ref := NEW(REF ARRAY OF CHAR, len); BEGIN
      	Text.SetChars(ref^, txt);
      	RETURN TightBoundingBoxSubValid(ref^, fnt, junk)
      END
    END;
  END TightBoundingBox;

PROCEDURE TightBoundingBoxSub(
  READONLY txt: ARRAY OF CHAR;
  fnt: ScrnFont.T)
  : Rect.T =
  VAR junk: BOOLEAN; BEGIN
    RETURN TightBoundingBoxSubValid(txt, fnt, junk)
  END TightBoundingBoxSub;

PROCEDURE TightBoundingBoxSubValid(
  READONLY txt: ARRAY OF CHAR;
  fnt: ScrnFont.T;
  VAR (*OUT*) valid: BOOLEAN)
  : Rect.T =
  VAR res: Rect.T; len := NUMBER(txt); BEGIN
    valid := TRUE;
    IF len = 0 THEN RETURN Rect.Empty END;
    IF fnt = NIL OR fnt.metrics = NIL THEN RETURN Rect.FromSize(len, 1) END;
    WITH m = fnt.metrics DO
      <* ASSERT m # NIL *>
      IF m.charMetrics # NIL THEN
        (* This font has character metrics *)

        (* Compute join of bounding boxes *)
        res := Rect.Empty;
        VAR rp := 0; BEGIN
          FOR i := 0 TO LAST(txt) DO
            WITH cm = GetCM(m, txt[i], valid) DO
              res := Rect.Join(res, Rect.MoveH(cm.boundingBox, rp));
              INC(rp, cm.printWidth)
            END
          END
        END
      ELSE
        (* All characters have the same "printWidth" and "boundingBox", which
           are stored in "m.charMetrics.maxBounds". *)

        (* Set "valid" if some characters may be invalid, and set "len"
           to the number of valid characters. *)
        VAR fc, lc: INTEGER; BEGIN
          IF NOT AllCharsValid(m, fc, lc) THEN
            VAR validCnt := 0; ch: INTEGER; BEGIN
              FOR i := 0 TO len - 1 DO
          	ch := ORD(txt[i]);
          	IF fc <= ch AND ch <= lc THEN INC(validCnt) END
              END;
              valid := (len = validCnt);
              len := validCnt
            END
          END
        END;

        (* Compute the bounding box *)
        IF len = 0 THEN RETURN Rect.Empty END;
        WITH max = m.maxBounds, pw = max.printWidth, box = max.boundingBox DO
          res := box;
          IF len > 1 THEN
            res := Rect.Join(res, Rect.MoveH(box, pw * (len - 1)))
          END
        END
      END;
      RETURN res
    END
  END TightBoundingBoxSubValid;

CONST EmptyCM = ScrnFont.CharMetric{
  boundingBox := Rect.Empty, printWidth := 0};

PROCEDURE GetCM(
  m: ScrnFont.Metrics;
  ch: CHAR;
  VAR (*INOUT*) valid: BOOLEAN)
  : ScrnFont.CharMetric =
  CONST SpaceCode = ORD(' '); Char240 = 8_240;
  VAR c := ORD(ch); BEGIN
    WITH fc = m.firstChar, lc = m.lastChar DO
      IF c < fc OR c > lc THEN
    	c := m.defaultChar;
    	IF c < fc OR c > lc THEN
    	  valid := FALSE;
    	  RETURN EmptyCM
    	END
      END;
      IF c # SpaceCode AND c # Char240 THEN
        RETURN m.charMetrics[c - fc]
      ELSE
        (* The space character should have an empty bounding box, but the X
           font definition mistakenly gives it a width and height of 1. The
           same is true for character 8_240 in all fonts. *)
        RETURN ScrnFont.CharMetric{boundingBox := Rect.Empty,
          printWidth := m.charMetrics[c - fc].printWidth}
      END
    END
  END GetCM;

PROCEDURE AllCharsValid(m: ScrnFont.Metrics; VAR (*OUT*) fc,lc: INTEGER):
  BOOLEAN =
(* Return "TRUE" iff all characters must be valid, that is, if "m" is defined
   on all characters, or if its default character is defined. This also has
   the side-effect of setting "fc" and "lc" to the codes of "m"'s first and
   last defined characters. *)
  BEGIN
    fc := m.firstChar;
    lc := m.lastChar;
    WITH dc = m.defaultChar DO
      RETURN (fc <= ORD(FIRST(CHAR)) AND ORD(LAST(CHAR)) <= lc)
          OR (fc <= dc AND dc <= lc)
    END
  END AllCharsValid;

BEGIN
END ScrnFontExtras.

