(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSTextBounder.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE PSTextBounder;
IMPORT Rect;
IMPORT LinoText;
IMPORT TextReader;
IMPORT Scan;
IMPORT FloatMode;
IMPORT Text;
IMPORT TextRd;
IMPORT Lex;
IMPORT Rd;
IMPORT Bundle;
IMPORT PSFormBundle;
IMPORT Thread;
IMPORT WYSIWYGify;

<* FATAL Rd.Failure, TextReader.NoMore, FloatMode.Trap *>
<* FATAL Lex.Error, Thread.Alerted *>

REVEAL
  T = Public BRANDED "PSTextBounder" OBJECT
  OVERRIDES
    init := Init;
    bound := Bound;
  END;

VAR
  Widths: ARRAY CHAR OF INTEGER;
  FudgeFactor := 1000.0 / WYSIWYGify.ScanTextSizeFromPS("1000");

PROCEDURE Init(self: T): T = BEGIN RETURN self; END Init;

PROCEDURE Bound(<*UNUSED*>self: T; t: LinoText.T): Rect.T =
  CONST
    Ascent = 1.1;
    Descent = 0.2;
    SizeUnits = 1000.0;
  VAR
    width := 0;
    r: Rect.T;
    adjustedSize := FLOAT(t.size) * FudgeFactor;
  BEGIN
    <* ASSERT t.attach = LinoText.Attach.West *>
    FOR i := 0 TO Text.Length(t.t)-1 DO
      width := width + Widths[Text.GetChar(t.t, i)];
    END;
    r.west := 0;
    r.east := ROUND((FLOAT(width) * adjustedSize) / SizeUnits + 1.0);
    r.north := -TRUNC(adjustedSize * Ascent + 1.0);
    r.south := TRUNC(adjustedSize * Descent + 1.0);
    RETURN Rect.Move(r, t.a);
  END Bound;

PROCEDURE ReadWidths() =
  CONST
    Delims = " ";
  VAR
    rd := TextRd.New(Bundle.Get(PSFormBundle.Get(), "times.afm"));
    tr: TextReader.T;
    char, width: INTEGER;
  PROCEDURE Check(a, b: TEXT) =
    BEGIN
      <* ASSERT Text.Equal(a, b) *>
    END Check;
  BEGIN
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      Widths[i] := 0;
    END;
    TRY
      LOOP
        tr := NEW(TextReader.T).init(Rd.GetLine(rd));
        Check(tr.nextE(Delims), "C");
        char := Scan.Int(tr.nextE(Delims, TRUE));
        Check(tr.nextE(Delims), ";");
        Check(tr.nextE(Delims), "WX");
        width := Scan.Int(tr.nextE(Delims, TRUE));
        Widths[VAL(char, CHAR)] := width;
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
  END ReadWidths;

BEGIN
  ReadWidths();
END PSTextBounder.
