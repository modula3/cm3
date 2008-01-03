(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:39:10 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:18 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE SimpleTextDLE;

IMPORT DisplayList, DPS, DPSWindow, Text, Fmt;

CONST xMargin = 5.0;
CONST yMargin = 3.0;
CONST fontHeight = DPS.StandardFontPoints;
CONST fontDescender = 2.0;
CONST lineEscapement = 20.0;

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*> only: REFANY): TEXT =
 VAR lineCount: INTEGER;
 VAR data: TEXT := "";
  BEGIN
  IF NOT DPS.BoxesIntersect (e.box, box) THEN RETURN NIL; END;
  data := DPS.EdgedBoxClipAndPaint (e.box);
  lineCount := NUMBER (e.lines^);
  FOR line := 0 TO lineCount-1 DO
    data := data & " " 
      & Fmt.Real(e.box.low.x + xMargin) & " " 
      & Fmt.Real (e.box.low.y + yMargin + fontDescender
         + FLOAT(lineCount-line-1) * lineEscapement ) 
      & " moveto " 
      & " (" & e.lines[line] & ") " (* EscapeText called at Init time. *)
      & " ButtonDLEFont setfont 0.0 setgray show ";
    END;
  RETURN " gsave " & data & " grestore "; 
  END Repaint;

PROCEDURE Init (e: E; window: DPSWindow.T) =
  BEGIN
  IF e.initialized THEN RETURN; END;
  window.SendFoundation ( " /ButtonDLEFont /Times-Roman findfont "
    & Fmt.Real(fontHeight) & " scalefont def " ); (* Many times alas. *)
  Recalculate (e, window);
  e.initialized := TRUE;
  END Init;

PROCEDURE Recalculate (e: E; window: DPSWindow.T) =
 VAR width: REAL;
 VAR lineCount, line: INTEGER;
 VAR maxWidth: REAL;
 VAR c: CHAR;
  BEGIN
  lineCount := 1;
  FOR k := 0 TO Text.Length(e.text)-2 DO
    IF Text.GetChar(e.text,k)='\n' THEN lineCount := lineCount + 1; END;
    END;
  e.lines := NEW (REF ARRAY OF TEXT, lineCount);
  FOR k := 0 TO lineCount-1 DO e.lines[k] := ""; END;
  line := 0;
  FOR k := 0 TO Text.Length(e.text)-1 DO
    c := Text.GetChar(e.text,k);
    IF c='\n' THEN line := line + 1;
     ELSE e.lines[line] := e.lines[line] & Text.FromChar(c);
      END;
    END;
  maxWidth := 0.0;
  FOR k := 0 TO lineCount-1 DO 
    e.lines[k] := DPS.EscapeText(e.lines[k]); 
    width := DPS.TextWidth (e.lines[k], window, "ButtonDLEFont");
    IF width > maxWidth THEN maxWidth := width; END; 
    END;
  e.box.high.x := e.box.low.x + xMargin + maxWidth + xMargin;
  e.box.high.y := e.box.low.y + yMargin + fontHeight + yMargin 
   + FLOAT(lineCount-1) * lineEscapement;
  END Recalculate;

PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
 VAR affected: DisplayList.E;
  BEGIN
  Init (e, window);
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF in THEN
    IF event.clickType = DPS.ClickType.FirstDown THEN
      affected := e.MoveToLast();
      IF affected#NIL THEN affected.Dirty (affected.box, affected); END;
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE Replace (e: E; text: TEXT; window: DPSWindow.T := NIL) =
 VAR oldBox: DPS.Box;
  BEGIN
  e.text := text;
  IF window # NIL THEN (* Else better be before initialization. *)
    oldBox := e.box;
    Recalculate (e, window); 
    window.Dirty ( DPS.BoxUnion (oldBox, e.box), NIL ); 
    END;
  END Replace;

  BEGIN
  END SimpleTextDLE.

