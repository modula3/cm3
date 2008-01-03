(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:38:44 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:15 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE TextLineDLE;

IMPORT DisplayList, DPS, DPSWindow, Text, Fmt;

CONST xMargin = 5.0;
CONST yMargin = 3.0;
CONST fontHeight = DPS.StandardFontPoints;
CONST fontDescender = 2.0;

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*> only: REFANY := NIL): TEXT =
 VAR data: TEXT := "";
  BEGIN
  IF NOT DPS.BoxesIntersect (e.box, box) THEN RETURN NIL; END;
  IF e.hasInputFocus THEN data := DPS.EdgedBoxClipAndPaint (e.box, 0.25); 
   ELSE data := DPS.EdgedBoxClipAndPaint (e.box);
    END;
  data := data & " " 
    & Fmt.Real(e.box.low.x + xMargin) & " "
    & Fmt.Real(e.box.low.y + yMargin + fontDescender) & " moveto " 
    & " (" & DPS.EscapeText(e.text) & ") " (* If 'text' contains parens? *)
    & " ButtonDLEFont setfont 0.0 setgray show ";
  RETURN " gsave " & data & " grestore ";
  END Repaint;

PROCEDURE Init (e: E; window: DPSWindow.T) =
 VAR textWidth: REAL;
  BEGIN
  IF e.initialized THEN RETURN; END;
  window.SendFoundation ( " /ButtonDLEFont /Times-Roman findfont "
    & Fmt.Real(fontHeight) & " scalefont def " ); (* Many times alas. *)
  textWidth := DPS.TextWidth (e.text, window, "ButtonDLEFont");
  IF e.box.high.x <= e.box.low.x THEN (* Else user supplied width. *)
    e.box.high.x := e.box.low.x + xMargin + textWidth + xMargin; 
    END;
  e.box.high.y := e.box.low.y + yMargin + fontHeight + yMargin;
  e.initialized := TRUE;
  END Init;

PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
 VAR affected: DisplayList.E;
  BEGIN
  Init (e, window);
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF in THEN
    IF event.clickType = DPS.ClickType.LastUp THEN
      IF e.hasInputFocus THEN RETURN TRUE; END;
      e.GetInputFocus (NIL);
      e.hasInputFocus := TRUE;
      affected := e.MoveToLast();
      IF affected=NIL THEN affected := e; END;
      affected.Dirty (affected.box, affected);
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN =
  BEGIN
  Init (e, window);
  IF e.hasInputFocus THEN 
    IF char = '\n' THEN 
      e.Proc (window, e.text, char);
     ELSIF char = '\010' THEN
      IF Text.Length(e.text) > 0 THEN
        e.text := Text.Sub (e.text, 0, Text.Length(e.text) - 1);
        e.Dirty (e.box); (* Do not assume it is on top. *)
        END;
     ELSE 
      e.text := e.text & Text.FromChar(char);
      e.Dirty (e.box); (* Do not assume it is on top. *)
      END;
    END;
  RETURN e.hasInputFocus;
  END Char;

PROCEDURE LoseInputFocus (e: E) =
  BEGIN
  IF e.hasInputFocus THEN 
    e.hasInputFocus := FALSE; 
    e.Dirty (e.box); (* It may not be on top now! *)
    END;
  END LoseInputFocus;

PROCEDURE Proc (<*UNUSED*> e: E;
                <*UNUSED*> window: DPSWindow.T;
                <*UNUSED*> text: TEXT;
                <*UNUSED*> char: CHAR) =
  BEGIN
  END Proc;

  BEGIN
  END TextLineDLE.

