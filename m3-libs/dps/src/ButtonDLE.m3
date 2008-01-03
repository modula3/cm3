(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:57:38 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:33 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE ButtonDLE;

IMPORT DPS, DPSWindow, Fmt, wraps;

CONST xMargin = 5.0;
CONST yMargin = 3.0;
CONST fontHeight = DPS.StandardFontPoints;
CONST fontDescender = 2.0;

CONST grayStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)
CONST colorStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*> only: REFANY := NIL): TEXT =
 VAR data: TEXT := "";
  BEGIN
  IF NOT DPS.BoxesIntersect (e.box, box) THEN RETURN NIL; END;
  data := DPS.BoxCoordsAsText (e.box) & Fmt.Real(DPS.StandardFontPoints)
    & " 0.5 mul ButtonDLEDrawRoundedPath "; 
  IF e.hot THEN data := data & " clip " 
     & " 0.0 0.5 0.95 sethsbcolor gsave fill grestore "
     & " 0.0 1.0 0.5 sethsbcolor "
     & colorStrokeWidthText & " setlinewidth stroke ";
   ELSE data := data & " clip 0.95 setgray gsave fill grestore "
     & "0.5 setgray " & grayStrokeWidthText & " setlinewidth stroke ";
    END;
  data := data 
    & Fmt.Real(e.textPlace.x) & " " 
    & Fmt.Real(e.textPlace.y) & " moveto " 
    & " (" & e.text & ") " 
    & " ButtonDLEFont setfont 0.0 setgray show ";
  RETURN DPS.GSaveAndClip(box) & data & DPS.GRestore();
  END Repaint;

PROCEDURE Init (e: E; t: DPSWindow.T) =
 VAR height, width: REAL;
  BEGIN
  IF e.initialized THEN RETURN; END;
  t.SendFoundation ( " /ButtonDLEDrawCircledPath "
   & " { /highy exch def /highx exch def /lowy exch def /lowx exch def "
   & " /half highy lowy sub 2 div def "
   & " newpath lowx half add highy moveto "
   & " lowx half add lowy half add half 90 270 arc "
   & " highx half sub lowy lineto "
   & " highx half sub lowy half add half 270 90 arc "
   & " lowx half add highy lineto closepath } def " ); 
  t.SendFoundation ( " /ButtonDLEDrawRoundedPath "
   & " { /r exch def /highy exch def /highx exch def "
   & " /lowy exch def /lowx exch def "
   & " newpath lowx lowy r add moveto "
   & " lowx r add highy r sub r 180 90 arcn "
   & " highx r sub highy r sub r 90 0 arcn "
   & " highx r sub lowy r add r 0 270 arcn "
   & " lowx r add lowy r add r 270 180 arcn "
   & "  closepath } def " ); 
  t.SendFoundation ( " /ButtonDLEFont /Times-Roman findfont "
    & Fmt.Real(fontHeight) & " scalefont def " );
  wraps.Stringwidth (t.ctx, "ButtonDLEFont", e.text, height, width);
  e.textPlace.x := e.box.low.x + xMargin;
  e.textPlace.y := e.box.low.y + yMargin + fontDescender;
  e.box.high.x := e.box.low.x + xMargin + width + xMargin;
  e.box.high.y := e.box.low.y + yMargin + fontHeight + yMargin;
  e.text := DPS.EscapeText(e.text); 
  (* ^^ Fixed, unexaminable, only have to convert once. *)
  e.initialized := TRUE;
  END Init;  

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
  BEGIN
  Init(e, t);
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF e.hot THEN (* We handle everything! *)
    IF event.clickType = DPS.ClickType.LastUp THEN
      e.hot := FALSE;
      e.Dirty (e.box, e);
      IF in THEN e.Proc (t, event); END;
     ELSIF NOT in THEN
      e.hot := FALSE;
      e.Dirty (e.box, e);
      END;
    RETURN TRUE; (* We handled it! *)
    END;
  IF in THEN
    IF event.clickType = DPS.ClickType.FirstDown THEN
      e.hot := TRUE;
      EVAL e.MoveToLast();
      e.Dirty (e.box, e);
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE Proc (<*UNUSED*> e: E;
                <*UNUSED*> t: DPSWindow.T;
                <*UNUSED*> event: DPS.MouseEvent) =
  BEGIN
  END Proc;

  BEGIN
  END ButtonDLE.

