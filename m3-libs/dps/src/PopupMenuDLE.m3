(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:02:55 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:20 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE PopupMenuDLE;

IMPORT DLWindow, DPS, DPSWindow, Fmt;

CONST xMargin = 5.0;
CONST yMargin = 3.0;
CONST fontHeight = DPS.StandardFontPoints;
CONST fontDescender = 0.0;

CONST itemsBelowButton = 1.0;

CONST grayStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)
(*CONST colorStrokeWidthText = "4.0";*) (* Only 'inside' paints, due to clip. *)

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*> only: REFANY := NIL): TEXT =
 VAR data: TEXT := "";
  BEGIN
  IF DPS.BoxesIntersect (e.box, box) THEN 
    data := " gsave " 
     & PushBoxCoords (e.box) & Fmt.Real(DPS.StandardFontPoints) 
     & " 0.5 mul ButtonDLEDrawRoundedPath "
     & " clip 0.95 setgray gsave fill grestore "
     & "0.5 setgray " & grayStrokeWidthText & " setlinewidth stroke ";
    IF e.items#NIL THEN FOR k := 0 TO NUMBER(e.items^)-1 DO
      data := data & RewriteItemText (e, e.items^[k]);
      END; END;
    data := data & " grestore ";
    END;
  RETURN data;
  END Repaint;

PROCEDURE RewriteItemText (<*UNUSED*> e: E; i:Item): TEXT =
 VAR data: TEXT;
  BEGIN
  data := " " & Fmt.Real(i.textPlace.x) & " " 
    & Fmt.Real(i.textPlace.y) & " moveto " 
    & " (" & i.text & ") " 
    & " ButtonDLEFont setfont ";
  IF i.hot THEN data := data & " 0.0 1.0 1.0 sethsbcolor show ";
   ELSE data := data & " 0.5 setgray show "
    END;
  RETURN data;
  END RewriteItemText;

PROCEDURE RewriteItem (e: E; i:Item) =
  BEGIN
  e.ImmediatePostScript ( " gsave " & RewriteItemText(e, i) & " grestore " );
  END RewriteItem;

PROCEDURE PushBoxCoords (box: DPS.Box): TEXT =
  BEGIN
  RETURN " " & Fmt.Real(box.low.x) & " " & Fmt.Real(box.low.y) & " " 
   & Fmt.Real(box.high.x) & " " & Fmt.Real(box.high.y) & " ";
  END PushBoxCoords;

PROCEDURE RecalculateItems (e: E; place: DPS.Place; t: DPSWindow.T) =
 VAR width, maxWidth: REAL;
  BEGIN
  IF e.items=NIL THEN e.box := DPS.ZeroBox; RETURN; END;
  IF NUMBER(e.items^) < 1 THEN RETURN; END;
  e.box.high.y := place.y - itemsBelowButton;
  e.box.low.y := e.box.high.y 
   - ( FLOAT(NUMBER(e.items^)) * fontHeight + yMargin + yMargin );
  maxWidth := 0.0;
  FOR k := 0 TO NUMBER(e.items^)-1 DO
    width := DPS.TextWidth (e.items^[k].text, t, "ButtonDLEFont");
    e.items^[k].textPlace.y := e.box.high.y 
      - ( yMargin + FLOAT(k+1)*fontHeight - fontDescender );
    maxWidth := MAX (maxWidth, width);
    END;
  e.box.low.x := place.x - (maxWidth/2.0 + xMargin);
  e.box.high.x := e.box.low.x + maxWidth + xMargin + xMargin;
  FOR k := 0 TO NUMBER(e.items^)-1 DO
    e.items^[k].textPlace.x := e.box.low.x + xMargin;
    END;
  END RecalculateItems;

PROCEDURE Init (e: E; t: DPSWindow.T) =
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
  (* Cannot calculate box until Popup() supplies a place. *)
  e.initialized := TRUE;
  END Init;

PROCEDURE ItemMoused (e: E; <*UNUSED*>t: DPSWindow.T; place: DPS.Place): Item =
 VAR box:DPS.Box;
  BEGIN
  IF e.items#NIL THEN FOR k := 0 TO NUMBER(e.items^)-1 DO
    box := DPS.Box { 
      DPS.Place {e.box.low.x, e.items^[k].textPlace.y}, 
      DPS.Place {e.box.high.x, e.items^[k].textPlace.y + fontHeight} };
    IF DPS.PlaceIsInBox (place, box) THEN RETURN e.items^[k]; END;
    END; END;
  RETURN NIL;
  END ItemMoused;

PROCEDURE Mouse (e: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
 VAR new, old: Item;
 VAR fudgedBox: DPS.Box;
  BEGIN
  Init (e, window);
  in := DPS.PlaceIsInBox (event.place, e.box);
  <* ASSERT e.hot *>
  IF event.clickType = DPS.ClickType.LastUp THEN
    (* Not necessary: we will Remove e.hot := FALSE; *)
    fudgedBox := e.box; 
    fudgedBox.low.y := fudgedBox.low.y - 1.0;
    fudgedBox.high.x := fudgedBox.high.x + 1.0;
    (* There ^^ is be something wrong with DPS clipping. *)
    e.Remove ();
    window.Dirty (fudgedBox, NIL); 
    new := ItemMoused (e, window, event.place);
    IF new # NIL (* Can no longer test Proc#NIL *) THEN new.Proc(); END;
   ELSE (* Not LastUp. Keep correct item highlighted. *)
    new := ItemMoused (e, window, event.place);
    old := NIL;
    IF e.items#NIL THEN 
      LOOP FOR k := 0 TO NUMBER(e.items^)-1 DO
        IF e.items^[k].hot THEN old := e.items^[k]; EXIT; END;
        END; EXIT; END;
      END;
    IF new # old THEN
      IF old # NIL THEN old.hot := FALSE; RewriteItem(e, old); END;
      IF new # NIL THEN new.hot := TRUE; RewriteItem(e, new); END;
      END;
    END;
  RETURN TRUE;
  END Mouse;

PROCEDURE Popup (t: T; place: DPS.Place; window: DLWindow.T) = 
  BEGIN
  (* Should assert that it is not being displayed. *)
  (* Client must do this if window changes: t.initialized := FALSE; *)
  Init (t, window); (* Could arrange to test for 'different window' *)
  FOR k := 0 TO NUMBER(t.items^)-1 DO t.items^[k].hot := FALSE; END;
  t.hot := TRUE; (* We are called with the button already down! *)
  RecalculateItems (t, place, window);
  window.displayList.Append (t);
  t.Dirty (t.box, t);
  END Popup;

PROCEDURE ProcIsReallyNIL (<*UNUSED*> i: Item) =
  BEGIN
  END ProcIsReallyNIL; 

  BEGIN
  END PopupMenuDLE.

