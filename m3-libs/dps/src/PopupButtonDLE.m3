(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:01:12 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:21 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


(*UNSAFE*) (* For procedure=NIL test *)
MODULE PopupButtonDLE;

IMPORT DPS, DPSWindow, Fmt, wraps;

CONST xMargin = 5.0;
CONST yMargin = 3.0;
CONST fontHeight = DPS.StandardFontPoints;
CONST fontDescender = 0.0;

CONST itemsBelowButton = 1.0;

CONST grayStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)
CONST colorStrokeWidthText = "4.0"; (* Only 'inside' paints, due to clip. *)

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*> only: REFANY := NIL): TEXT =
 VAR data: TEXT := "";
  BEGIN
  IF DPS.BoxesIntersect (e.box, box) THEN data := DPSForButton(e); END;
  IF e.hot AND DPS.BoxesIntersect (e.itemBox, box) THEN
    data := data & DPSForItemBox (e);
    END;
  RETURN " gsave " & data & " grestore ";
  END Repaint;

PROCEDURE DPSForButton (e: E): TEXT =
 VAR data: TEXT;
  BEGIN
  data := PushBoxCoords (e.box) & Fmt.Real(DPS.StandardFontPoints)
   & " 0.5 mul ButtonDLEDrawRoundedPath "; 
  IF e.hot THEN data := data & " clip " 
     & " 0.0 0.5 0.95 sethsbcolor gsave fill grestore "
     & " 0.0 1.0 0.5 sethsbcolor "
     & colorStrokeWidthText & " setlinewidth stroke ";
   ELSE data := data & " clip 0.95 setgray gsave fill grestore "
     & "0.5 setgray " & grayStrokeWidthText & " setlinewidth stroke ";
    END; 
  data := data 
    & Fmt.Real(e.box.low.x + xMargin) & " " 
    & Fmt.Real(e.box.low.y + yMargin + fontDescender) & " moveto " 
    & " (" & e.text & ") " 
    & " ButtonDLEFont setfont 0.0 setgray show ";
  RETURN data;
  END DPSForButton;

PROCEDURE DPSForItemBox (e: E): TEXT =
 VAR data: TEXT;
  BEGIN
  data := PushBoxCoords (e.itemBox) 
   & Fmt.Real(DPS.StandardFontPoints) 
   & " 0.5 mul ButtonDLEDrawRoundedPath "
   & " clip 0.95 setgray gsave fill grestore "
   & "0.5 setgray " & grayStrokeWidthText & " setlinewidth stroke ";
  IF e.items # NIL THEN FOR k := 0 TO NUMBER(e.items^)-1 DO
    data := data & RewriteItemText (e, e.items^[k]);
    END; END;
  RETURN data;
  END DPSForItemBox;

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

PROCEDURE RecalculateItems (e: E; t: DPSWindow.T) =
 VAR height, width, maxWidth: REAL;
  BEGIN
  IF e.items=NIL THEN e.itemBox := DPS.ZeroBox; RETURN; END;
  IF NUMBER(e.items^) < 1 THEN RETURN; END;
  e.itemBox.low.x := e.box.low.x;
  e.itemBox.high.y := e.box.low.y - itemsBelowButton;
  e.itemBox.low.y := e.itemBox.high.y 
   - ( FLOAT(NUMBER(e.items^)) * fontHeight + yMargin + yMargin );
  maxWidth := 0.0;
  FOR k := 0 TO NUMBER(e.items^)-1 DO
    wraps.Stringwidth (t.ctx, "ButtonDLEFont", e.items^[k].text, height, width);
    e.items^[k].textPlace.x := e.itemBox.low.x + xMargin;
    e.items^[k].textPlace.y := e.itemBox.high.y 
      - ( yMargin + FLOAT(k+1)*fontHeight - fontDescender );
    maxWidth := MAX (maxWidth, width);
    END;
  e.itemBox.high.x := e.itemBox.low.x + maxWidth + xMargin + xMargin;
  END RecalculateItems;

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
  e.box.high.x := e.box.low.x + xMargin + width + xMargin;
  e.box.high.y := e.box.low.y + yMargin + fontHeight + yMargin;
  e.text := DPS.EscapeText(e.text); (* Fixed, unexaminable, only have to convert once. *)
  e.initialized := TRUE;
  END Init;

PROCEDURE ItemMoused (e: E; <*UNUSED*> t: DPSWindow.T; place: DPS.Place): Item =
 VAR box:DPS.Box;
  BEGIN
  IF e.items#NIL THEN FOR k := 0 TO NUMBER(e.items^)-1 DO
    box := DPS.Box { 
      DPS.Place {e.itemBox.low.x, e.items^[k].textPlace.y}, 
      DPS.Place {e.itemBox.high.x, e.items^[k].textPlace.y + fontHeight} };
    IF DPS.PlaceIsInBox (place, box) THEN RETURN e.items^[k]; END;
    END; END;
  RETURN NIL;
  END ItemMoused;

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
 VAR new, old: Item;
 VAR possibleNewItems: REF ARRAY OF Item;
 VAR fudgedBox: DPS.Box;
  BEGIN
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF e.hot THEN (* We handle everything! *)
    IF event.clickType = DPS.ClickType.LastUp THEN
      e.hot := FALSE;
      fudgedBox := e.itemBox; fudgedBox.low.y := fudgedBox.low.y - 1.0;
      (* There may be something wrong with DPS clipping. *)
      e.Dirty (fudgedBox, NIL); 
      e.Dirty (e.box, e);
      new := ItemMoused (e, t, event.place);
      IF new # NIL (* Can no longer test method#NIL. *) THEN new.Proc(); END;
     ELSE (* Not LastUp. Keep correct item highlighted. *)
      new := ItemMoused (e, t, event.place);
      old := NIL;
      IF e.items # NIL THEN 
        LOOP FOR k := 0 TO NUMBER(e.items^)-1 DO
          IF e.items^[k].hot THEN old := e.items^[k]; EXIT; END;
          END; EXIT; END;
        END;
      IF new # old THEN
        IF old # NIL THEN old.hot := FALSE; RewriteItem(e, old); END;
        IF new # NIL THEN new.hot := TRUE; RewriteItem(e, new); END;
        END;
      END;
    RETURN TRUE; (* We handled it! *)
    END;
  IF in THEN
    IF event.clickType = DPS.ClickType.FirstDown THEN
      (* Can no longer use a NIL value of a method, alas .. 28jan92 *)
      (*
      IF e.Proc # ProcIsReallyNIL THEN
        e.items := e.Proc(t); 
        RecalculateItems(e, t);
       ELSIF e.itemBox.low.x=0.0 THEN RecalculateItems(e, t);
        END;
      *)
      possibleNewItems := e.Proc(t); 
      IF possibleNewItems # e.items THEN
        e.items := e.Proc(t); 
        RecalculateItems(e, t);
       ELSIF e.itemBox.low.x=0.0 THEN RecalculateItems(e, t);
        END;
      e.hot := TRUE;
      IF e.items#NIL THEN FOR k := 0 TO NUMBER(e.items^)-1 DO
        e.items^[k].hot := FALSE;
        END; END;
      EVAL e.MoveToLast();
      e.ImmediatePostScript ( " gsave " & DPSForButton(e) & " grestore ");
      e.ImmediatePostScript ( " gsave " & DPSForItemBox(e) & " grestore ");
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE ProcIsReallyNIL (e: E; <*UNUSED*>t: DPSWindow.T): REF ARRAY OF Item =
  BEGIN
  RETURN e.items;
  END ProcIsReallyNIL; 

  BEGIN
  END PopupButtonDLE.

