(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:08:17 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:14 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE TranslateDLE;

IMPORT DisplayList, DPS, DPSWindow, Fmt;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT =
 VAR text: TEXT := "";
  BEGIN
  IF (t.translationX = 0.0) AND (t.translationY = 0.0) THEN 
    RETURN DisplayList.Repaint (t, box, only);
    END;
  box.low.x := box.low.x - t.translationX;
  box.low.y := box.low.y - t.translationY;
  box.high.x := box.high.x - t.translationX;
  box.high.y := box.high.y - t.translationY;
  text := DisplayList.Repaint (t, box, only);
  RETURN Xlate (t, text);
  END Repaint;
 
PROCEDURE Xlate ( t: T; text: TEXT ): TEXT =
  BEGIN
  RETURN
    " gsave "
    & Fmt.Real(t.translationX) & " " 
    & Fmt.Real(t.translationY) & " translate " 
    & text 
    & " grestore ";
  END Xlate;

PROCEDURE PostScriptToParentTranslated (e: E; script: TEXT) =
 VAR p: DisplayList.T;
  BEGIN
  p := e.parent; 
  IF p # NIL THEN
    p.ImmediatePostScript (Xlate (e, script));
    END;
  END PostScriptToParentTranslated;

PROCEDURE DirtyToParentTranslated (e: E; box: DPS.Box; only: DisplayList.T := NIL) =
 VAR p: DisplayList.T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN 
    box.low.x := box.low.x + e.translationX;
    box.low.y := box.low.y + e.translationY;
    box.high.x := box.high.x + e.translationX;
    box.high.y := box.high.y + e.translationY;
    p.Dirty (box, only); 
    END;
  END DirtyToParentTranslated;

PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: DisplayList.E;
  BEGIN
  IF t.useMouseRight
   AND (event.whatChanged = DPS.Button.Right) 
   AND ((NOT t.onlyIfShifted) OR (DPS.Modifier.Shift IN event.modifiers)) THEN
    CASE event.clickType OF
    | DPS.ClickType.FirstDown => t.mouseDownPlace := event.place; 
    | DPS.ClickType.LastUp => 
       IF NOT t.fixedX THEN
         t.translationX := t.translationX + event.place.x - t.mouseDownPlace.x;
         END;
       IF NOT t.fixedY THEN
         t.translationY := t.translationY + event.place.y - t.mouseDownPlace.y;
         END;
       t.Dirty (DPS.EverywhereBox, NIL);
     ELSE RETURN TRUE;
      END; (* of CASE *)
    RETURN TRUE;
    END; (* of IF t.useMouseRight ... *)

  event.place.x := event.place.x - t.translationX;
  event.place.y := event.place.y - t.translationY;

  e := t.Last();
  WHILE e # NIL DO 
    IF e.Mouse (window, event) THEN RETURN TRUE; END; 
    e := e.Previous(); 
    END;

  RETURN FALSE;
  END Mouse;

  BEGIN

  END TranslateDLE.

