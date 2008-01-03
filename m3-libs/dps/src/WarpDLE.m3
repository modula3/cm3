(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:29:40 PDT 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:11 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE WarpDLE;

IMPORT DisplayList, DPS, DPSWindow, Fmt;

(*******************
PROCEDURE BoxFromXYWH (x, y: REAL; w, h: REAL := 0.0): DPS.Box =
 VAR box: DPS.Box;
  BEGIN
  box.low.x := x; box.high.x := x + w;
  box.low.y := y; box.high.y := y + h;
  RETURN box;
  END BoxFromXYWH;
*****************)

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT =
 VAR text: TEXT := "";
  BEGIN
  box.low.y := WarpY (t, box.low.y);
  box.high.y := WarpY (t, box.high.y);
  text := text & DisplayList.Repaint (t, box, only);
  RETURN Warp (t, text);
  END Repaint;
 
PROCEDURE WarpY (t: T; y: REAL): REAL = (* From mouse-ish coords to clients *)
  BEGIN
  IF t.multiplierWarpY = 1.0 THEN RETURN y; END;
  IF y < t.bottomWarpY THEN RETURN y / t.multiplierWarpY; END;
  RETURN (y - t.bottomWarpY) + (t.bottomWarpY / t.multiplierWarpY);
  END WarpY;
 
PROCEDURE WarpYToScreen (t: T; y: REAL): REAL = 
  BEGIN (* From client's view to screen *)
  IF t.multiplierWarpY = 1.0 THEN RETURN y; 
   ELSIF y < (t.bottomWarpY / t.multiplierWarpY) THEN 
    RETURN y * t.multiplierWarpY; 
   ELSE RETURN (y - t.bottomWarpY / t.multiplierWarpY) + t.bottomWarpY;
    END;
  END WarpYToScreen;

(*********
PROCEDURE WarpedHeight (t: T): REAL = 
  BEGIN
  IF t.multiplierWarpY = 1.0 THEN RETURN t.box.high.y; END;
  RETURN (t.box.high.y - t.bottomWarpY) + (t.bottomWarpY / t.multiplierWarpY);
  END WarpedHeight;
**********)
 
PROCEDURE Warp ( t: T; text: TEXT ): TEXT =
 VAR b1, b2: DPS.Box; 
 VAR yTranslation: REAL;
  BEGIN
  IF t.multiplierWarpY = 1.0 THEN RETURN text; END;
  b1.low.x := 0.0; b1.high.x := 10000.0;
  b1.low.y := 0.0; b1.high.y := t.bottomWarpY;
  b2.low.x := 0.0; b2.high.x := 10000.0;
  b2.low.y := t.bottomWarpY; b2.high.y := 10000.0;
  yTranslation := t.bottomWarpY / t.multiplierWarpY - t.bottomWarpY;
  RETURN
    DPS.GSaveAndClip (b2)
    & " 0.0 0.0 " & Fmt.Real(yTranslation) & " sub translate "
    & text 
    & DPS.GRestore ()
    & DPS.GSaveAndClip (b1)
    & " 1.0 " & Fmt.Real(t.multiplierWarpY) & " scale " 
    & text 
    & DPS.GRestore ();
  END Warp;

PROCEDURE PostScriptToParentWarped (e: E; script: TEXT) =
 VAR p: DisplayList.T;
  BEGIN
  p := e.parent; 
  IF p # NIL THEN
    p.ImmediatePostScript (Warp (e, script));
    END;
  END PostScriptToParentWarped;

PROCEDURE DirtyToParentWarped (e: E; box: DPS.Box; only: DisplayList.T := NIL) =
 VAR p: DisplayList.T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN 
    box.low.y := WarpYToScreen (e, box.low.y);
    box.high.y := WarpYToScreen (e, box.high.y);
    (*
    box.low.y := 0.0;
    box.high.y := 1000.0;
    *)
    p.Dirty (box, only); 
    END;
  END DirtyToParentWarped;

PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: DisplayList.E;
  BEGIN
  event.place.y := WarpY (t, event.place.y);

  e := t.Last();
  WHILE e # NIL DO 
    IF e.Mouse (window, event) THEN RETURN TRUE; END; 
    e := e.Previous(); 
    END;

  RETURN FALSE;
  END Mouse;

  BEGIN

  END WarpDLE.

