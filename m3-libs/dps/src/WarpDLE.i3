(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:52:49 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:11 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE WarpDLE;

IMPORT DisplayList, DPS, DPSWindow;

TYPE T = DisplayList.T OBJECT 
  leftWarpX, rightWarpX, multiplierWarpX: REAL := 1.0;
  topWarpY, bottomWarpY, multiplierWarpY: REAL := 1.0;
 OVERRIDES
  Mouse := Mouse;
  Repaint := Repaint;
  ImmediatePostScript := PostScriptToParentWarped;
  Dirty := DirtyToParentWarped;
  END;
TYPE E = T;

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT;
PROCEDURE PostScriptToParentWarped (e: E; script: TEXT);
PROCEDURE DirtyToParentWarped (e: E; box: DPS.Box; only: DisplayList.T := NIL);
 
  END WarpDLE.



