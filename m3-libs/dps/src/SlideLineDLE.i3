(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:57 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:18 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE SlideLineDLE;

IMPORT DisplayList, DPS, DPSWindow;

TYPE E = DisplayList.E OBJECT 
  initialized: BOOLEAN := FALSE;
  hasInputFocus: BOOLEAN := FALSE;
  text: TEXT := ""; hot: BOOLEAN := FALSE; 
  isPostscript: BOOLEAN; (* Calculated at Init() by peeking at text. *)
  aboveLeading: REAL := -1.0;
  height: REAL := -1.0;
  belowLeading: REAL := -1.0;
  leftLeading: REAL := -1.0;
  showPostScript: TEXT := "";
  fontName: TEXT;
  typefacePoints: REAL := -1.0; (* Real defaults in implementation. *)
  insertAfterIndex: INTEGER := 0; (* After zeroth is at start. *)
  togetherWithNext: BOOLEAN := FALSE; (* Should be in subclass? *)
  widths: REF ARRAY OF REAL := NIL;
  r, g, b: REAL:= 0.0;
 OVERRIDES
  Repaint := Repaint;
  Mouse := Mouse;
  Char := Char;
  LoseInputFocus := LoseInputFocus;
  END;

PROCEDURE Init (e: E; t: DPSWindow.T);

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (e: E; t: DPSWindow.T; char: CHAR): BOOLEAN;

PROCEDURE LoseInputFocus (e: E);

  END SlideLineDLE.



