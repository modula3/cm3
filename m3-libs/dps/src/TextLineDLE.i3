(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:19:28 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:16 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE TextLineDLE;

IMPORT DisplayList, DPS, DPSWindow;

TYPE E = DisplayList.E OBJECT 
  initialized: BOOLEAN := FALSE;
  hasInputFocus: BOOLEAN := FALSE;
  text: TEXT := ""; hot: BOOLEAN := FALSE; 
 METHODS
  Proc (t: DPSWindow.T; text: TEXT; char: CHAR) := Proc;
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
PROCEDURE Proc (e: E; t: DPSWindow.T; text: TEXT; char: CHAR); (* A no-op. *)

PROCEDURE LoseInputFocus (e: E);

  END TextLineDLE.



