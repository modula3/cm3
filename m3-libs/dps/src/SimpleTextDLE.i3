(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:19:36 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:04:18 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE SimpleTextDLE;

(*  A SimpleTextDLE.Eis a subclass of a DisplayList.E.  

    The SimpleTextDLE.E displays its "text" field using
    the default font.

    The text value may be changed by calling the Replace method. *)

IMPORT DisplayList, DPS, DPSWindow;

TYPE E = DisplayList.E OBJECT 
  initialized: BOOLEAN:= FALSE;
  text: TEXT := ""; lines: REF ARRAY OF TEXT
 METHODS
  Replace (text: TEXT; t: DPSWindow.T := NIL) := Replace;
 OVERRIDES
  Repaint := Repaint;
  Mouse := Mouse;
  END;

PROCEDURE Init (e: E; window: DPSWindow.T);

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Replace (e: E; text: TEXT; window: DPSWindow.T := NIL);

  END SimpleTextDLE.



