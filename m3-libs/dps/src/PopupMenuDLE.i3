(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:50 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:20 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE PopupMenuDLE;

IMPORT DisplayList, DPS, DLWindow, DPSWindow;

TYPE Item = OBJECT 
  text: TEXT := ""; context: REFANY := NIL;
  textPlace: DPS.Place; hot: BOOLEAN := FALSE; 
 METHODS
  Proc ();
  END;

TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  hot: BOOLEAN := FALSE; 
  items: REF ARRAY OF Item;
 METHODS
  Popup (place: DPS.Place; window: DLWindow.T) := Popup;
 OVERRIDES
  Repaint := Repaint;
  Mouse := Mouse;
  END;

TYPE E = T;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;

PROCEDURE Init (t: T; window: DPSWindow.T); 

PROCEDURE Popup (t: T; place: DPS.Place; window: DLWindow.T); 

PROCEDURE ProcIsReallyNIL (i: Item); 

  END PopupMenuDLE.



