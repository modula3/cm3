(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:41 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:03:59 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE PopupButtonDLE;

(*  A PopupButtonDLE.T is a subclass of a DisplayList.T. (And
    similarly for a PopupButtonDLE.E.)

    The PopupButtonDLE.T normally paints itself as a "button"
    shape, displaying its text field.

    If the button is moused, then a popup list of commands is
    additionally displayed.  The command list is an array of
    PopupButtonDLE.Item; each Item has a text value and a Proc
    method.

    If the user lets the mouse button up while over an Item, the Proc
    method of the item is called.

    Appropriate visual feedback is provided as the mosue is moved. *)

IMPORT DisplayList, DPS, DPSWindow;

TYPE Item = OBJECT 
  text: TEXT := "";
  textPlace: DPS.Place; hot: BOOLEAN := FALSE; 
 METHODS
  Proc ();
  END;

TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  text: TEXT := ""; hot: BOOLEAN := FALSE; 
  items: REF ARRAY OF Item := NIL;
  itemBox: DPS.Box := DPS.Box { DPS.Place{0.0,0.0}, DPS.Place{0.0,0.0} };
 METHODS
  Proc (t: DPSWindow.T): REF ARRAY OF Item := ProcIsReallyNIL; 
  (* Now longer can compare methods against NIL ... 28jan92 *)
 OVERRIDES
  Repaint := Repaint;
  Mouse := Mouse;
  END;
(* If Proc is NIL then items is immutable. *)

TYPE E = T;

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;

PROCEDURE Init (e: E; t: DPSWindow.T); 

PROCEDURE ProcIsReallyNIL (e: T; t: DPSWindow.T): REF ARRAY OF Item; 

  END PopupButtonDLE.



