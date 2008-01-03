(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:18:35 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:48:18 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE ButtonDLE;  

IMPORT DisplayList, DPS, DPSWindow;

TYPE E = DisplayList.E OBJECT 
  initialized: BOOLEAN := FALSE;
  text: TEXT := ""; 
  textPlace: DPS.Place; hot: BOOLEAN := FALSE; 
 METHODS
  Proc (t: DPSWindow.T; event: DPS.MouseEvent) := Proc;
 OVERRIDES
  Repaint := Repaint;
  Mouse := Mouse;
  END;

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Proc (e: E; t: DPSWindow.T; event: DPS.MouseEvent); (* A no-op. *)

PROCEDURE Init (e: E; t: DPSWindow.T); 

  END ButtonDLE.



