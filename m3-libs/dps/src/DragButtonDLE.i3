(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:51:37 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:28 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE DragButtonDLE;  

IMPORT ButtonDLE, DPS, DPSWindow;

TYPE E = ButtonDLE.E OBJECT 
 METHODS
  Down (t: DPSWindow.T; event: DPS.MouseEvent) := Down;
  Move (t: DPSWindow.T; event: DPS.MouseEvent) := Move;
  Up (t: DPSWindow.T; event: DPS.MouseEvent) := Up;
 OVERRIDES
  Mouse := Mouse;
  END;

PROCEDURE Init (e: E; t: DPSWindow.T);

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Down (e: E; t: DPSWindow.T; event: DPS.MouseEvent); 
PROCEDURE Move (e: E; t: DPSWindow.T; event: DPS.MouseEvent); 
PROCEDURE Up (e: E; t: DPSWindow.T; event: DPS.MouseEvent); 

  END DragButtonDLE.



