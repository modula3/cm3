(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:51:52 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:32 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE DLWindow;  

IMPORT DisplayList, DPS, DPSWindow;

TYPE T = DPSWindow.T OBJECT 
   displayList: DisplayList.R;
  OVERRIDES 
   Paint := Paint; 
   Mouse := Mouse; 
   Char := Char;
   Key := Key;
   KillInputFocus := KillInputFocus;
  END; 

(* The first element in the list is the furthest from the user. *)
(* The list is painted in normal order and mouse-scanned in reverse order. *)

CONST BackgroundGray = 1.00;

PROCEDURE Paint (t: T; box: DPS.Box; only: REFANY);
PROCEDURE Mouse (t: T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (t: T; char: CHAR): BOOLEAN;

PROCEDURE Key (t: T; event: DPS.KeyEvent);

PROCEDURE KillInputFocus (t: T);

  END DLWindow.



