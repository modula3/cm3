(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:52:29 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:02:03 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE TranslateDLE;

IMPORT DisplayList, DPS, DPSWindow;

(* A TranslateDLE.T is a subclass of a DisplayList.T. (And
   similarly with TranslateDLE.E.)  

   It effectively translates its children by overriding the
   Mouse and Repaint methods, offsetting the x and y coordinates
   as they are passed through.

   The current translation amount (in the Display PostScript
   coordinate system, of course), is stored in the fields
   translationX and translationY.

   Additionally, the implementation allows the user to change the
   transformation by dragging with the right mouse button:

     If useMouseRight is TRUE, then the mouse can drag the coordinates.
     If onlyIfShifted is TRUE, then the mouse only performs this function
       if a shift key is down.  The fields fixeX and fixedY determine
       which coordinates may be altered by the user.

   The default values are
      fixedX, fixedY: BOOLEAN := FALSE;
      useMouseRight: BOOLEAN := TRUE; 
      onlyIfShifted: BOOLEAN := FALSE; 
   so normal use of the mouse will drag in both x and y. *)


TYPE T = DisplayList.T OBJECT 
  translationX, translationY: REAL := 0.0;
  fixedX, fixedY: BOOLEAN := FALSE;
  useMouseRight: BOOLEAN := TRUE; (* Utilize drag. *)
  onlyIfShifted: BOOLEAN := FALSE; (* Utilize drag. *)
  mouseDownPlace: DPS.Place; (* Remembering for drag. *)
 OVERRIDES
  Mouse := Mouse;
  Repaint := Repaint;
  ImmediatePostScript := PostScriptToParentTranslated;
  Dirty := DirtyToParentTranslated;
  END;
TYPE E = T;

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT;
PROCEDURE PostScriptToParentTranslated (e: E; script: TEXT);
PROCEDURE DirtyToParentTranslated (e: E; box: DPS.Box; only: DisplayList.T := NIL);
 
  END TranslateDLE.



