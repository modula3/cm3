(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:19:42 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:00:34 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE VContainerDLE;  

IMPORT DisplayList, DPS, DPSWindow, Linked2Tree;

(* A VContainerDLE.T is a subclass of DisplayList.T. (And similarly
   for VContainerDLE.E.)

   A VContainerDLE.T arranges its children into a vertical column,
   origined at its own {x, y}, and sets its own width and height to
   circumscribe them. *)


TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  separation: REAL := 0.0;
 METHODS
  Rearrange() := Rearrange;
 OVERRIDES
  Prepend := Prepend;
  Append := Append;
  Remove := Remove;
  Repaint := Repaint;
  Mouse := Mouse;
  Char := Char;
  MakeChildLast := MakeChildLast;
  END;

TYPE E = T;

PROCEDURE Init (e: E; window: DPSWindow.T);

PROCEDURE Rearrange (e: E);

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN;

PROCEDURE Prepend (t: Linked2Tree.T; e: Linked2Tree.E);
PROCEDURE Append (t: Linked2Tree.T; e: Linked2Tree.E);
PROCEDURE Remove (e: Linked2Tree.E);

PROCEDURE MakeChildLast (t: Linked2Tree.T; e: Linked2Tree.E): Linked2Tree.E;

  END VContainerDLE.
