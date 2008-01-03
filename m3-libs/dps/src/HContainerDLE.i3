(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:19:53 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 18:03:11 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE HContainerDLE;  

(*  A HContainerDLE.T is a subclass of DisplayList.T. (And similarly
    for HContainerDLE.E.)

    A HContainerDLE.T arranges its children into a horizontal row,
    origined at its own {x, y}, and sets its own width and height to
    circumscribe them. *)

IMPORT DisplayList, DPS, DPSWindow, Linked2Tree;

TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  separation: REAL := 0.0;
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

PROCEDURE Initialize (e: E; window: DPSWindow.T);

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT;
PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN;

PROCEDURE Prepend (t: Linked2Tree.T; e: Linked2Tree.E);
PROCEDURE Append (t: Linked2Tree.T; e: Linked2Tree.E);
PROCEDURE Remove (e: Linked2Tree.E);

PROCEDURE MakeChildLast (t: Linked2Tree.T; e: Linked2Tree.E): Linked2Tree.E;

  END HContainerDLE.
