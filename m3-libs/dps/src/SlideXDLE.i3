(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:31:40 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:16 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE SlideXDLE;

IMPORT DisplayList, DisplayListStack, DPS, DPSWindow;
IMPORT Linked2Tree, PopupMenuDLE, Rd;

TYPE Page = RECORD
  fixed: DisplayList.T := NIL;
  visible: DisplayListStack.T := NIL;
  invisible: Linked2Tree.T := NIL;
  END;

TYPE T = DisplayList.T OBJECT 
  initialized: BOOLEAN := FALSE;
  window: DPS.T := NIL;
  canMouseChildren: BOOLEAN := FALSE;
  backgroundPopup: PopupMenuDLE.T;
  pages: ARRAY [0..9] OF Page;
  page: INTEGER := 0;
  lastPage: INTEGER := 0;
  minimumHeight, maximumHeight: REAL := 0.0;
 METHODS
  AppendFixed (e: Linked2Tree.E) := AppendFixed;
  AppendVariable (e: Linked2Tree.E) := AppendVariable;
  AllInvisible () := AllInvisible;
  NextVisible () := NextVisible;
 OVERRIDES
  Prepend := Prepend;
  Append := Append;
  InsertBefore := InsertBefore;
  InsertAfter := InsertAfter;
  MakeChildLast := MakeChildLast;
  MakeChildFirst := MakeChildFirst;
  Mouse := Mouse;
  Char := Char;
  Repaint := Repaint;
  END;
TYPE E = T;
TYPE R = T;

PROCEDURE Init (t: T; window: DPSWindow.T);

PROCEDURE AddPage ( t: T; window: DPSWindow.T; 
 data: Rd.T; page: INTEGER := 999999 );

PROCEDURE Prepend (t: T; e: Linked2Tree.E);
PROCEDURE Append (t: T; e: Linked2Tree.E);
PROCEDURE InsertBefore (t: T; e, before: Linked2Tree.E);
PROCEDURE InsertAfter (t: T; e, after: Linked2Tree.E);

PROCEDURE MakeChildLast (t: T; e: Linked2Tree.E): Linked2Tree.E;
PROCEDURE MakeChildFirst (t: T; e: Linked2Tree.E): Linked2Tree.E;

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN;
PROCEDURE Char (e: E; t: DPSWindow.T; char: CHAR): BOOLEAN;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT;
 
PROCEDURE AppendFixed (t: T; e: Linked2Tree.E);
PROCEDURE AppendVariable (t: T; e: Linked2Tree.E); (* = Append *)
  
PROCEDURE AllInvisible (t: T); 
PROCEDURE NextVisible (t: T);
  
  END SlideXDLE.



