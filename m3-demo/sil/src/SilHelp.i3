(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 17:05:36 PST 1994 by kalsow    *)

INTERFACE SilHelp;

IMPORT WinDef;

PROCEDURE GetMenuEntry (entry: INTEGER): TEXT;
(* Returns the menu line for 'entry'.  If entry is less than zero
   or greater than the largest help entry, 'NIL' is returned. *)

PROCEDURE Show (wnd: WinDef.HWND;  entry: INTEGER);
(* Displays the text of 'entry' in a popup child of 'wnd' *) 

END SilHelp.
