(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 09:04:20 PST 1994 by kalsow    *)

INTERFACE SilFont;

IMPORT SilWindow, SilWr, SilRd, WinDef;

TYPE T <: REFANY;

PROCEDURE New (name: TEXT;  height, weight: INTEGER;  italics: BOOLEAN): T;

PROCEDURE Write (t: T;  wr: SilWr.T): INTEGER;
(* writes a description of 't' to 'wr' and returns its index
   in the list of fonts written to 'wr'. *)

PROCEDURE FromID (rd: SilRd.T;  id: INTEGER): T;
(* returns the font with index 'id' in reader 'rd'. *)

PROCEDURE Select (t: T;  w: SilWindow.T);
(* make 't' the current font in window 'w' *)

PROCEDURE BuildMenu (msg: INTEGER): WinDef.HMENU;
(* build and return a pop-up menu with event ids  msg, msg+1, ... *)

PROCEDURE FromMenu (index: INTEGER): T;
(* return the font with index 'x' in the menu *)

PROCEDURE Reset ();
(* release the windows resources associated with any fonts created here *)

END SilFont.


