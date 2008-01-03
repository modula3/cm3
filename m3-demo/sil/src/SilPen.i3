(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 08:02:00 PST 1994 by kalsow    *)

INTERFACE SilPen;

IMPORT WinDef, SilObject, SilWindow;

PROCEDURE Push (t: SilObject.T;  sel: BOOLEAN;
                width: INTEGER;  w: SilWindow.T):  WinDef.HGDIOBJ;

PROCEDURE Pop  (old: WinDef.HGDIOBJ;  w: SilWindow.T);

END SilPen.

