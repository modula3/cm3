(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 09:09:06 PST 1994 by kalsow    *)

MODULE SilPen;

IMPORT WinGDI, WinDef;
IMPORT SilObject, SilWindow;

PROCEDURE Push (t: SilObject.T; sel: BOOLEAN;
                width: INTEGER; w: SilWindow.T): WinDef.HGDIOBJ =
  CONST Style = ARRAY BOOLEAN OF INTEGER { WinGDI.PS_SOLID, WinGDI.PS_DOT };
  CONST Color = ARRAY BOOLEAN OF INTEGER { 0, 16_ff };
  VAR new: WinDef.HPEN;
  BEGIN
    sel := sel OR (t.state = SilObject.Selected);
    new := WinGDI.CreatePen (Style[sel], width, Color[sel]);
    RETURN WinGDI.SelectObject (w.dc, new);
  END Push;

PROCEDURE Pop (old: WinDef.HGDIOBJ;  w: SilWindow.T) =
  BEGIN
    EVAL WinGDI.DeleteObject (WinGDI.SelectObject (w.dc, old));
  END Pop;

BEGIN
END SilPen.

