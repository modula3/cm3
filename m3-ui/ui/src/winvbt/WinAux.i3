(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jan 18 14:28:38 PST 1995 by najork                   *)
(*       Created on Wed Jan 18 14:21:25 PST 1995 by najork                   *)


INTERFACE WinAux;

IMPORT VBT, WinDef;

PROCEDURE WindowHandle (v: VBT.T): WinDef.HWND;
(* Return the window handle associated with a VBT (or NIL) *)

END WinAux.
