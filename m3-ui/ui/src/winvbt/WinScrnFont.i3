(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jun 27 14:14:32 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:04:49 PST 1995 by najork                   *)


INTERFACE WinScrnFont;

IMPORT PaintPrivate, ScrnFont, WinDef;

PROCEDURE NewOracle (): ScrnFont.Oracle;
(* Create a ScrnFont.Oracle suitable for accessing Windows fonts. *)

PROCEDURE FromFont (font: PaintPrivate.Font): WinDef.HFONT;

END WinScrnFont.
