(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:31:25 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 11:33:12 PST 1995 by najork                   *)


INTERFACE WinScrnColorMap;

IMPORT ScrnColorMap, WinDef;

PROCEDURE NewOracle (): ScrnColorMap.Oracle;

PROCEDURE DefaultPalette (): WinDef.HPALETTE;

END WinScrnColorMap.
