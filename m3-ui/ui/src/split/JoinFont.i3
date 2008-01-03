(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Nov 10 13:40:26 PST 1992 by msm     *)
<*PRAGMA LL*>

INTERFACE JoinFont;

IMPORT ScrnFont, JoinScreen, Palette, Font;

TYPE
  Oracle <: ScrnFont.Oracle;

PROCEDURE New(st: JoinScreen.T): Oracle;

PROCEDURE Apply (st: JoinScreen.T; cl: Palette.FontClosure; fnt: Font.T):
  ScrnFont.T;

END JoinFont.
