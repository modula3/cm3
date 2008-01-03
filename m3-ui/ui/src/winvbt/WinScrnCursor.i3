(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun  1 13:49:37 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:02:26 PST 1995 by najork                   *)


INTERFACE WinScrnCursor;

IMPORT ScrnCursor, WinScreenType;

PROCEDURE NewOracle (st: WinScreenType.T): ScrnCursor.Oracle;

PROCEDURE SetCursor (cs: ScrnCursor.T);

END WinScrnCursor.
