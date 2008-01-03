(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Nov 18 23:56:00 PST 1992 by msm     *)
<*PRAGMA LL*>

INTERFACE JoinCMap;

IMPORT ScrnColorMap, JoinScreen;

TYPE
  Oracle <: ScrnColorMap.Oracle;

PROCEDURE New(st: JoinScreen.T): Oracle;

END JoinCMap.
