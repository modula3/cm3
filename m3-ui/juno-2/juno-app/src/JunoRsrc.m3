(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Feb 16 14:58:55 PST 1994 by heydon                   *)

MODULE JunoRsrc;

IMPORT Rsrc, JunoBundle;

BEGIN
  Path := Rsrc.BuildPath("$JunoPATH", JunoBundle.Get());
END JunoRsrc.
