(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Thu Jul 16 07:44:44 1992 by mhb      *)
(*      modified on Thu Jun 11 21:28:03 1992 by steveg   *)

INTERFACE MatchRect;

(* support interface for GenericSubTreeSelector (generic module) *)

IMPORT
  MG;

TYPE
  T = MG.Rectangle;

END MatchRect.

