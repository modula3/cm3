(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Thu Jun 11 21:28:03 1992 by steveg   *)

INTERFACE GenericSubTree;

(* support interface for GenericSubTreeSelector (generic module) *)

IMPORT
  GenericTree;

TYPE
  T = GenericTree.SubTree;

END GenericSubTree.
