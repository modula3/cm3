(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug  5 00:59:04 PDT 1992 by heydon                   *)

INTERFACE RedBlackAlg;

TYPE
  NodeType = {Red, Black};

PROCEDURE NodeTypeToText(nt: NodeType): TEXT;
(* Returns the textual representation of "nt". *)

END RedBlackAlg.
