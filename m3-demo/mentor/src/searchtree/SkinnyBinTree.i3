(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 30 12:05:57 PDT 1992 by heydon                   *)

INTERFACE SkinnyBinTree;

IMPORT BinaryTree;

TYPE
  T <: TPublic;
  TPublic = BinaryTree.T BRANDED OBJECT
    dyAbove: REAL := 0.0		 (* extra distance above parent *)
  END;

END SkinnyBinTree.
