(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May  3 12:54:49 PDT 1994 by najork                   *)
(*      modified on Wed Aug  5 10:48:08 PDT 1992 by heydon                   *)

INTERFACE BSTView;

(* Views for binary search trees. *)

IMPORT STView, BinaryTree, PaintOp, RefList;

TYPE
  T <: TPublic;
  TPublic = STView.T OBJECT
    current: BinaryTree.T;		 (* current node *)
    comp_list: RefList.T;		 (* list of CompareElem's *)
    del_node_color: PaintOp.ColorScheme; (* orig color of deleted node *)
  END;

VAR (* READONLY *)
  red, black: PaintOp.ColorScheme;	 (* red, black node colors *)
  redBg, blackBg: PaintOp.ColorScheme;	 (* red, black edge colors *)
  whiteRed, whiteBlack: PaintOp.ColorScheme;

END BSTView.
