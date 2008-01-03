(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug  5 02:51:35 PDT 1992 by heydon                   *)

INTERFACE SkelView;

IMPORT BSTView;

TYPE
  T <: BSTView.T;

CONST
  ChildDx = 3.0;			 (* horiz sep between siblings *)
  ChildDy = 15.0;			 (* vertical sep betw parent/child *)
  NodeWidth = 6.0;
  NodeHeight = 6.0;

END SkelView.
