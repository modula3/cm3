(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Wed May 20 00:46:17 1992 by steveg *)

INTERFACE BinaryTree;

<* PRAGMA LL *>

IMPORT GenericTree;

TYPE
  V <: GenericTree.V;
  T <: PublicT;
  PublicT = GenericTree.GenericTree OBJECT
    l, r: T;
  METHODS
    <* LL = v.mu *>
    set(v: V; lr: LR; ch: T);
  END;

  LR = {Left, Right};

END BinaryTree.
