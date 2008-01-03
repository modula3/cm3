(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 11:10:33 PDT 1994 by heydon                   *)
(*      modified on Tue May  3 13:29:58 PDT 1994 by najork                   *)

INTERFACE BSTAlg;

(* Interface to types/procedures common to all binary search tree algorithms.
*)

IMPORT Random, SearchTreeAlgClass, VBT;

TYPE
  PanelData = OBJECT
    nodeCnt: INTEGER;			 (* number of nodes to add *)
    inputType: TEXT;			 (* input data type choice *)
    deleteType: TEXT;			 (* deletion data type choice *)
    rand: Random.T;			 (* random number generator *)
  END;

  Key = CARDINAL;			 (* key value *)
  Keys = REF ARRAY OF Key;		 (* node key array *)

  Node = OBJECT
    key: Key;
    index: INTEGER;			 (* for use by MG *)
    parent, left, right: Node := NIL
  END;

  Tree = OBJECT
    root: Node := NIL;
    nil:  Node := NIL;
  END;

  T = SearchTreeAlgClass.T BRANDED OBJECT 
    tree: Tree := NIL;
  END;

  Side = {Left, Right};

VAR
  OtherSide := ARRAY Side OF Side{Side.Right, Side.Left};

PROCEDURE GetPanelData(panel: VBT.T): PanelData;
(* Returns a new PanelData with field read from the panel "panel". *)

PROCEDURE NewKeys(data: PanelData; input := TRUE): Keys;
(* Return an array of "data.nodeCnt" new keys containing some permutation of
   the key values [1.."data.nodeCnt"]. If "data.inputType" = "rand", then the
   permutation is a random one; the seed for the random number generator is
   "data.seed" if "data.useSeed = TRUE", or a random seed otherwise. If
   "data.inputType" = "inc", then the permutation contains the keys in
   increasing order. If "data.inputType" = "dec", then the permutation
   contains the keys in decreasing order. IF "input" is false, then the string
   "data.deleteType" is used to determine how to build the resulting key array
   instead of "data.inputType". *)

PROCEDURE NewIndex(): INTEGER;
(* Returns a new, distinct node "index". *)

PROCEDURE GetChild(node: Node; side: Side): Node;
(* Returns "node.left" if "side = Side.Left"; returns "node.right" if "side =
   Side.Right". *)

PROCEDURE SetChild(node: Node; side: Side; val: Node);
(* Sets "node.left" to "val" if "side = Side.Left"; sets "node.right" to "val"
   if "side = Side.Right". *)

PROCEDURE Rotate(t: Tree; parent: Node; side: Side);
(* Rotates the "parent" node about it's "side" child, and updates the parent
   of "parent" (which may be "t.root") to point to the new parent. *)

END BSTAlg.
