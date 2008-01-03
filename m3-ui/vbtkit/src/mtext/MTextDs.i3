(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 30 19:31:15 1992 by mhb    *)
(*      modified on Tue Jun 16 13:16:26 PDT 1992 by muller *)
(*      modified on Fri Nov 22 19:58:54 PST 1991 by meehan *)
(* modified on Tue Jul 14 18:42:58 1987 by chan *)
(* Created on Thu Sep 4 13:13:00 1986 by chan *)

INTERFACE MTextDs;

FROM MText IMPORT T;
FROM MTextPrivate IMPORT Node;

PROCEDURE Locate (             mtext     : T;
                               index     : CARDINAL;
                  VAR (* out*) node      : Node;
                  VAR (* out*) localIndex: CARDINAL  );
(* Given an index into the mtext, returns the relevant node and the local
   index within the node. If the index is between nodes, then the node to
   the left is chosen; i.e., localIndex = node.length. *)

PROCEDURE LocateB (             mtext     : T;
                                index     : CARDINAL;
                   VAR (* out*) node      : Node;
                   VAR (* out*) localIndex: CARDINAL  );
(* Same as Locate except if the index is between nodes, then the node to
   the right is chosen; i.e., localIndex = 0. *)

PROCEDURE GetIndexOfNode (node: Node; localIndex: CARDINAL): CARDINAL
 ;
(* Returns the index with respect to the entire mtext. *)

PROCEDURE LeftNeighbor (node: Node): Node;
(* Find the nearest leaf to the left of node, which should be a leaf.
   Returns NIL if there isn't one. *)

PROCEDURE RightNeighbor (node: Node): Node;
(* Find the nearest leaf to the right of node, which is usually but not
   always a leaf. Returns NIL if there isn't one, which shouldn't happen,
   because there's no reason to be finding the right neighbor of the anchor
   node. *)

PROCEDURE InsertAt (node: Node; nodeIndex: CARDINAL; newnode: Node);

PROCEDURE InsertBefore (node, newnode: Node);
(* InsertBefore is called ONLY when InsertAfter will not work because there
   is nothing to insert the node before. It proceeds up a leftmost branch
   until it finds a node that is not leftmost; then it calls InsertAfter to
   do the rest of the job. *)

PROCEDURE InsertAfter (node, newnode: Node);
(* Internal procedure. Node is part of a balanced tree; newnode is a new,
   unattached node to be inserted in the tree after node. *)

PROCEDURE Remake (node, left, right: Node);
(* Change the child links of a node. Keeps the length fields of node, and
   the uplinks of its new children, correct. WARNING: it pays no attention
   to the "sub" field. *)

PROCEDURE Delete (VAR (* inout*) node: Node; b, e: CARDINAL);
(* node may change. *)

PROCEDURE RemoveNode (node: Node);
(* Removes a node (at any height) from the tree, keeping the tree balanced.
   Does not fix heads. Node must not be an ancestor of the anchor node,
   because the anchor node is never removed. The careful observer will note
   that each Remake under here abandons one interior node which is never
   cleaned out. That is all right, because all deleted leaves ARE cleaned
   out, and the abandoned node involves no cycles. *)

PROCEDURE SplitLeaf (node: Node; i: CARDINAL);
(* Break the node in two. Error if i = 0 OR i = node.length. *)

PROCEDURE ReplaceLeaf (old, new: Node);
(* Sometimes a leaf node must be replaced with an equivalent node of a
   different type. For example, a reader will not read a file node; it must
   be converted into a text node first. ReplaceLeaf assumes that old and
   new are both leaves and have the same length and the same content. It
   will link new into the place formerly occupied by old, and it will
   release any resources implicitly held by old. Heads which pointed into
   old will be fixed. *)

(***********************)
(* Buf node operations *)
(***********************)

PROCEDURE BufOpen (node: Node; point, size: CARDINAL);
(* Internal procedure to insert space in a buf node. We assume that caller
   will then put something reasonable into that space. Node must be a buf
   node, and node.length + size <= m.bufMax. *)

PROCEDURE Free (node: Node);
(* Free takes a subtree which is no longer needed and shreds it, setting
   all REFs to NIL. This makes it easy for the reference-counting garbage
   collector to clean up after us. *)

(************************)
(* File node operations *)
(************************)

VAR FileChunkSize: CARDINAL := 8192;
  (* How much of a file node we like to read in at a time. default = 8192.
     This a variable so that it can be made smaller during debugging. *)

PROCEDURE MoveBufTo (               m    : T;
                     VAR (* inout*) node : Node;
                     VAR (* inout*) nodeI: CARDINAL);
(* Places an empty buf node at <node,nodeI>; on return, node = bufnode AND
   nodeI = 0. *)


PROCEDURE ToText (VAR (* inout*) node: Node; all: BOOLEAN := TRUE);
(* Converts the node into a text node. If all is FALSE, at most
   FileChunkSize of a file node is converted into text. *)

(******************************)
(* Extracting text from nodes *)
(******************************)


PROCEDURE GetNodeText (VAR (* inout *) node : Node;
                                       begin: CARDINAL := 0;
                                       end  : CARDINAL := LAST (CARDINAL)):
  TEXT;
(* node changes if node.type = file. *)

END MTextDs.
