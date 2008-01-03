(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Nov 23 12:27:59 PST 1992 by meehan                   *)
(*      modified on Tue Jun 16 13:16:24 PDT 1992 by muller                   *)
(*      modified on Fri Oct  7 14:32:06 1988 by chan                         *)
(*      modified on Mon Jul  6 09:56:11 1987 by brooks                       *)

INTERFACE MTextPrivate;

IMPORT MText, Rd;

REVEAL MText.T = BRANDED REF NodeRec;

VAR debug: BOOLEAN := FALSE;

TYPE
  Node = MText.T;
  (* To Modula, types Node and T are equal.  However, we use T to refer only
     to a top node, for the purpose of representing the whole MText.  Node
     refers to any sort of node.  Only T is exported, and it is opaque. *)

  NodeType = {tree, text, file, buf, anchor, top};
  (*
    tree: an interior node
    text: a leaf containing a TEXT
    file: a leaf referring to part of a file
    buf: a leaf containing a typein buffer
    anchor: (bookkeeping?)
    top: the header node, one per MText *)

  NodeRec =
    RECORD
      up     : Node       := NIL; (* parent *)
      id     : INTEGER;          (* FOR DEBUGGING *)
      length : CARDINAL   := 0;  (* number of characters in this subtree *)
      lock   : MUTEX;            (* for reading and updating the mtext *)
      root   : Node;             (* root of the tree *)
      height : CARDINAL;         (* height of the tree, 1 leaf => 0 *)
      bufNode: Node;             (* handy access to the mutable buffer *)
      bufMax : CARDINAL;         (* maximum size of mutable buffer *)
      version: CARDINAL   := 0;  (* is incremented each modification. *)

      (* Nodes change their type, so subtypes aren't appropriate. *)
      type: NodeType;

      (* NodeType.tree: *)
      left, right: Node;         (* the two children *)
      leftSize   : CARDINAL;     (* number of chars in left subtree *)
      sub        : BOOLEAN;
      (* TRUE if this is a right subnode of a node in the 2-3 tree we are
         modeling. *)

      (* NodeType.text: *)
      text := "";

      (* NodeType.buf: *)
      buffer: REF ARRAY OF CHAR;  (* a mutable buffer *)

      (* NodeType.file: *)
      file : Rd.T;
      start: CARDINAL;           (* where this piece starts in the file *)
    END;

PROCEDURE Check (VAR start, end: CARDINAL; actualLength: CARDINAL);
   
END MTextPrivate.
