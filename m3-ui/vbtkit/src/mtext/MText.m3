(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(*    Last modified on Mon Jan 30 14:46:08 PST 1995 by kalsow                *)
(*         modified on Mon Nov 23 12:33:01 PST 1992 by meehan                *)
(*         modified on Tue Jun 30 20:27:04 1992 by mhb                       *)
(*         modified on Tue Jun 16 13:16:27 PDT 1992 by muller                *)
(*         modified on Wed Jul  4 10:14:27 PDT 1990 by mcjones               *)
(*         modified On Thu May  4  9:23:45 PDT 1989 by mbrown                *)
(*         modified On Tue Oct 11 19:06:48 1988 by chan                      *)
(*         modified On Mon Dec  2 10:14:30 1985 by brooks                    *)

MODULE MText EXPORTS MText, MTextPrivate;

(* The Mutable Text package, an internal representation for text editors.
   To Do:
|  1) synchronization: it is not possible to have several readers
   reading the mtext concurrently. The reason is that a read to a file node
   causes the mtext data structure to change. *)

IMPORT MTextDs, Rd, Text, TextF, Thread;

(*********************************************************************)
(* Creating Mutable Texts *)
(*********************************************************************)

PROCEDURE New (t := ""; bufMax: CARDINAL := 256): T =
  VAR
    rootNode, anchorNode: Node;
    height              : CARDINAL;
    topnode             : T;
  BEGIN
    anchorNode := NEW (Node, type := NodeType.anchor,
                       length := 1 (* the "phantom character" *));
    IF Text.Empty (t) THEN
      rootNode := anchorNode;
      height := 0
    ELSE
      rootNode := NEW (Node, type := NodeType.tree, sub := FALSE);
      MTextDs.Remake (
        rootNode, NEW (Node, type := NodeType.text, length := Text.Length (t),
                       text := t), anchorNode);
      height := 1
    END;
    topnode :=
      NEW (T, type := NodeType.top, lock := NEW (MUTEX), version := 0,
           root := rootNode, length := rootNode.length - 1, height := height,
           bufNode := NEW (Node, type := NodeType.buf,
                           buffer := NEW (REF ARRAY OF CHAR, bufMax)),
           bufMax := bufMax);
    rootNode.up := topnode;
    RETURN topnode
  END New;

PROCEDURE ChangeBufMax (m: T; bufMax: CARDINAL) =
  BEGIN
    LOCK m.lock DO
      IF m.bufMax < bufMax THEN
        WITH b = NEW (REF ARRAY OF CHAR, bufMax) DO
          SUBARRAY (b^, 0, m.bufNode.length) :=
            SUBARRAY (m.bufNode.buffer^, 0, m.bufNode.length);
          m.bufNode.buffer := b
        END
      END;
      m.bufMax := bufMax
    END
  END ChangeBufMax;

PROCEDURE Close (m: T) =
  BEGIN
    Replace (m, 0, LAST (CARDINAL), "");
    LOCK m.lock DO
      m.bufNode.buffer := NIL;
      m.bufNode.up := NIL;
      m.root := NIL
    END
  END Close;

(*********************************************************************)
(*                    Range Check                                    *)
(*********************************************************************)

PROCEDURE Check (VAR start, end: CARDINAL; actualLength: CARDINAL) =
  BEGIN
    start := MIN (start, actualLength);
    end := MAX (start, MIN (end, actualLength))
  END Check;

(*********************************************************************)
(* Basic Editing Operations *)
(*********************************************************************)


PROCEDURE Replace (m: T; begin, end: CARDINAL; newtext: TEXT) =
  BEGIN
    ReplaceInternal (
      m, begin, end, SUBARRAY (newtext^, 0, NUMBER (newtext^) - 1), newtext)
  END Replace;

PROCEDURE ReplaceChars (         m         : T;
                                 begin, end: CARDINAL;
                        READONLY str       : ARRAY OF CHAR) =
  BEGIN
    ReplaceInternal (m, begin, end, str, NIL)
  END ReplaceChars;

PROCEDURE ReplaceInternal (         m         : T;
                                    begin, end: CARDINAL;
                           READONLY chars     : ARRAY OF CHAR;
                                    newtext   : TEXT           ) =
  VAR
    beginN, endN: Node;
    beginI, endI: CARDINAL;
    numChars               := NUMBER (chars);
  BEGIN
    LOCK m.lock DO
      Check (begin, end, m.length);
      MTextDs.Locate (m, begin, beginN, beginI);
      MTextDs.Locate (m, end, endN, endI);
      DeleteNodes (beginN, beginI, endN, endI, end - begin);
      IF numChars > 0 THEN
        (* It's not just a deletion. *)
        IF begin = end AND numChars <= m.bufMax THEN
          (* A simple insertion, and it will fit in the buf node. *)
          (* Get or adjust the buf node. *)
          IF beginN.type # NodeType.buf OR beginN.length + numChars > m.bufMax THEN
            MTextDs.MoveBufTo (m, beginN, beginI)
          END;
          MTextDs.BufOpen (beginN, beginI, numChars);
          SUBARRAY (beginN.buffer^, beginI, numChars) := chars
        ELSIF beginN.type = NodeType.buf
                AND beginN.length + numChars <= m.bufMax THEN
          (* It's a replacement, and it's entirely within the buf node. *)
          MTextDs.BufOpen (beginN, beginI, numChars);
          SUBARRAY (beginN.buffer^, beginI, numChars) := chars
        ELSE
          (* It's too big for the buf node, or it overlaps. *)
          IF newtext = NIL THEN newtext := Text.FromChars (chars) END;
          MTextDs.InsertAt (
            beginN, beginI, NEW (Node, type := NodeType.text, text := newtext,
                                 length := numChars))
        END
      END;
      INC (m.version)
    END
  END ReplaceInternal;

PROCEDURE ReplaceFile (m         : T;
                       begin, end: CARDINAL;
                       rd        : Rd.T;
                       start     : CARDINAL   := 0;
                       numChars  : CARDINAL   := LAST (CARDINAL)) =
  <* FATAL Rd.Failure, Thread.Alerted *>
  VAR
    beginN, endN: Node;
    beginI, endI: CARDINAL;
  BEGIN
    LOCK m.lock DO
      Check (begin, end, m.length);
      start := MIN (start, Rd.Length (rd));
      numChars := MIN (numChars, Rd.Length (rd) - start);
      MTextDs.Locate (m, begin, beginN, beginI);
      MTextDs.Locate (m, end, endN, endI);
      DeleteNodes (beginN, beginI, endN, endI, end - begin);
      MTextDs.InsertAt (
        beginN, beginI, NEW (Node, type := NodeType.file, length := numChars,
                             file := rd, start := start));
      INC (m.version)
    END
  END ReplaceFile;

PROCEDURE DeleteNodes (VAR (* inout*) firstN: Node;
                       VAR (* inout*) firstI: CARDINAL;
                                      lastN : Node;
                                      lastI : CARDINAL;
                                      size  : CARDINAL  ) =
  VAR node, parent: Node;
  BEGIN
    IF firstN # lastN THEN
      IF firstI > 0 THEN
        DEC (size, firstN.length - firstI);
        MTextDs.Delete(firstN, firstI, firstN.length);
        firstN := MTextDs.RightNeighbor(firstN);
        firstI := 0
      END;
      WHILE firstN # lastN DO
        node := firstN;
        parent := node.up;
        (* find largest subtree including firstN that we can delete. This
           will never include the root, because it can never include the
           anchor node, because you can't get a head to the end of the
           anchor node. *)
        WHILE node = parent.left AND parent.length <= size DO
          node := parent;
          parent := node.up
        END;
        (* Strange use of RightNeighbor: we want the nearest leaf to the
           right of node, which may not be a leaf! It should work,
           though. *)
        firstN := MTextDs.RightNeighbor(node);
        size := size - node.length;
        MTextDs.RemoveNode(node);
        IF size = 0 THEN RETURN END
      END
    END;
    IF firstI # lastI THEN MTextDs.Delete(firstN, firstI, lastI) END
  END DeleteNodes;
  
(*********************************************************************)
(* Extracting Text and Information *)
(*********************************************************************)

PROCEDURE Length (m: T): CARDINAL =
  BEGIN
    LOCK m.lock DO RETURN m.length END
  END Length;

PROCEDURE GetText (m    : T;
                   begin: CARDINAL := 0;
                   end  : CARDINAL := LAST(CARDINAL)): TEXT =
  VAR
    beginN, endN: Node;
    beginI, endI: CARDINAL;
    t:            TEXT;
  BEGIN
    LOCK m.lock DO
      Check (begin, end, m.length);
      MTextDs.Locate(m, begin, beginN, beginI);
      MTextDs.Locate(m, end, endN, endI);
      IF endN = beginN THEN
        RETURN MTextDs.GetNodeText(beginN, beginI, endI)
      ELSE
        t := MTextDs.GetNodeText(beginN, beginI, beginN.length);
        beginN := MTextDs.RightNeighbor(beginN);
        WHILE beginN # endN DO
          t := t & MTextDs.GetNodeText(beginN);
          beginN := MTextDs.RightNeighbor(beginN)
        END;
        t := t & MTextDs.GetNodeText(beginN, 0, endI);
        RETURN t
      END
    END
  END GetText;

PROCEDURE GetChar (m: T; index: CARDINAL): CHAR =
  <* FATAL Rd.Failure, Thread.Alerted, Rd.EndOfFile *>
  VAR
    node:  Node;
    nodeI: CARDINAL;
  BEGIN
    LOCK m.lock DO
      MTextDs.LocateB(m, index, node, nodeI);
      CASE node.type OF
      | NodeType.text => RETURN Text.GetChar(node.text, nodeI);
      | NodeType.buf => RETURN node.buffer[nodeI];
      | NodeType.file =>
          Rd.Seek(node.file, nodeI + node.start);
          RETURN Rd.GetChar(node.file);
      ELSE <* ASSERT FALSE *>
      END
    END
  END GetChar;

BEGIN
END MText.

(* Data Structure and Algorithms

   Mutable Text is intended as a text editor's internal representation of
   text. It is designed on the assumption that insertions and deletions and
   replacements will be common, and that the text being edited may be quite
   large (order of 1 million characters). This data structure, a balanced 2-3
   tree, supports all operations except writing out the file in log(n) time.

   The data structure is a balanced 2-3 tree, represented by a binary tree. A
   node with 2 children is an ordinary binary node; a node with 3 children has
   a subnode as its right child. A subnode is a binary node marked with the
   bit sub=TRUE, which influences the operation of the rebalancing algorithms.
   At intermediate stages in an insertion, a node may have 4 or 5 children; in
   this case a subnode may have a subnode, always on the right.

   The leaves of the tree are pieces of text.  They can be of three
   kinds: text, a TEXT; file, an Rd.T together with the start and length
   of the section referred to; or buf, an array buffer which is used
   for efficient typein.  A mutable text has only one buf node at a
   time.  Every leaf node stores the length of the text in it, and every
   interior node stores the total length of all the text in the subtree
   beneath it.

   To a client, a mutable text is represented by a REF to a special type of
   node, a "top" node. There is one of these per mutable text; it is the
   parent of the root of the tree, and it stores information about the text as
   a whole. The rightmost node in a mutable text tree is a special node of
   type "anchor" and length 1; it represents the end of the text. An empty
   mutable text is represented by a top node whose "root" field points to an
   anchor node. The length 1 in the anchor node represents a "phantom
   character". This phantom character means that the length field of the root
   node is always 1 greater than the truth. This 1 is subtracted off in
   maintaining the length of the top node, so the top node's length is true.

   A Reader maintains a reference to a leaf node in the text. The index within
   that leaf is kept in the reader package's data structure. The MText must
   not be edited while a reader on it is in use; a reader initialized before
   an editing operation must not be used afterward.

   To insert a new node after a existing node n, we give n's parent a new
   subnode with the new node as a child. If n's parent now has one subnode (3
   children), we are done. But if n's parent now has two subnodes (4
   children), we need to split the next level up. We give n's parent the left
   2 children, make a new node with the right two children, and use the same
   procedure to insert the new node after n's parent.

   Deletion is more complicated. In the simple case, the deletion lies within
   one node, which we will split into two parts containing the remaining first
   and last parts of the affected node. In the general case, several nodes are
   involved. We chop of the last part of the first node in the range and the
   first part of the last node in the range, and we delete all intervening
   nodes from the tree. The tree, of course, must stay 2-3 and balanced. To
   delete a node while keeping a balanced 2-3 tree, we do as follows.

   If we delete a node which has two brothers, a 3 node has become a 2 node
   and all is well. But if we delete a node which has only one brother, then
   we must take that brother and hang it somewhere else. The procedure
   MoveToLeft does this; it attaches its argument as the new right brother of
   its nearest "cousin to the left" in the tree. Or it will in fact move the
   node to the right, if there is no left cousin. In any event, it changes
   neither the length nor the ordering of the mtext, but it does leave its
   argument's original parent with no children. This parent can then be
   deleted. This process proceeds up the tree until we reach a 3-node, which
   becomes a 2-node, and all is well. Or in the rare case, the root gets left
   with only one child. At this point, we delete that root node, and its child
   becomes the new root. *)
