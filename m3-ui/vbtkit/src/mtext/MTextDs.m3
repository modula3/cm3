(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 14:46:58 PST 1995 by kalsow *)
(*      modified on Tue Jun 30 20:29:13 1992 by mhb    *)
(*      modified on Tue Jun 16 13:16:24 PDT 1992 by muller *)
(*      modified on Fri Nov 22 20:45:28 PST 1991 by meehan *)
(*      modified on Mon May 7 8:50:42 PDT 1990 by mcjones *)
(*      modified on Thu Oct 19 21:29:38 1989 by chan *)
(* Created on Thu Sep 4 13:13:00 1986 by chan *)

MODULE MTextDs;

IMPORT Rd, Text, Thread, MTextPrivate;
FROM MTextPrivate IMPORT Node, NodeType;
FROM MText IMPORT T;

(**************************)
(* node-location routines *)
(**************************)

PROCEDURE Locate (              m    : T;
                                index: CARDINAL;
                  VAR (* out *) node : Node;
                  VAR (* out *) nodeI: CARDINAL  ) =
  BEGIN
    node := m.root;
    WHILE node.type = NodeType.tree DO
      IF index > node.leftSize THEN
        DEC (index, node.leftSize);
        node := node.right
      ELSE
        node := node.left
      END
    END;
    nodeI := index
  END Locate;

PROCEDURE LocateB (             m    : T;
                                index: CARDINAL;
                   VAR (* out*) node : Node;
                   VAR (* out*) nodeI: CARDINAL  )=
  BEGIN
    node := m.root;
    WHILE node.type = NodeType.tree DO
      IF index >= node.leftSize THEN
        DEC (index, node.leftSize);
        node := node.right
      ELSE
        node := node.left
      END
    END;
    nodeI := index
  END LocateB;

PROCEDURE GetIndexOfNode (node: Node; nodeI: CARDINAL):CARDINAL =
  VAR parent: Node;
  BEGIN
    parent := node.up;
    WHILE parent.type # NodeType.top DO
      IF node = parent.right THEN INC (nodeI, parent.leftSize) END;
      node := parent;
      parent := node.up
    END;
    RETURN nodeI
  END GetIndexOfNode;

PROCEDURE LeftNeighbor (node: Node):Node =
  VAR parent: Node;
  BEGIN
    parent := node.up;
    WHILE parent.type = NodeType.tree AND node = parent.left DO
      node := parent;
      parent := node.up
    END;
    IF parent.type = NodeType.tree THEN (* go down left branch *)
      node := parent.left;
      WHILE node.type = NodeType.tree DO node := node.right END;
      RETURN node
    ELSE
      RETURN NIL
    END
  END LeftNeighbor;

PROCEDURE RightNeighbor (node: Node):Node =
  VAR parent: Node;
  BEGIN
    parent := node.up;
    WHILE parent.type = NodeType.tree AND node = parent.right DO
      node := parent;
      parent := node.up
    END;
    IF parent.type = NodeType.tree THEN (* go down right branch *)
      node := parent.right;
      WHILE node.type = NodeType.tree DO node := node.left END;
      RETURN node
    ELSE
      RETURN NIL
    END
  END RightNeighbor;

(******************************)
(* tree manipulation routines *)
(******************************)

PROCEDURE InsertAt (node: Node; nodeI: CARDINAL; newnode: Node) =
  BEGIN
    IF nodeI = 0 THEN
      InsertBefore (node, newnode)
    ELSIF nodeI = node.length THEN
      InsertAfter (node, newnode)
    ELSE
      SplitLeaf (node, nodeI);
      InsertAfter (node, newnode)
    END
  END InsertAt;

PROCEDURE InsertBefore (node, newnode: Node) =
  VAR parent, spare, right2: Node;
  BEGIN
    parent := node.up;
    IF parent.type = NodeType.top THEN
      spare := NEW (Node, type := NodeType.tree, sub := FALSE);
      Remake (spare, newnode, node);
      SplitRoot (parent, spare)
    ELSIF node = parent.right THEN
      InsertAfter (parent.left, newnode)
    ELSIF parent.sub THEN       (* parent must be a right subchild *)
      InsertAfter (parent.up.left, newnode)
    ELSE
      (* make newnode the leftmost child of parent *)
      spare := NEW (Node, type := NodeType.tree, sub := TRUE);
      Remake (spare, parent.left, parent.right);
      Remake (parent, newnode, spare);
      right2 := parent.right.right;
      IF right2.type = NodeType.tree AND right2.sub THEN
        (* we have a 4-child node, split the parent *)
        Remake (parent, parent.left, parent.right.left);
        right2.sub := FALSE;    (* it becomes a new logical node *)
        InsertAfter (parent, right2)
      ELSE
        (* we have a 3-child node, we're ok *)
        FixLengths (parent)
      END
    END
  END InsertBefore;

PROCEDURE InsertAfter (node, newnode: Node) =
  VAR parent, spare, right2: Node;
  BEGIN
    spare := NEW (Node, type := NodeType.tree, sub := TRUE);
    (* insertion requires 1 new interior node *)
    LOOP                        (* terminates at top or when tree is OK *)
      parent := node.up;
      IF parent.type = NodeType.top THEN (* need to split the root *)
        Remake (spare, node, newnode);
        SplitRoot (parent, spare);
        RETURN
      ELSIF node = parent.left THEN
        Remake (spare, newnode, parent.right)
      ELSE
        Remake (spare, parent.right, newnode)
      END;
      Remake (parent, parent.left, spare);
      (* find the head of the logical node *)
      IF parent.sub THEN parent := parent.up END;
      right2 := parent.right.right;
      IF right2.type = NodeType.tree AND right2.sub THEN
        (* we have a 4-child node, split the parent *)
        spare := parent.right;
        Remake (parent, parent.left, parent.right.left);
        (* spare just got dropped, so it's reusable *)
        right2.sub := FALSE;    (* it becomes a new logical node *)
        node := parent;
        newnode := right2;
        (* now, one level up, repeat the loop *)
      ELSE
        (* we have a 3-child node, we're ok *)
        FixLengths (parent);
        RETURN
      END
    END;                        (* LOOP *)
  END InsertAfter;

PROCEDURE SplitRoot (top, newroot: Node) =
  (* Given the top node and the new root node, make the necessary
     connections. Analogous to Remake, for the top node. *)
  BEGIN
    newroot.sub := FALSE;
    newroot.up := top;          (* the "top" node *)
    top.root := newroot;
    top.length := newroot.length - 1;
    INC (top.height)
  END SplitRoot;

PROCEDURE FixLengths (node: Node) =
  VAR parent: Node;
  BEGIN
    parent := node.up;
    WHILE parent.type # NodeType.top DO
      IF node = parent.left THEN parent.leftSize := node.length END;
      parent.length := parent.leftSize + parent.right.length;
      node := parent;
      parent := node.up
    END;
    (* correct the top node *)
    parent.length := node.length - 1
  END FixLengths;

PROCEDURE Remake (node, left, right: Node) =
  BEGIN
    node.left := left;
    left.up := node;
    node.leftSize := left.length;
    node.right := right;
    right.up := node;
    node.length := left.length + right.length
  END Remake;

PROCEDURE RemoveNode (node: Node) =
  VAR parent: Node;
  BEGIN
    LOOP
      parent := node.up;
      (* unusual termination: previous MoveToLeft collapsed the root, and
         the job is finished *)
      IF parent = NIL THEN RETURN END;
      IF node = parent.left THEN
        IF parent.sub THEN
          Remake (parent.up, parent.up.left, parent.right);
          FixLengths (parent.up);
          EXIT
        ELSIF parent.right.type = NodeType.tree AND parent.right.sub
          THEN
          Remake (parent, parent.right.left, parent.right.right);
          FixLengths (parent);
          EXIT
        ELSE                    (* parent has only 2 children *)
          MoveToLeft (parent.right);
          parent.right := NIL;
          (* and now repeat the loop to remove parent *)
        END
      ELSE                      (* node is a right child *)
        IF parent.sub THEN
          Remake (parent.up, parent.up.left, parent.left);
          FixLengths (parent.up);
          EXIT
        ELSE                    (* parent has only 2 children *)
          MoveToLeft (parent.left);
          parent.left := NIL;
          (* and now repeat the loop to remove parent *)
        END
      END;
      (* now remove parent *)
      node := parent
    END;
    Free (node);
    (* no more levels to remove, the tree is balanced again *)
  END RemoveNode;

PROCEDURE MoveToLeft (node: Node) =
  (* Move a node from a subtree which is about to disappear. Hang it on the
     nearest node to the left of its parent. If there is nothing to the
     left of its parent, attach it as the rightmost child of the nearest
     node to the right of its parent. If there is nothing to the right, its
     parent must be the root, so this node becomes the root. Do not call
     MoveToLeft on the root. *)
  VAR
    h    : CARDINAL; (* relative height of n from node *)
    n, up: Node;
  BEGIN
    n := node.up;
    h := 1;
    up := n.up;
    IF up.type = NodeType.top THEN (* node is a child of root! *)
      (* this is how the tree decreases in height: node becomes root *)
      up.root := node;
      node.up := up;
      up.length := node.length - 1;
      DEC (up.height);
      n.up := NIL;
      IF node = n.left THEN Free (n.right) ELSE Free (n.left) END;
      RETURN
    END;
    WHILE up.type = NodeType.tree AND n = up.left DO
      n := up;
      up := n.up;
      IF NOT n.sub THEN INC (h) END
    END;
    IF up.type = NodeType.tree THEN
      (* normal case: didn't reach the root *)
      n := up.left;             (* go down left branch *)
      WHILE h > 0 DO
        n := n.right;
        IF n.type # NodeType.tree OR NOT n.sub THEN DEC (h) END
      END;                      (* we've found our new brother *)
      InsertAfter (n, node)
    ELSE                        (* leftmost node, have to move it to the
                                   right *)
      (* Below is a very presumptious piece of code. I'll explain it: Up in
         the first section we proved that node^.up^.up is not top, it must
         be a tree node. Just now we proved that we're on the leftmost
         branch, so node^.up^.up^.right must be on a different branch from
         us. Because the tree is balanced, that must be a tree node, so we
         can safely test for sub and take its left branch. Balance allows
         us to take the left branch again, if so. Result: we find the
         rightmost "first cousin" of this node. *)
      n := node.up.up.right;    (* father-grandfather-(uncle) *)
      IF n.sub THEN n := n.left END; (* true uncle *)
      n := n.left;              (* cousin *)
      InsertBefore (n, node)
    END
  END MoveToLeft;

(*******************)
(* node operations *)
(*******************)

PROCEDURE Delete (VAR (* inout*) node: Node; b, e: CARDINAL) =
  (* deletes characters [b, e] from node. *)
  VAR
    i             : CARDINAL;
    rnode, newnode: Node;
  BEGIN
    IF b = 0 AND e = node.length THEN
      rnode := RightNeighbor (node);
      RemoveNode (node);
      node := rnode;
      RETURN
    END;
    CASE node.type OF
    | NodeType.text =>
        IF b = 0 THEN
          node.text := Text.Sub (node.text, e, LAST (CARDINAL))
        ELSIF e = node.length THEN
          node.text := Text.Sub (node.text, 0, b)
        ELSE
          (* copied from SplitLeaf *)
          newnode := NEW (Node, type := node.type);
          newnode.text := Text.Sub (node.text, e, LAST (CARDINAL));
          newnode.length := node.length - e;
          node.length := b;
          node.text := Text.Sub (node.text, 0, b);
          InsertAfter (node, newnode);
          RETURN
        END;
    | NodeType.file =>
        IF b = 0 THEN
          INC (node.start, e)
        ELSIF e < node.length THEN
          SplitLeaf (node, e)
        END;
    | NodeType.buf =>
        i := node.length - e;
        IF i > 0 THEN
          SUBARRAY (node.buffer^, b, i) := SUBARRAY (node.buffer^, e, i)
        END
    ELSE <* ASSERT FALSE *>
    END;
    DEC (node.length, e - b);
    FixLengths (node)
  END Delete;

PROCEDURE Free (node: Node) =
  BEGIN
    node.up := NIL;
    IF node.type = NodeType.tree THEN
      IF node.left # NIL THEN Free (node.left) END;
      IF node.right # NIL THEN Free (node.right) END;
      node.left := NIL;
      node.right := NIL
    END
  END Free;
  
(************************)
(* leaf node operations *)
(************************)

PROCEDURE ReplaceLeaf (old, new: Node) =
  VAR parent: Node;
  BEGIN
    parent := old.up;
    old.up := NIL;              (* free the leaf node. *)
    IF parent.type = NodeType.top THEN
      parent.root := new
    ELSIF old = parent.left THEN
      parent.left := new
    ELSE
      parent.right := new
    END;
    new.up := parent
  END ReplaceLeaf;

PROCEDURE SplitLeaf (node: Node; i: CARDINAL) =
  VAR newnode: Node;
  BEGIN
    <* ASSERT ((i # 0) AND (i # node.length)) *>
    (* programming error. *)
    CASE node.type OF
    | NodeType.buf =>
        newnode := NEW (Node, type := NodeType.text,
                        text := Text.FromChars (SUBARRAY (node.buffer^, i,
                                                          node.length - i)))
    | NodeType.text =>
        newnode := NEW (Node, type := node.type,
                        text := Text.Sub (node.text, i, node.length - i));
        node.text := Text.Sub(node.text, 0, i)
    | NodeType.file =>
        newnode := NEW (Node, type := node.type, start := node.start + i,
                        file := node.file)
    ELSE <* ASSERT FALSE *>
    END;
    newnode.length := node.length - i;
    node.length := i;
    InsertAfter (node, newnode)
  END SplitLeaf;

(******************************)
(* node conversion operations *)
(******************************)

PROCEDURE ToText (VAR (* inout *) node: Node; all: BOOLEAN := TRUE) =
  <* FATAL Rd.Failure, Thread.Alerted *>
  VAR
    textnode        : Node;
    start, end, size: CARDINAL;
  BEGIN
    textnode := NEW (Node, type := NodeType.text, length := node.length);
    CASE node.type OF
    | NodeType.buf =>
        textnode.text :=
          Text.FromChars (SUBARRAY (node.buffer^, 0, node.length));
        ReplaceLeaf (node, textnode);
    | NodeType.file =>
        start := node.start;
        end := start + FileChunkSize - start MOD FileChunkSize;
        IF all OR end >= start + node.length THEN
          size := node.length
        ELSE
          size := end - start
        END;
        Rd.Seek (node.file, start);
        textnode.text := Rd.GetText (node.file, size);
        IF all OR end >= start + node.length THEN
          ReplaceLeaf (node, textnode)
        ELSE
          textnode.length := size;
          INC (node.start, size); (* == Delete(node, 0, size) *)
          DEC (node.length, size);
          FixLengths (node);
          InsertBefore (node, textnode)
        END
    ELSE <* ASSERT FALSE *>
    END;
    node := textnode
  END ToText;

PROCEDURE MoveBufTo (                m    : T;
                     VAR (* inout *) node : Node;
                     VAR (* inout *) nodeI: CARDINAL) =
  VAR bufnode: Node;
  BEGIN
    bufnode := m.bufNode;
    CASE node.type OF
    | NodeType.anchor =>        <* ASSERT (nodeI = 0) *>
    | NodeType.buf =>
        IF nodeI > 0 AND nodeI < node.length THEN
          SplitLeaf (node, nodeI)
        END;
        ToText (node);
    | NodeType.text =>
        IF bufnode.up # NIL THEN ToText (bufnode) END;
        bufnode := m.bufNode;   (* get it back *)
    | NodeType.file =>
        IF bufnode.up # NIL THEN ToText (bufnode) END;
        bufnode := m.bufNode;   (* get it back *)
    ELSE <* ASSERT FALSE *>
    END;
    InsertAt (node, nodeI, bufnode);
    node := bufnode;
    node.length := 0;
    nodeI := 0
  END MoveBufTo;

(***********************)
(* Buf node operations *)
(***********************)


PROCEDURE BufOpen (node: Node; point, size: CARDINAL) =
  BEGIN
    FOR i := node.length - 1 TO point BY -1 DO
      node.buffer [size + i] := node.buffer [i]
    END;
    INC(node.length, size);
    FixLengths(node)
  END BufOpen;

(******************************)
(* Extracting text from nodes *)
(******************************)

PROCEDURE GetNodeText (VAR(*inout*) node : Node;
                                    begin: CARDINAL := 0;
                                    end  : CARDINAL := LAST (CARDINAL)): TEXT =
  VAR length: CARDINAL;
  BEGIN
    end := MIN (node.length, end);
    length := end - begin;
    IF length = 0 THEN RETURN "" END;
    CASE node.type OF
    | NodeType.text =>
        IF length = node.length THEN
          RETURN node.text
        ELSE
          RETURN Text.Sub (node.text, begin, length)
        END;
    | NodeType.buf =>
        RETURN Text.FromChars (SUBARRAY (node.buffer^, begin, length));
    | NodeType.file =>
        ToText (node);
        IF length = node.length THEN
          RETURN node.text
        ELSE
          RETURN Text.Sub (node.text, begin, length)
        END;
    | NodeType.anchor => RETURN ""
    ELSE <* ASSERT FALSE *>
    END
  END GetNodeText;

BEGIN
END MTextDs.
