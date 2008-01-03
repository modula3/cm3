(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 30 15:57:53 PDT 1992 by mhb         *)
(*      modified on Tue Jun 16 13:16:26 PDT 1992 by muller      *)
(*      modified on Sun Oct 13 17:26:59 PDT 1991 by meehan      *)

MODULE MTextDebug;

IMPORT MTextPrivate, Wr, Fmt, Rd, Text, Thread;
FROM MText IMPORT T;
FROM MTextPrivate IMPORT Node, NodeType;
FROM Stdio IMPORT stderr;

<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure *>


(*********************************************************************)
(*                       For Debugging Only                          *)
(*********************************************************************)

(* A generator for node i.d. numbers. These exist only to make structural
   printouts more readable, and are assigned only when the nodes are
   printed. *)
   
VAR idCounter: CARDINAL := 0;

PROCEDURE Id (node: T): CARDINAL RAISES {} =
  BEGIN
    IF node.id = 0 THEN INC(idCounter); node.id := idCounter END;
    RETURN node.id
  END Id;

(* Dump gives a structural printout of everything interesting about an
   MText: size, files held open, tree structure, list of heads. *)

PROCEDURE Dump (m: T) =
  VAR
    index: CARDINAL;
    wr            := stderr;
  PROCEDURE Space (n: INTEGER) =
    BEGIN
      FOR i := 1 TO n DO Wr.PutChar(wr, ' ') END
    END Space;
  PROCEDURE DumpTree (node: Node; indent: INTEGER) =
    BEGIN
      CASE node.type OF
        NodeType.tree =>
          DumpTree(node.left, indent + 2);
          Space(indent);
          Wr.PutText(wr, "[n" & Fmt.Int(Id (node)) & " len="
                           & Fmt.Int(node.length) & "]\n");
          IF (node.right.type = NodeType.tree) AND (node.right.sub) THEN
            DumpTree(node.right.left, indent + 2);
            Space(indent + 1);
            Wr.PutText(wr, "[+" & Fmt.Int(Id (node.right)) & " len="
                             & Fmt.Int(node.right.length) & "]\n");
            DumpTree(node.right.right, indent + 2)
          ELSE
            DumpTree(node.right, indent + 2)
          END
      | NodeType.anchor =>
          Space(indent);
          Wr.PutText(
            wr, "[a" & Fmt.Int(Id (node)) & " len=" & Fmt.Int(node.length)
                  & " index=" & Fmt.Int(index) & "]\n")
      | NodeType.text =>
          Space(indent);
          Wr.PutText(
            wr, "[t" & Fmt.Int(Id (node)) & " len=" & Fmt.Int(node.length)
                  & " index=" & Fmt.Int(index) & "]<");
          IF node.length < 20 THEN
            Wr.PutText(wr, node.text & ">\n")
          ELSE
            Wr.PutText(wr, Text.Sub(node.text, 0, 20) & "...>\n")
          END;
          INC(index, node.length)
      | NodeType.buf =>
          Space(indent);
          Wr.PutText(
            wr, "[b" & Fmt.Int(Id (node)) & " len=" & Fmt.Int(node.length)
                  & " index=" & Fmt.Int(index) & "]<");
          IF node.length < 20 THEN
            Wr.PutString(wr, SUBARRAY(node.buffer^, 0, node.length));
            Wr.PutText(wr, ">\n")
          ELSE
            Wr.PutString(wr, SUBARRAY(node.buffer^, 0, 20));
            Wr.PutText(wr, "...>\n")
          END;
          INC(index, node.length)
      | NodeType.file =>
          Space(indent);
          Wr.PutText(
            wr, "[f" & Fmt.Int(Id (node)) & ",start=" & Fmt.Int(node.start)
                  & " len=" & Fmt.Int(node.length) & " index="
                  & Fmt.Int(index) & "]<");
          Rd.Seek(node.file, node.start);
          IF node.length < 20 THEN
            Wr.PutText(wr, Rd.GetText(node.file, node.length) & ">\n")
          ELSE
            Wr.PutText(wr, Rd.GetText(node.file, 20) & "...>\n")
          END;
          INC(index, node.length)
      ELSE <* ASSERT FALSE *>
      END
    END DumpTree;
  BEGIN
    LOCK m.lock DO
      Wr.PutText(wr, "mtext of length " & Fmt.Int(m.length) & ", height "
                       & Fmt.Int(m.height) & ", root ["
                       & Fmt.Int(Id (m.root)) & "]\n");
      index := 0;
      DumpTree(m.root, 0);
      Wr.PutText(wr, "\n");
      Wr.Flush(wr)
    END
  END Dump;


(* Verify verifies all the properties which I assert to be true of a
   consistent mutable text tree or subtree: It contains no NIL Node
   pointers except top.up.
|   For every interior node,
|    node.left.up = node
|    node.right.up = node
|    node.length = node.left.length + node.right.length
|    node.leftSize = node.left.length NOT node.left.sub
|  If node.sub then node.right.sub = FALSE (2-3 tree condition)
|  For the top node,
|        top.up = NIL
|        top.root # NIL
|        top.root.up = top
|        top.length = top.root.length - 1
|        top.root.sub = FALSE
|        top.height = height of the tree at top.root
|  For each head on the top.heads list,
|        top.root is an ancestor of head.node
|        0 <= head.index <= head.node.length
|        head.index = head.node.length only if
|            head.node is the final anchor node *)


EXCEPTION VerifyError;

PROCEDURE Verify (wr: Wr.T; node: Node; msg: TEXT) =
  <* FATAL VerifyError *>
  VAR
    height: INTEGER;
    root:   Node;
  PROCEDURE Err (err: TEXT) =
    BEGIN
      Wr.PutText(
        wr, "Verify of " & msg & ": " & err & " in [" & Fmt.Int(Id (node))
              & "] at height " & Fmt.Int(height) & "\n");
      Wr.Flush(wr);
      RAISE VerifyError
    END Err;
  PROCEDURE ErrN (err1: TEXT; n: INTEGER; err2: TEXT) =
    BEGIN
      Wr.PutText(
        wr, "Verify of " & msg & ": " & err1 & Fmt.Int(n) & err2 & " in ["
              & Fmt.Int(Id (node)) & "] at height " & Fmt.Int(height) & "\n");
      Wr.Flush(wr);
      RAISE VerifyError
    END ErrN;
  BEGIN
    height := -1;               (* used before height is computed *)
    IF node.type = NodeType.top THEN
      IF node.up # NIL THEN Err("top.up # NIL") END;
      root := node.root;
      IF root = NIL THEN Err("root is NIL") END;
      IF root.up # node THEN Err("inconsistent uplink to top") END;
      IF node.length # root.length - 1 THEN
        ErrN("root length ", root.length, " is wrong")
      END;
      IF (root.type = NodeType.tree) AND (root.sub) THEN
        Err("root is a sub")
      END;
      height := Height(root);
      IF node.height # height THEN Err("height is wrong") END;
      Verify(wr, root, msg);
    ELSIF node.type = NodeType.tree THEN (* regular interior node *)
      height := Height(node);
      IF (node.left = NIL) OR (node.right = NIL) THEN
        Err("node has NIL child")
      END;
      IF node.left.up # node THEN Err("inconsistent left uplink") END;
      IF node.right.up # node THEN Err("inconsistent right uplink") END;
      IF node.length # node.left.length + node.right.length THEN
        Err("length is wrong")
      END;
      IF node.leftSize # node.left.length THEN
        Err("leftSize is wrong")
      END;
      IF (node.left.type = NodeType.tree) AND (node.left.sub) THEN
        Err("sub on left")
      END;
      IF (node.sub) AND (node.right.type = NodeType.tree)
           AND (node.right.sub) THEN
        Err("double sub")
      END;
      Verify(wr, node.left, msg);
      Verify(wr, node.right, msg);
    ELSIF node.type = NodeType.text THEN
      height := 0;
      IF node.length # Text.Length(node.text) THEN
        Err("text node length is wrong")
      END;
    ELSIF node.type = NodeType.buf THEN
      root := node;
      WHILE root.type # NodeType.top DO root := root.up END;
      IF node.length > root.bufMax THEN Err("buf node too long") END;
    ELSIF node.type = NodeType.anchor THEN
      IF node.length # 1 THEN Err("anchor\'s length is not 1") END
    END
  END Verify;


PROCEDURE Height (node: Node): CARDINAL =
  VAR h: CARDINAL := 0;
  BEGIN
    WHILE node.type = NodeType.tree DO h := h + 1; node := node.left END;
    RETURN h
  END Height;

BEGIN
END MTextDebug.
