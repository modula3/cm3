(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

@RedBlackTest
PROCEDURE RedBlackTest(insKey, delKey: ARRAY OF Key)@
  VAR t: Tree; n: Node; BEGIN
    @1 t.root := NIL@;
    @2 FOR i := FIRST(insKey^) TO LAST(insKey^) DO@
      @3 n := NEW(Node, key := insKey[i])@;
      @4 RedBlackInsert(t, n)@
    END

    (* Delete "delKey" keys *)
    @5 FOR i := FIRST(delKey^) TO LAST(delKey^) DO@
      @6 n := Search(alg, delKey[i])@;
      @7 RedBlackDelete(alg, n)@
    END;
  END RedBlackTest;
@RedBlackTest

@Insert
PROCEDURE Insert(t: Tree; n: Node)@ =
  VAR temp, curr: Node; BEGIN
    @3 temp := t.root;@
    @4 curr := NIL;@
    @5 WHILE temp # NIL DO@
      @6 curr := temp;@
      @7 IF n.key < curr.key@
        THEN @8 temp := curr.left@
        ELSE @9 temp := curr.right@
      END
    END;
    @10 n.parent := curr;@
    @11 IF curr = NIL@ THEN @12 t.root := n@ ELSE
      @13 IF n.key < curr.key@
        THEN @14 curr.left := n@
        ELSE @15 curr.right := n@
      END
    END
  END Insert;
@Insert

@RedBlackInsert
PROCEDURE RedBlackInsert(t: Tree; n: Node)@ =
  VAR side, other: Side; BEGIN
    @1 Insert(t, n)@;
    @2 n.type := NodeType.Red@;
    @3 WHILE n.parent # NIL AND n.parent.type = NodeType.Red@ DO
      <* ASSERT n.parent.parent # NIL *>
      @4 IF n.parent = n.parent.parent.left@
        THEN @5 side := BSTAlg.Side.Left@;
        ELSE @6 side := BSTAlg.Side.Right;@
      END;
      @7 other := BSTAlg.OtherSide[side]@;
      @8 y := GetChild(n.parent.parent, other)@;
      @9 IF y # NIL AND y.type = NodeType.Red@ THEN
        (* Case 1 *)
        @10 n.parent.type := NodeType.Black@;
        @11 y.type := NodeType.Black@;
        @12 n.parent.parent.type := NodeType.Red@;
        @13 n := n.parent.parent@
      ELSE
        @14 IF n = BSTAlg.GetChild(n.parent, other)@ THEN
          (* Case 2 *)
          @15 n := n.parent@;
          @16 BSTAlg.Rotate(tree, n, side)@
        END;
        (* Case 3 *)
        @17 n.parent.type := NodeType.Black@;
        @18 n.parent.parent.type := NodeType.Red@;
        @19 Rotate(tree, n.parent.parent, other)@
      END;
    END;
    <* ASSERT tree.root # NIL *>
    @20 IF tree.root.type # NodeType.Black THEN
      @21 tree.root.type := NodeType.Black@
    END
  END RedBlackInsert;
@RedBlackInsert

@Search
PROCEDURE Search(t: Tree; k: Key): Node@ =
  VAR n: Node; BEGIN
    @1 n := t.root@;
    @2 WHILE n # t.nil AND n.key # key DO@
      @3 IF key < n.key@
        THEN @4 n := n.left@
        ELSE @5 n := n.right@
      END
    END;
    @6 RETURN n@
  END Search;
@Search

@RedBlackDelete
PROCEDURE RedBlackDelete(t: Tree; n: Node)@ =
  VAR splice, splice_ch: Node; BEGIN
    (* Set "splice" to the node to splice out *)
    @1 IF n.left = t.nil OR n.right = t.nil@
      THEN @2 splice := n@
      ELSE @3 splice := FindMin(n.right)@
    END;

    (* Splice out "splice" node *)
    @4 splice_ch := SpliceOut(t, splice)@;

    (* Replace "n" by "splice" if necessary *)
    @5 IF splice # n@ THEN
      @6 n.key := splice.key@
      (* << copy other node fields here >> *)
    END

    (* Fix up tree if there is potential for double-black *)
    @7 IF splice.type = NodeType.Black@ THEN
      @8 RedBlackFixUp(t, splice_ch)@
  END RedBlackDelete;
@RedBlackDelete

@FindMin
PROCEDURE FindMin(n: Node): Node@ =
  BEGIN
    @1 WHILE n.left # NIL DO@
      @2 n := n.left2@
    END;
    @3 RETURN n@
  END FindMin;
@FindMin

@SpliceOut
PROCEDURE SpliceOut(t: Tree; n: Node)@ =
  VAR ch: Node; BEGIN
    (* Set "ch" to child of "n" or NIL *)
    @1 IF n.left # NIL@
      THEN @2 ch := n.left@
      ELSE @3 ch := n.right@
    END;

    (* Update "up" pointer *)
    @4 IF ch # NIL@ THEN
      @5 ch.parent := n.parent@
    END;

    (* Update "down" pointers *)
    @6 IF n.parent = NIL@ THEN
      @7 t.root := ch@
    ELSE
      @8 IF n = n.parent.left@
        THEN @9 n.parent.left := ch@
        ELSE @10 n.parent.right := ch@
      END
    END;
    @11 RETURN ch@
  END SpliceOut;
@SpliceOut

@RedBlackFixUp
PROCEDURE RedBlackFixUp(t: Tree; n: Node)@ =
  VAR sibling: Node; side, other: Side BEGIN
    @1 WHILE n # t.root AND n.type = NodeType.Black DO@
      @2 IF n = n.parent.left@
        THEN @3 side := Side.Left;  other := side.Right@;
        ELSE @4 side := Side.Right; other := side.Left@;
      END;
      @5 sibling := GetChild(n.parent, other)@;
      @6 IF sibling.type = NodeType.Red@ THEN
        (* Case 1 *)
        @7 sibling.type := NodeType.Black@;
        @8 n.parent.type := NodeType.Red@;
        @9 Rotate(t, n.parent, side)@;
        @10 sibling := GetChild(n.parent, other)@
      END;
      @11 IF sibling.left.type = NodeType.Black
         AND sibling.right.type = NodeType.Black@ THEN
        (* Case 2 *)
        @12 sibling.type := NodeType.Red@;
        @13 n := n.parent@
      ELSE
        @14 IF GetChild(sibling, other).type = NodeType.Black@ THEN
          @15 GetChild(sibling, side).type := NodeType.Black@;
          @16 sibling.type := NodeType.Red@;
          @17 Rotate(t, sibling, other)@;
          @18 sibling := GetChild(n.parent, other)@
        END;
        (* Case 4 *)
        @19 sibling.type := n.parent.type@;
        @20 n.parent.type := NodeType.Black@;
        @21 GetChild(sibling, other).type := NodeType.Black@;
        @22 Rotate(t, n.parent, side)@;
        @23 n := t.root@
      END
    END;
    @24 IF n.type # NodeType.Black@ THEN
      @25 n.type := NodeType.Black@
    END
  END RedBlackFixUp
@RedBlackFixUp
