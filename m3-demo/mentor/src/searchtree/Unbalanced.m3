(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

@UnbalancedTest
PROCEDURE UnbalancedTest(insKey, delKey: ARRAY OF Key)@ =
  VAR t: Tree; n: Node; BEGIN
    (* Insert "insKey" keys *)
    @1 t.root := NIL@;
    @2 FOR i := FIRST(insKey^) TO LAST(insKey^) DO@
      @3 n := NEW(Node, key := insKey[i])@;
      @4 Insert(t, n)@
    END;

    (* Delete "delKey" keys *)
    @5 FOR i := FIRST(delKey^) TO LAST(delKey^) DO@
      @6 n := Search(alg, delKey[i])@;
      @7 Delete(alg, n)@
    END;
  END UnbalancedTest;
@UnbalancedTest

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

@Search
PROCEDURE Search(t: Tree; k: Key): Node@ =
  VAR n: Node; BEGIN
    @1 n := t.root@;
    @2 WHILE n # NIL AND n.key # key DO@
      @3 IF key < n.key@
        THEN @4 n := n.left@
        ELSE @5 n := n.right@
      END
    END;
    @6 RETURN n@
  END Search;
@Search

@Delete
PROCEDURE Delete(t: Tree; n: Node)@ =
  VAR splice: Node; BEGIN
    (* Set "splice" to the node to splice out *)
    @1 IF n.left = NIL OR n.right = NIL@
      THEN @2 splice := n@
      ELSE @3 splice := FindMin(n.right)@
    END;

    (* Splice out "splice" node *)
    @4 SpliceOut(t, splice)@;

    (* Replace "n" by "splice" if necessary *)
    @5 IF splice # n@ THEN
      @6 n.key := splice.key@
      (* << copy other node fields here >> *)
    END
  END Delete;
@Delete

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
    END
  END SpliceOut;
@SpliceOut
