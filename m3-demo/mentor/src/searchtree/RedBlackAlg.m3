(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 11:09:43 PDT 1994 by heydon                   *)
(*      modified on Tue May  3 13:55:07 PDT 1994 by najork                   *)
(*      modified on Thu Sep 24 14:46:17 PDT 1992 by mhb                      *)
(*      modified on Tue Sep  8 20:30:27 PDT 1992 by johnh                    *)

MODULE RedBlackAlg;

IMPORT Algorithm, BSTAlg, RefList, SearchTreeIE, Thread, UnbalancedAlg, 
       ZeusPanel, ZeusCodeView;

TYPE
  Tree = BSTAlg.Tree BRANDED "RedBlackAlg.Tree" OBJECT
    METHODS
      init(): Tree := TreeInit;
    END;

  RBNode = BSTAlg.Node BRANDED OBJECT
      type: NodeType;
    METHODS
      init(t: Tree): RBNode := RBNodeInit;
  END;

TYPE
  T = BSTAlg.T BRANDED "RedBlackAlg.T" OBJECT
    OVERRIDES
      run := Run;
    END;

PROCEDURE NodeTypeToText(nt: NodeType): TEXT =
  BEGIN
    CASE nt OF
    | NodeType.Red => RETURN "Red"
    | NodeType.Black => RETURN "Black"
    END
  END NodeTypeToText;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN NEW (T, 
                data := ZeusPanel.NewForm ("SearchTree.fv"),
                codeViews :=
                    RefList.List2(
                        RefList.List2("Pseudo-Code",   "RedBlack.pseudo"),
                        RefList.List2("Modula-3 Code", "RedBlack.m3"))).init()
  END New;

PROCEDURE TreeInit(t: Tree): Tree =
  BEGIN
    t.nil := NEW(RBNode, type := NodeType.Black);
    t.root := t.nil;
    RETURN t
  END TreeInit;

PROCEDURE RBNodeInit(n: RBNode; t: Tree): RBNode =
  BEGIN
    n.left := t.nil;
    n.right := t.nil;
    RETURN n
  END RBNodeInit;

PROCEDURE Run(alg: T) RAISES {Thread.Alerted} =
  VAR data: BSTAlg.PanelData; keys: BSTAlg.Keys;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    (* Read input data from form *)
    data := BSTAlg.GetPanelData(alg.data);

    (* Construct new keys *)
    keys := BSTAlg.NewKeys(data, input := TRUE);

    (* Insert all keys into empty tree *)
    ZeusCodeView.Enter(alg, "RedBlackTest");
At(1); alg.tree := NEW(Tree).init();
       VAR n: RBNode; BEGIN 
At(2);   FOR i := 0 TO LAST(keys^) DO 
At(3);     n := NEW(RBNode, index := BSTAlg.NewIndex(),
                  key := keys[i]).init(alg.tree);
At(4);     Insert(alg, n) 
         END;
       END;

       (* Remove keys in a random order *)
       keys := BSTAlg.NewKeys(data, input := FALSE);
       VAR n: BSTAlg.Node; BEGIN
At(5);   FOR i := 0 TO LAST(keys^) DO
At(6);     n := Search(alg, keys[i]);
           <* ASSERT n # NIL *>
At(7);     Delete(alg, n)
         END;
       END;
    ZeusCodeView.Exit(alg)
  END Run;

PROCEDURE SetType(alg: T; n: RBNode; c: NodeType) RAISES {Thread.Alerted} =
  VAR pc: NodeType; BEGIN
    <* ASSERT n # NIL *>
    IF n.parent = NIL
      THEN pc := NodeType.Red
      ELSE pc := NARROW(n.parent, RBNode).type
    END;
    SearchTreeIE.SetType(alg, n.index, c, pc);
  END SetType;

PROCEDURE Insert(alg: T; n: RBNode) RAISES {Thread.Alerted} =
  VAR tree: BSTAlg.Tree := alg.tree; side, other: BSTAlg.Side; y: RBNode;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "RedBlackInsert");
At(1);  UnbalancedAlg.Insert(alg, n);
        SearchTreeIE.CurrentNode(alg, n.index);
At(2);  SetType(alg, n, NodeType.Red);
        n.type := NodeType.Red;
        WHILE n.parent # NIL AND
          NARROW(n.parent, RBNode).type = NodeType.Red DO At(3);
          <* ASSERT n.parent.parent # NIL *>
          SearchTreeIE.RedRedClash(alg, n.index, n.parent.index, TRUE);
At(4);    IF n.parent = n.parent.parent.left THEN
At(5);      side := BSTAlg.Side.Left;
          ELSE
At(6);      side := BSTAlg.Side.Right;
          END;
At(7);    other := BSTAlg.OtherSide[side];
At(8);    y := BSTAlg.GetChild(n.parent.parent, other);
          SearchTreeIE.CheckUncle(alg, n.index);
At(9);    IF y.type = NodeType.Red THEN
            (* Case 1 *)
At(10);     SetType(alg, n.parent, NodeType.Black);
            SearchTreeIE.RedRedClash(alg, n.index, n.parent.index, FALSE);
            NARROW(n.parent, RBNode).type := NodeType.Black;
At(11);     SetType(alg, y, NodeType.Black);
            y.type := NodeType.Black;
At(12);     SetType(alg, n.parent.parent, NodeType.Red);
            NARROW(n.parent.parent, RBNode).type := NodeType.Red;
At(13);     n := n.parent.parent;
            SearchTreeIE.CheckUncle(alg, 0);
            SearchTreeIE.CurrentNode(alg, n.index);
          ELSE
At(14);     IF n = BSTAlg.GetChild(n.parent, other) THEN
              (* Case 2 *)
              VAR old_n := n; BEGIN
At(15);         n := n.parent;
At(16);         SearchTreeIE.CheckUncle(alg, 0);
                SearchTreeIE.CurrentNode(alg, 0);
                SearchTreeIE.Rotate(alg, old_n.index, n.index);
                BSTAlg.Rotate(tree, n, side);
                SearchTreeIE.CurrentNode(alg, n.index);
                SearchTreeIE.CheckUncle(alg, n.index);
              END
            END;
            (* Case 3 *)
At(17);     SetType(alg, n.parent, NodeType.Black);
            SearchTreeIE.RedRedClash(alg, n.index, n.parent.index, FALSE);
            NARROW(n.parent, RBNode).type := NodeType.Black;
At(18);     SetType(alg, n.parent.parent, NodeType.Red);
            NARROW(n.parent.parent, RBNode).type := NodeType.Red;
At(19);     SearchTreeIE.CheckUncle(alg, 0);
            SearchTreeIE.CurrentNode(alg, 0);
            SearchTreeIE.Rotate(alg, n.parent.index, n.parent.parent.index);
            BSTAlg.Rotate(tree, n.parent.parent, other)
          END;
        END; At(3);
        SearchTreeIE.CurrentNode(alg, 0);

        (* set the root type to NodeType.Black if not so already *)
        <* ASSERT tree.root # NIL *>
        VAR rt: RBNode := tree.root; BEGIN
At(20);   IF rt.type # NodeType.Black THEN
At(21);     SetType(alg, tree.root, NodeType.Black);
            rt.type := NodeType.Black;
          END
        END;
    ZeusCodeView.Exit(alg)
  END Insert;

PROCEDURE Search(alg: BSTAlg.T; key: BSTAlg.Key): BSTAlg.Node
  RAISES {Thread.Alerted} =
  VAR x: BSTAlg.Node; t: Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "Search");
At(1);  SearchTreeIE.NewSearchKey(alg, key);
        x := t.root;
        WHILE x # t.nil AND x.key # key DO At(2);
At(3);    SearchTreeIE.CompareKeys(alg, x.index);
          IF key < x.key THEN
At(4);      x := x.left
          ELSE
At(5);      x := x.right
          END
        END;
        IF x # t.nil THEN
          SearchTreeIE.CompareKeys(alg, x.index);
          SearchTreeIE.SearchEnd(alg, x.index)
        ELSE
          SearchTreeIE.SearchEnd(alg, 0)
        END; At(3); At(6);
    ZeusCodeView.Exit(alg);
    RETURN x
  END Search;

PROCEDURE Delete(alg: T; n: RBNode) RAISES {Thread.Alerted} =
  VAR x, y: RBNode; t: Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    <* ASSERT n # NIL *>
    ZeusCodeView.Enter(alg, "RedBlackDelete");
        (* Set "y" to node to splice out *)
At(1);  IF n.left = t.nil OR n.right = t.nil THEN
At(2);    y := n
        ELSE
At(3);    y := FindMin(alg, n.right);
        END;
        
        (* Splice out node "y" *)
At(4);  x := SpliceOut(alg, y, y # n);

        (* Replace "n" by "y" if necessary *)
At(5);  IF y # n THEN
At(6);    n.key := y.key;
          SearchTreeIE.Copy(alg, y.index, n.index)
        END;
At(7);  IF y.type = NodeType.Black THEN
At(8);    FixUp(alg, x)
        END;
    ZeusCodeView.Exit(alg)
  END Delete;

PROCEDURE FindMin(alg: BSTAlg.T; n: BSTAlg.Node): BSTAlg.Node
  RAISES {Thread.Alerted} =
  VAR t: Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "FindMin");
          SearchTreeIE.GoLeft(alg, n.index);
          WHILE n.left # t.nil DO At(1);
            SearchTreeIE.GoLeft(alg, n.left.index);
At(2);      n := n.left
          END; At(1);
At(3);    SearchTreeIE.GoLeft(alg, 0);
    ZeusCodeView.Exit(alg);
    RETURN n
  END FindMin;

PROCEDURE SpliceOut(alg: T; n: BSTAlg.Node; save: BOOLEAN): BSTAlg.Node
  RAISES {Thread.Alerted} =
  VAR x: BSTAlg.Node; tree: Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "SpliceOut");
        <* ASSERT NOT (n.left # tree.nil AND n.right # tree.nil) *>
        (* set "x" to the child of "n" if it has one *)
At(1);  IF n.left # tree.nil THEN
At(2);    x := n.left
        ELSE
At(3);    x := n.right
        END;
        IF x # tree.nil
          THEN SearchTreeIE.SpliceOut(alg, n.index, x.index, save);
          ELSE SearchTreeIE.SpliceOut(alg, n.index, 0, save);
        END;

        (* update "up" pointer *)
At(4);  x.parent := n.parent;

        (* update "down" pointer *)
At(6);  IF n.parent = NIL THEN
At(7);    tree.root := x
        ELSE
At(8);    IF n = n.parent.left THEN
At(9);      n.parent.left := x
          ELSE
At(10);     n.parent.right := x
          END
        END; At(11);
    ZeusCodeView.Exit(alg);
    RETURN x
  END SpliceOut;

PROCEDURE FixUp(alg: T; n: RBNode) RAISES {Thread.Alerted} =
  VAR
    t: Tree := alg.tree;
    side, other: BSTAlg.Side; 
    w, wside, wother, par: RBNode;
  PROCEDURE At(line: CARDINAL) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "RedBlackFixUp");
        IF n # t.nil THEN SearchTreeIE.CurrentNode(alg, n.index) END;
        WHILE n # t.root AND n.type = NodeType.Black DO At(1);
          <* ASSERT n.parent # NIL *>
          par := n.parent;
At(2);    IF n = par.left THEN
At(3);      side := BSTAlg.Side.Left;
          ELSE
At(4);      side := BSTAlg.Side.Right;
          END;
          other := BSTAlg.OtherSide[side];
At(5);    w := BSTAlg.GetChild(par, other);
          <* ASSERT w # t.nil *>
At(6);    IF w.type = NodeType.Red THEN
            (* Case 1 -- w is red *)
At(7);      SetType(alg, w, NodeType.Black);
            w.type := NodeType.Black;
            <* ASSERT par # NIL *>
At(8);      SetType(alg, par, NodeType.Red);
            par.type := NodeType.Red;
            SearchTreeIE.CurrentNode(alg, 0);
At(9);      SearchTreeIE.Rotate(alg, w.index, par.index);
            BSTAlg.Rotate(t, par, side);
            <* ASSERT par = n.parent *>
            SearchTreeIE.CurrentNode(alg, n.index);
At(10);     w := BSTAlg.GetChild(par, other);
            <* ASSERT w # t.nil *>
          END;
          wside := BSTAlg.GetChild(w, side);
          wother := BSTAlg.GetChild(w, other);
At(11);   IF wside.type = NodeType.Black AND wother.type = NodeType.Black THEN
            (* Case 2 -- w is black; both its children are black *)
At(12);     SetType(alg, w, NodeType.Red);
            w.type := NodeType.Red;
At(13);     n := par; par := n.parent;
            SearchTreeIE.CurrentNode(alg, n.index);
          ELSE
At(14);     IF wother.type = NodeType.Black THEN
              (* Case 3 -- w is black; its "other" child is black *)
At(15);       SetType(alg, wside, NodeType.Black);
              wside.type := NodeType.Black;
At(16);       SetType(alg, w, NodeType.Red);
              w.type := NodeType.Red;
              SearchTreeIE.CurrentNode(alg, 0);
At(17);       SearchTreeIE.Rotate(alg, wside.index, w.index);
              BSTAlg.Rotate(t, w, other);
              SearchTreeIE.CurrentNode(alg, n.index);
At(18);       w := BSTAlg.GetChild(par, other);
              <* ASSERT w # t.nil *>
              wother := BSTAlg.GetChild(w, other);
            END;
            (* Case 4 *)
At(19);     SetType(alg, w, par.type);
            w.type := par.type;
At(20);     SetType(alg, par, NodeType.Black);
            par.type := NodeType.Black;
At(21);     SetType(alg, wother, NodeType.Black);
            wother.type := NodeType.Black;
            SearchTreeIE.CurrentNode(alg, 0);
At(22);     SearchTreeIE.Rotate(alg, w.index, par.index);
            BSTAlg.Rotate(t, par, side);
At(23);     n := t.root;
            SearchTreeIE.CurrentNode(alg, n.index);
          END;
        END; At(1);
        SearchTreeIE.CurrentNode(alg, 0);
        <* ASSERT n # NIL *>
At(24);   IF n.type # NodeType.Black THEN
At(25);     SetType(alg, n, NodeType.Black);
            n.type := NodeType.Black;
        END;
    ZeusCodeView.Exit(alg)
  END FixUp;

BEGIN
  ZeusPanel.RegisterAlg(New, "Red-Black", "SearchTree");
END RedBlackAlg.
