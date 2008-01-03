(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 15 11:09:44 PDT 1994 by heydon                   *)
(*      modified on Tue May  3 13:52:07 PDT 1994 by najork                   *)
(*      modified on Thu Sep 24 14:46:10 PDT 1992 by mhb                      *)
(*      modified on Tue Sep  8 20:30:30 PDT 1992 by johnh                    *)

MODULE UnbalancedAlg;

IMPORT Algorithm, BSTAlg, RefList, SearchTreeIE, ZeusCodeView, ZeusPanel;

FROM Thread IMPORT Alerted;

TYPE
  T = BSTAlg.T BRANDED "UnbalancedAlg.T" OBJECT
    OVERRIDES
      run := Run;
    END;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN NEW (T, 
                data := ZeusPanel.NewForm("SearchTree.fv"),
                codeViews := 
                    RefList.List2 (
                        RefList.List2("Pseudo-Code", "Unbalanced.pseudo"),
                        RefList.List2("Modula-3 Code", "Unbalanced.m3"))
               ).init()
  END New;

PROCEDURE Run(alg: T) RAISES {Alerted} =
  VAR data: BSTAlg.PanelData; keys: BSTAlg.Keys;
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    (* Read input data from form *)
    data := BSTAlg.GetPanelData(alg.data);

    (* Construct new keys *)
    keys := BSTAlg.NewKeys(data, input := TRUE);

    (* Insert all keys into empty tree *)
    ZeusCodeView.Enter(alg, "UnbalancedTest");
At(1); alg.tree := NEW(BSTAlg.Tree);
       VAR n: BSTAlg.Node; BEGIN
At(2);   FOR i := 0 TO LAST(keys^) DO 
At(3);     n := NEW(BSTAlg.Node, index := BSTAlg.NewIndex(), key := keys[i]);
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

PROCEDURE Insert(alg: BSTAlg.T; n: BSTAlg.Node) RAISES {Alerted} =
  VAR x, y: BSTAlg.Node; t: BSTAlg.Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "Insert");
        SearchTreeIE.NewNode(alg, n.index, n.key);
At(3);  x := t.root;
At(4);  y := NIL;
        WHILE x # t.nil DO At(5);
At(6);    y := x;
          SearchTreeIE.CompareKeys(alg, x.index);
At(7);    IF n.key < x.key THEN
At(8);      x := x.left
          ELSE
At(9);      x := x.right
          END
        END; At(5);
At(10); n.parent := y;
At(11); IF y = NIL THEN
          SearchTreeIE.AddLeaf(alg, 0, 0);
At(12);   t.root := n
        ELSE
At(13);   IF n.key < y.key THEN
            SearchTreeIE.AddLeaf(alg, y.index, 0);
At(14);     y.left := n
          ELSE
            SearchTreeIE.AddLeaf(alg, y.index, 1);
At(15);     y.right := n
          END
        END;
    ZeusCodeView.Exit(alg)
  END Insert;

PROCEDURE Search(alg: BSTAlg.T; key: BSTAlg.Key): BSTAlg.Node
  RAISES {Alerted} =
  VAR x: BSTAlg.Node; t: BSTAlg.Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "Search");
At(1);  SearchTreeIE.NewSearchKey(alg, key);
        x := t.root;
        WHILE x # NIL AND x.key # key DO At(2);
At(3);    SearchTreeIE.CompareKeys(alg, x.index);
          IF key < x.key THEN
At(4);      x := x.left
          ELSE
At(5);      x := x.right
          END
        END;
        IF x # NIL THEN
          SearchTreeIE.CompareKeys(alg, x.index);
          SearchTreeIE.SearchEnd(alg, x.index)
        ELSE
          SearchTreeIE.SearchEnd(alg, 0)
        END; At(3); At(6);
    ZeusCodeView.Exit(alg);
    RETURN x
  END Search;

PROCEDURE Delete(alg: BSTAlg.T; n: BSTAlg.Node) RAISES {Alerted} =
  VAR y: BSTAlg.Node;
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    <* ASSERT n # NIL *>
    ZeusCodeView.Enter(alg, "Delete");
        (* Set "y" to node to splice out *)
At(1);  IF n.left = NIL OR n.right = NIL THEN
At(2);    y := n
        ELSE
At(3);    y := FindMin(alg, n.right);
        END;
        
        (* Splice out node "y" *)
At(4);  SpliceOut(alg, y, y # n);

        (* Replace "n" by "y" if necessary *)
At(5);  IF y # n THEN
At(6);    n.key := y.key;
          SearchTreeIE.Copy(alg, y.index, n.index)
        END;
    ZeusCodeView.Exit(alg)
  END Delete;

PROCEDURE FindMin(alg: BSTAlg.T; n: BSTAlg.Node): BSTAlg.Node
  RAISES {Alerted} =
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "FindMin");
          SearchTreeIE.GoLeft(alg, n.index);
          WHILE n.left # NIL DO At(1);
            SearchTreeIE.GoLeft(alg, n.left.index);
At(2);      n := n.left
          END; At(1);
At(3);    SearchTreeIE.GoLeft(alg, 0);
    ZeusCodeView.Exit(alg);
    RETURN n
  END FindMin;

PROCEDURE SpliceOut(alg: T; n: BSTAlg.Node; save: BOOLEAN) RAISES {Alerted} =
  VAR x: BSTAlg.Node; t: BSTAlg.Tree := alg.tree;
  PROCEDURE At(line: CARDINAL) RAISES {Alerted} =
    BEGIN ZeusCodeView.At(alg, line) END At;
  BEGIN
    ZeusCodeView.Enter(alg, "SpliceOut");
        <* ASSERT NOT (n.left # NIL AND n.right # NIL) *>
        (* set "x" to the child of "n" if it has one *)
At(1);  IF n.left # NIL THEN
At(2);    x := n.left
        ELSE
At(3);    x := n.right
        END;
        IF x # NIL
          THEN SearchTreeIE.SpliceOut(alg, n.index, x.index, save);
          ELSE SearchTreeIE.SpliceOut(alg, n.index, 0, save);
        END;

        (* update "up" pointer *)
At(4);  IF x # NIL THEN
At(5);    x.parent := n.parent
        END;

        (* update "down" pointer *)
At(6);  IF n.parent = NIL THEN
At(7);    t.root := x
        ELSE
At(8);    IF n = n.parent.left THEN
At(9);      n.parent.left := x
          ELSE
At(10);     n.parent.right := x
          END
        END;
    ZeusCodeView.Exit(alg);
  END SpliceOut;

BEGIN
  ZeusPanel.RegisterAlg(New, "Unbalanced", "SearchTree");
END UnbalancedAlg.
