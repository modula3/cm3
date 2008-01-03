(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue May  3 13:24:53 PDT 1994 by najork                   *)
(*      modified on Thu Sep 17 15:35:45 PDT 1992 by heydon                   *)

MODULE SkelView;

IMPORT BSTView, BinaryTree, GenericTree, MG, MGPublic, MGV, PaintOp, R2, 
       RedBlackAlg, RefList, STView, SkinnyBinTree, Thread, VBT, View, 
       ZeusPanel;

REVEAL
  T = BSTView.T BRANDED "SkelView.T" OBJECT
      splice_par, splice_ch: INTEGER;
    OVERRIDES
      startrun := STView.StartRun;
      oeNewNode := NewNode;
      oeCompareKeys := CompareKeys;
      oeAddLeaf := AddLeaf;
      oeNewSearchKey := NewSearchKey;
      oeSearchEnd := SearchEnd;
      oeGoLeft := GoLeft;
      oeSpliceOut := SpliceOut;
      oeCopy := Copy;
      oeCurrentNode := CurrentNode;
      oeSetType := SetType;
      oeRedRedClash := RedRedClash;
      oeCheckUncle := CheckUncle;
      oeRotate := Rotate;
    END;

TYPE
  Tree = SkinnyBinTree.T;

CONST
  ThickLineWeight = STView.ThickLineWeight;
  ThickWeight = 6.0;

PROCEDURE New(): View.T =
  BEGIN RETURN STView.New(NEW(T), NEW(BinaryTree.V)) END New;

PROCEDURE UndoPath(view: T) =
  VAR v: BinaryTree.V := view.v; link: MG.T; l := view.comp_list; BEGIN
    LOCK v.mu DO
      WHILE l # NIL DO
        link := l.head;
        link.setWeight(v, STView.ThinWeight);
        l := l.tail
      END
    END;
    view.comp_list := NIL
  END UndoPath;

PROCEDURE NewNode(view: T; node: INTEGER; <* UNUSED *> key: INTEGER) =
  BEGIN
    view.current := NEW(Tree, id := node, dxChildren := ChildDx,
      dyChildren := ChildDy).init(view.v,
        NEW(MG.Ellipse, label := "", color := STView.nodeColor,
          font := STView.font, weight := STView.ThinWeight).init(R2.Origin,
            R2.T{NodeWidth, NodeHeight}));
    view.comp_list := NIL
  END NewNode;

PROCEDURE CompareKeys(view: T; node: INTEGER) =
  VAR
    v: BinaryTree.V := view.v;
    compTree: Tree := MGPublic.Lookup(v, node);
    link: MG.T := GetLink(compTree, v);
  BEGIN
    (* add "compNode" to list of compared Nodes *)
    IF link # NIL THEN 
      view.comp_list := RefList.Cons (link, view.comp_list) 
    END;

    (* update weights *)
    LOCK v.mu DO
      IF link # NIL THEN link.setWeight(v, ThickLineWeight) END;
    END;
  END CompareKeys;

PROCEDURE AddLeaf(view: T; node: INTEGER; childNum: CARDINAL) 
    RAISES {Thread.Alerted} =
  VAR v: BinaryTree.V := view.v; BEGIN
    IF node = 0 THEN
      (* new node is a root *)
      SetRoot(view.current, v, FALSE);
      VBT.NewShape(v);
      MGV.Animation(v, 0.0);
    ELSE
      VAR
        parent: Tree := MGPublic.Lookup(v, node);
        vector: R2.T;
        lr: BinaryTree.LR;
      BEGIN
        (* instantaneously move node to just under its parent *)
        vector := MGPublic.Pos(parent.graphic, v);
        vector := R2.Sub(vector, R2.T{0.0, NodeHeight});
        vector := R2.Sub(vector, MGPublic.Pos(view.current.graphic, v));
        LOCK v.mu DO
          EVAL GenericTree.LinearAnimation(v, vector, view.current)
        END;
        MGV.Animation(v, 0.0);

        (* set new child *)
        IF childNum = 0
          THEN lr := BinaryTree.LR.Left
          ELSE lr := BinaryTree.LR.Right
        END;
        LOCK v.mu DO
          parent.set(v, lr, view.current);
          GenericTree.RelayoutAncestors(parent, v);
        END;
        VBT.NewShape(v);
        VBT.Mark(v);
        MGV.Animation(v);
      END
    END;

    (* Change colors of nodes on path *)
    UndoPath(view);
    MGV.Animation(v, 0.0);
  END AddLeaf;

PROCEDURE NewSearchKey(view: T; <* UNUSED *> key: INTEGER) =
  BEGIN
    view.current := NIL;
    view.comp_list := NIL;
  END NewSearchKey;

PROCEDURE SearchEnd (view: T; node: INTEGER) RAISES {Thread.Alerted} =
(* Sets "view.current" to be the found node "node" if "node # 0" *)
  VAR v: BinaryTree.V := view.v; n: Tree := NIL; c: PaintOp.ColorScheme; BEGIN
    (* Erase the current node in all cases *)
    IF node # 0 THEN
      n := MGPublic.Lookup(v, node);
    END;

    (* Show the search path down to this node *)
    MGV.Animation(v, 0.0);

    (* Highlight found node *)
    view.current := n;
    IF n # NIL THEN
      LOCK v.mu DO
        view.del_node_color := n.graphic.color;
        IF n.graphic.color = BSTView.red
          THEN c := BSTView.whiteRed
          ELSE c := BSTView.whiteBlack
        END;
        n.graphic.setColor(v, c);
      END
    END;
    MGV.Animation(v);

    (* Undo search path highlighting *)
    UndoPath(view);
    MGV.Animation(v, 0.0);
  END SearchEnd;

PROCEDURE GoLeft(view: T; node: INTEGER) RAISES {Thread.Alerted} =
  VAR v: BinaryTree.V := view.v; link: MG.Line; BEGIN
    IF node # 0 THEN
      VAR curr: Tree := MGPublic.Lookup(v, node); BEGIN
        (* Add "curr" to the list *)
        view.comp_list := RefList.Cons (curr, view.comp_list);

        (* Make the link thick *)
        link := GetLink(curr, v);
        LOCK v.mu DO link.setWeight(v, ThickLineWeight) END;
      END
    ELSE
      (* display lines from accumulated "GoLeft" calls *)
      MGV.Animation(v);

      (* make all links on "view.comp_list" thin again *)
      VAR l := view.comp_list; t: Tree; BEGIN
        LOCK v.mu DO
          WHILE l # NIL DO
            t := l.head;
            link := GetLink(t, v);
            link.setWeight(v, STView.ThinWeight);
            l := l.tail;
          END
        END
      END;

      (* set color of last "GoLeft" node *)
      VAR t: Tree := view.comp_list.head; BEGIN
        LOCK v.mu DO
          t.graphic.setWeight(v, ThickWeight);
          t.graphic.setColor(v, STView.currentColor);
        END
      END;
      view.comp_list := NIL;
      MGV.Animation(v)
    END;
  END GoLeft;

PROCEDURE SpliceOut(view: T; parent, child: INTEGER; save: BOOLEAN) 
  RAISES {Thread.Alerted} =
  VAR
    v: BinaryTree.V := view.v;
    ch: Tree := NIL;
    par: Tree := MGPublic.Lookup(v, parent);
    pp: Tree := GenericTree.Parent(par, v);
    layoutNode: Tree;
    lr: BinaryTree.LR;
  BEGIN
    IF child # 0 THEN
      ch := MGPublic.Lookup(v, child);
      <* ASSERT ch # NIL *>
      par.removeChild(v, ch);
    END;
    IF pp = NIL THEN
      (* "parent" is the current root of the tree *)
      SetRoot(ch, v, FALSE);
    ELSE
      (* "parent" is not the root *)
      IF pp.l = par
        THEN lr := BinaryTree.LR.Left
        ELSE lr := BinaryTree.LR.Right
      END;
      IF ch = NIL
        THEN layoutNode := pp
        ELSE layoutNode := ch
      END;
      LOCK v.mu DO
        pp.removeChild(v, par);
        pp.set(v, lr, ch);
        GenericTree.RelayoutAncestors(layoutNode, v);
      END;
      IF NOT save THEN
        VBT.NewShape(v);
        MGV.Animation(v);
      END;
    END
  END SpliceOut;

PROCEDURE Copy(view: T; source, dest: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v: BinaryTree.V := view.v;
    src: Tree := MGPublic.Lookup(v, source);
    dst: Tree := MGPublic.Lookup(v, dest);
    src_pos: R2.T := MGPublic.Pos(src.graphic, v);
    dst_pos: R2.T := MGPublic.Pos(dst.graphic, v);
  BEGIN
    (* Move "source" to "dest" *)
    MGPublic.AddToGroup(dst, v, src);
    LOCK v.mu DO
      EVAL GenericTree.LinearAnimation(v, R2.Sub(dst_pos, src_pos), src);
    END;
    MGV.Animation(v);

    (* Make source invisible and copy source label *)
    LOCK v.mu DO
      src.graphic.setVisible(v, 0.0);
      dst.graphic.setLabel(v, src.graphic.label);
      dst.graphic.setColor(v, view.del_node_color);
    END;
    MGPublic.RemoveFromGroup(dst, v, src);
    MGV.Animation(v)
  END Copy;

PROCEDURE CurrentNode(view: T; node: INTEGER) RAISES {Thread.Alerted} =
  VAR v: BinaryTree.V:= view.v; BEGIN
    IF view.current # NIL THEN
      LOCK v.mu DO view.current.graphic.setWeight(v, STView.ThinWeight) END
    END;
    IF node # 0 THEN
      VAR n: Tree := MGPublic.Lookup(view.v, node); BEGIN
        view.current := n;
        LOCK v.mu DO n.graphic.setWeight(v, ThickWeight) END;
      END;
    ELSE
      view.current := NIL
    END;
    MGV.Animation(v, 0.0)
  END CurrentNode;

PROCEDURE SetType (             view : T;
                                node : INTEGER;
                                type : RedBlackAlg.NodeType;
                   <* UNUSED *> pType: RedBlackAlg.NodeType) 
    RAISES {Thread.Alerted} =
  VAR
    v: BinaryTree.V := view.v;
    c: PaintOp.ColorScheme;
    n: Tree := MGPublic.Lookup(v, node);
  BEGIN
    CASE type OF
      RedBlackAlg.NodeType.Red =>  c := BSTView.red;
    | RedBlackAlg.NodeType.Black => c := BSTView.black;
    END;
    LOCK v.mu DO n.graphic.setColor(v, c) END;
    MGV.Animation(v)
  END SetType;

PROCEDURE RedRedClash (             view: T;
                                    child: INTEGER;
                       <* UNUSED *> parent: INTEGER;
                                    on: BOOLEAN) RAISES {Thread.Alerted} =
  VAR
    v: BinaryTree.V := view.v;
    ch: Tree := MGPublic.Lookup(v, child);
    link: MG.Line := GetLink(ch, v);
  BEGIN
    IF on THEN
      LOCK v.mu DO
        link.setColor(v, BSTView.redBg);
        link.setWeight(v, ThickLineWeight);
      END;
    ELSE
      LOCK v.mu DO
        link.setColor(v, BSTView.blackBg);
        link.setWeight(v, STView.ThinWeight);
      END;
    END;
    MGV.Animation(v, 0.0);
  END RedRedClash;

PROCEDURE CheckUncle(<* UNUSED *> view: T; <* UNUSED *> child: INTEGER) =
  BEGIN
  END CheckUncle;

PROCEDURE GetChild(t: Tree; lr: BinaryTree.LR): Tree =
  BEGIN
    CASE lr OF <* NOWARN *>
      BinaryTree.LR.Left  => RETURN NARROW(t.l, Tree)
    | BinaryTree.LR.Right => RETURN NARROW(t.r, Tree)
    END
  END GetChild;

PROCEDURE Rotate (view: T; child, parent: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v: BinaryTree.V := view.v;
    ch: Tree := MGPublic.Lookup(v, child);
    par: Tree := MGPublic.Lookup(v, parent);
    pp: Tree := GenericTree.Parent(par, v);
    lr, rl: BinaryTree.LR;
    ch_chIn: Tree;
    link: MG.Line := GetLink(ch, v);
    color: PaintOp.ColorScheme := link.color;
    weight: REAL := link.weight;
  BEGIN
    (* compute orientation *)
    IF ch = par.l
      THEN lr := BinaryTree.LR.Left; rl := BinaryTree.LR.Right
      ELSE lr := BinaryTree.LR.Right; rl := BinaryTree.LR.Left
    END;

    (* find "inner" child of child *)
    ch_chIn := GetChild(ch, rl);

    (* break 2 links *)
    LOCK v.mu DO
      par.set(v, lr, NIL);
      ch.set(v, rl, NIL);
    END;

    (* update root *)
    IF pp = NIL THEN
      SetRoot(ch, v, FALSE, FALSE)
    ELSE
      LOCK v.mu DO
        IF par = pp.l
          THEN pp.set(v, BinaryTree.LR.Left, ch)
          ELSE pp.set(v, BinaryTree.LR.Right, ch)
        END
      END
    END;

    (* update other two pointers and link attributes *)
    LOCK v.mu DO
      par.set(v, lr, ch_chIn);
      ch.set(v, rl, par);
      link := GetLink(par, v);
      link.setColor(v, color);
      link.setWeight(v, weight);

      (* make it happen *)
      IF ch_chIn # NIL
        THEN GenericTree.RelayoutAncestors(ch_chIn, v);
        ELSE GenericTree.RelayoutAncestors(par, v);
      END;
    END;
    MGV.Animation(v)
  END Rotate;

PROCEDURE GetLink(n: Tree; v: BinaryTree.V): MG.T =
(* Returns the MG.Line that connects the node "n" to its parent in view
   "v". This routine is necessary as a workaround to a bug in GenericTree.
   The problem is that "n.link(v)" returns a MG.LineEnd, and setting an
   attribute of the LineEnd is not setting the attribute of the corresponding
   MG.Line. *)
  VAR le: MG.LineEnd := n.link(v); BEGIN
    IF le = NIL THEN RETURN NIL ELSE RETURN le.line END
  END GetLink;

PROCEDURE SetRoot(t: Tree; v: BinaryTree.V; animate := TRUE; relayout:=TRUE) 
    RAISES {Thread.Alerted} =
(* This procedure is a workaround for 2 bugs in GenericTree.SetRoot: 1) this
   procedure crashes when the root is set to NIL, and 2) it makes the entire
   tree invisible.

   If "animate", then the setroot takes place immediately. Otherwise, the
   animations are accumulated for a later MGV.Animation(v).
*)
  BEGIN
    v.setRoot(t);
    IF t # NIL THEN
      LOCK v.mu DO
        t.setVisible(v, 1.0);
        IF relayout THEN
          GenericTree.RelayoutAncestors(t, v)
        END
      END
    END;
    IF animate THEN
      VBT.NewShape(v);
      MGV.Animation(v)
    END
  END SetRoot;

BEGIN
  ZeusPanel.RegisterView (New, "Tree Skeleton", "SearchTree");
END SkelView.
