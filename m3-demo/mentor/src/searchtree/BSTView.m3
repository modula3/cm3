(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 14:53:40 PST 1995 by kalsow                   *)
(*      modified on Tue May  3 13:40:57 PDT 1994 by najork                   *)
(*      modified on Wed Jan  6 15:55:45 PST 1993 by steveg                   *)
(*      modified on Thu Sep 17 15:08:36 PDT 1992 by heydon                   *)

MODULE BSTView;

IMPORT BinaryTree, Fmt, GenericTree, MG, MGPublic, MGV, PaintOp, R2, 
       RedBlackAlg, RefList, STView, SkinnyBinTree, Thread, VBT, ZeusPanel;

IMPORT View AS ZeusView;

REVEAL
  T = TPublic BRANDED "BSTView.T" OBJECT
      last_compared: Tree;		 (* node compared to last (or NIL) *)
      links: ARRAY [1..3] OF MG.T;	 (* uncle links *)
      last_red_red_ch: INTEGER;
      last_check_uncle: INTEGER;
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
  (* Tree = BinaryTree.T BRANDED OBJECT dyAbove: REAL END; *)
  View = BinaryTree.V;

CONST
  ChildDx = 4.0;			 (* horiz sep between siblings *)
  ChildDy = 20.0;			 (* vertical sep betw parent/child *)

PROCEDURE New(): ZeusView.T =
  BEGIN RETURN STView.New(NEW(T), NEW(View)) END New;

PROCEDURE UndoPath(view: T) =
  VAR v: View := view.v; link: MG.T; l := view.comp_list; BEGIN
    LOCK v.mu DO
      view.current.graphic.setColor(v, STView.nodeColor);
      WHILE l # NIL DO
        link := l.head;
        link.setWeight(v, STView.ThinWeight);
        l := l.tail
      END
    END;
    view.comp_list := NIL
  END UndoPath;

PROCEDURE NewNode(view: T; node: INTEGER; key: INTEGER) =
  VAR v: View := view.v; BEGIN
    view.current := NEW(Tree, id := node, dxChildren := ChildDx,
      dyChildren := ChildDy).init(v,
        NEW(MG.Ellipse, label := Fmt.Int(key), color := STView.currentColor,
          font := STView.font, weight := STView.ThinWeight).init(R2.Origin,
            R2.T{STView.NodeWidth, STView.NodeHeight}));
    view.last_compared := NIL;
    view.comp_list := NIL;
(*
    VAR
      pos: R2.T := MGPublic.Pos(view.current.graphic, v);
      vector := R2.Sub(R2.Add(R2.T{STView.BorderWidth, STView.BorderHeight},
        R2.T{STView.NodeWidth/2.0, STView.NodeHeight/2.0}), pos);
    BEGIN
      LOCK v.mu DO
        EVAL GenericTree.LinearAnimation(v, vector, view.current)
      END
    END;
    MGV.Animation(v)
*)
  END NewNode;

PROCEDURE CompareKeys(view: T; node: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
    compTree: Tree := MGPublic.Lookup(v, node);
    compNode: MG.T := compTree.graphic;
    link: MG.T := GetLink(compTree, v);
  BEGIN
    (* add "compNode" to list of compared Nodes *)
    IF link # NIL THEN 
      view.comp_list := RefList.Cons (link, view.comp_list) 
    END;

    (* move current node to new compare node *)
    VAR
      cPos: R2.T := MGPublic.Pos(view.current.graphic, v);
      nPos: R2.T := MGPublic.Pos(compNode, v);
      vector := R2.Sub(R2.Add(nPos, R2.T{0.0, STView.NodeHeight+1.0}), cPos);
      speed: REAL := 1.0;
    BEGIN
      IF view.last_compared = NIL THEN speed := 0.0 ELSE
        MGPublic.RemoveFromGroup(view.last_compared, v, view.current);
      END;
      MGPublic.AddToGroup(compTree, v, view.current);
      LOCK v.mu DO
        EVAL GenericTree.LinearAnimation(v, vector, view.current)
      END;
      MGV.Animation(v, speed);
    END;

    (* update weight *)
    IF link # NIL THEN MGPublic.SetWeight(link, v, STView.ThickLineWeight) END;
    view.last_compared := compTree;
  END CompareKeys;

PROCEDURE AddLeaf(view: T; node: INTEGER; childNum: CARDINAL) 
    RAISES {Thread.Alerted} =
(* Sets "view.current" to NIL. *)
  VAR v: View := view.v; BEGIN
    IF node = 0 THEN
      (* new node is a root *)
      SetRoot(view.current, v, FALSE);
      VBT.NewShape(v);
      MGV.Animation(v, 0.0);
    ELSE
      VAR
        parent: Tree := MGPublic.Lookup(v, node);
        lr: BinaryTree.LR;
      BEGIN
        <* ASSERT view.last_compared # NIL *>
        IF childNum = 0
          THEN lr := BinaryTree.LR.Left
          ELSE lr := BinaryTree.LR.Right
        END;
        MGPublic.RemoveFromGroup(view.last_compared, v, view.current);
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
    MGV.Animation(v);
    view.current := NIL;
  END AddLeaf;

PROCEDURE NewSearchKey(view: T; key: INTEGER) =
  VAR v: View := view.v; BEGIN
    view.current := NEW(Tree).init(v,
        NEW(MG.Rectangle, label := Fmt.Int(key), color := STView.currentColor,
          font := STView.font).init(
          R2.Origin, R2.T{STView.NodeWidth, STView.NodeHeight}));
    view.last_compared := NIL;
    view.comp_list := NIL;
  END NewSearchKey;

PROCEDURE SearchEnd(view: T; node: INTEGER) RAISES {Thread.Alerted} =
(* Sets "view.current" to be the found node "node" if "node # 0" *)
  VAR v: View := view.v; n: Tree := NIL; c: PaintOp.ColorScheme; BEGIN
    (* Erase the current node in all cases *)
    IF node # 0 THEN
      n := MGPublic.Lookup(v, node);
      LOCK v.mu DO
        n.remove(v, view.current);
        view.current.graphic.setVisible(v, 0.0)
      END
    END;

    (* Change colors of nodes on path *)
    UndoPath(view);

    (* Highlight found node *)
    view.current := n;
    IF n # NIL THEN
      LOCK v.mu DO
        view.del_node_color := n.graphic.color;
        IF n.graphic.color = red THEN c := whiteRed ELSE c := whiteBlack END;
        n.graphic.setColor(v, c);
      END
    END;
    MGV.Animation(v);
  END SearchEnd;

PROCEDURE GoLeft(view: T; node: INTEGER) RAISES {Thread.Alerted} =
  VAR v: View := view.v; link: MG.Line; BEGIN
    IF node # 0 THEN
      VAR curr: Tree := MGPublic.Lookup(v, node); BEGIN
        (* Add "curr" to the list *)
        view.comp_list := RefList.Cons (curr, view.comp_list);

        (* Make the link thick *)
        link := GetLink(curr, v);
        LOCK v.mu DO link.setWeight(v, STView.ThickLineWeight) END;
      END
    ELSE
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
          t.graphic.setWeight(v, STView.ThickWeight);
          t.graphic.setColor(v, STView.currentColor);
        END
      END;
      view.comp_list := NIL;
    END;
    MGV.Animation(v)
  END GoLeft;

PROCEDURE SpliceOut(view: T; parent, child: INTEGER; save: BOOLEAN) 
    RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
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
      SetRoot(ch, v);
    ELSE
      (* "parent" is not the root *)
      VAR dir: REAL; BEGIN
        IF pp.l = par
          THEN lr := BinaryTree.LR.Left; dir := 1.0
          ELSE lr := BinaryTree.LR.Right; dir := -1.0
        END;
        IF ch = NIL
          THEN layoutNode := pp
          ELSE layoutNode := ch
        END;
        LOCK v.mu DO
          pp.removeChild(v, par);
          (* par.graphic.setVisible(v, 1.0); *)
          WITH shift = dir * STView.NodeWidth DO
            EVAL GenericTree.LinearAnimation(v, R2.T{shift, 0.0}, par)
          END;
          pp.set(v, lr, ch);
          GenericTree.RelayoutAncestors(layoutNode, v)
        END
      END;
      IF save THEN
        MGPublic.AddToGroup(v.displayList, v, par);
      END;
      VBT.NewShape(v);
      MGV.Animation(v);
    END
  END SpliceOut;

PROCEDURE Copy (view: T; source, dest: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
    src: Tree := MGPublic.Lookup(v, source);
    dst: Tree := MGPublic.Lookup(v, dest);
    src_pos: R2.T := MGPublic.Pos(src.graphic, v);
    dst_pos: R2.T := MGPublic.Pos(dst.graphic, v);
  BEGIN
    (* Move "source" to "dest" *)
    MGPublic.RemoveFromGroup(v.displayList, v, src);
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

PROCEDURE CurrentNode (view: T; node: INTEGER) RAISES {Thread.Alerted} =
  VAR v: View := view.v; BEGIN
    IF view.current # NIL THEN
      LOCK v.mu DO view.current.graphic.setWeight(v, STView.ThinWeight) END
    END;
    IF node # 0 THEN
      VAR n: Tree := MGPublic.Lookup(view.v, node); BEGIN
        view.current := n;
        LOCK v.mu DO n.graphic.setWeight(v, STView.ThickWeight) END;
      END;
    ELSE
      view.current := NIL
    END;
    MGV.Animation(v)
  END CurrentNode;

PROCEDURE SetType (             view : T;
                                node : INTEGER;
                                type : RedBlackAlg.NodeType;
                   <* UNUSED *> pType: RedBlackAlg.NodeType) 
    RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
    c: PaintOp.ColorScheme;
    n: Tree := MGPublic.Lookup(v, node);
  BEGIN
    CASE type OF
      RedBlackAlg.NodeType.Red =>   c := red;
    | RedBlackAlg.NodeType.Black => c := black;
    END;
    LOCK v.mu DO n.graphic.setColor(v, c) END;
    MGV.Animation(v)
  END SetType;

PROCEDURE RedRedClash (             view  : T;
                                    child : INTEGER;
                       <* UNUSED *> parent: INTEGER;
                                    on    : BOOLEAN) RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
    ch: Tree := MGPublic.Lookup(v, child);
    link: MG.Line := GetLink(ch, v);
  BEGIN
    IF on THEN
      LOCK v.mu DO
        link.setColor(v, redBg);
        link.setWeight(v, STView.ThickLineWeight);
      END;
      view.last_red_red_ch := child;
    ELSE
      LOCK v.mu DO
        link.setColor(v, blackBg);
        (* next line necessary due to a bug in MG *)
        link.setWeight(v, STView.ThickLineWeight);
      END;
      view.last_red_red_ch := 0;
    END;
    MGV.Animation(v);
  END RedRedClash;

PROCEDURE CheckUncle(view: T; child: INTEGER) RAISES {Thread.Alerted} =
  VAR v: View := view.v; weight: REAL; BEGIN
    IF child = 0 THEN
      weight := STView.ThinWeight;
      IF view.last_red_red_ch = view.last_check_uncle THEN
        view.links[1] := NIL
      END;
    ELSE
      VAR
        ch: Tree := MGPublic.Lookup(v, child);
        par: Tree := ch.parent;
        pp: Tree := par.parent;
        uncle: Tree;
      BEGIN
        view.links[1] := GetLink(ch, v);
        view.links[2] := GetLink(par, v);
        IF pp.l = par
          THEN uncle := pp.r
          ELSE uncle := pp.l
        END;
        IF uncle = NIL
          THEN view.links[3] := NIL
          ELSE view.links[3] := GetLink(uncle, v)
        END;
        weight := STView.ThickLineWeight
      END
    END;
    LOCK v.mu DO
      FOR i := 1 TO 3 DO
        IF view.links[i] # NIL THEN
          view.links[i].setWeight(v, weight)
        END
      END
    END;
    MGV.Animation(v);
    view.last_check_uncle := child;
  END CheckUncle;

PROCEDURE GetChild(t: Tree; lr: BinaryTree.LR): Tree =
  BEGIN
    CASE lr OF <* NOWARN *>
      BinaryTree.LR.Left  => RETURN NARROW(t.l, Tree)
    | BinaryTree.LR.Right => RETURN NARROW(t.r, Tree)
    END
  END GetChild;

PROCEDURE Rotate(view: T; child, parent: INTEGER) RAISES {Thread.Alerted} =
  VAR
    v: View := view.v;
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

PROCEDURE GetLink(n: Tree; v: View): MG.T =
(* Returns the MG.Line that connects the node "n" to its parent in view
   "v". This routine is necessary as a workaround to a bug in GenericTree.
   The problem is that "n.link(v)" returns a MG.LineEnd, and setting an
   attribute of the LineEnd is not setting the attribute of the corresponding
   MG.Line. *)
  VAR le: MG.LineEnd := n.link(v); BEGIN
    IF le = NIL THEN RETURN NIL ELSE RETURN le.line END
  END GetLink;

PROCEDURE SetRoot(t: Tree; v: View; animate := TRUE; relayout := TRUE) 
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
  red := MGPublic.ColorFromText("Red");
  black := MGPublic.ColorFromText("LightGrey", "Black");
  redBg := MGPublic.ColorFromText("Black", "Red");
  blackBg := PaintOp.bgFg;
  whiteRed := MGPublic.ColorFromText("White", "Red");
  whiteBlack := MGPublic.ColorFromText("White", "Black");
  ZeusPanel.RegisterView(New, "Tree", "SearchTree");
END BSTView.
