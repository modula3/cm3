(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Wed Jun 23 12:08:18 PDT 1993 by steveg   *)
(*      modified on Sat Jul 18 15:33:58 PDT 1992 by harrison *)


<*PRAGMA LL*>

MODULE GenericTree;

IMPORT Animate, Axis, RefList, MG, MGPublic, MGV, Pts, RefListUtils,
  R2, R2Box, Thread, VBT;

EXCEPTION Fatal;
<* FATAL Fatal, Thread.Alerted *>

REVEAL
  V = PublicV BRANDED OBJECT
      OVERRIDES
        reshape   := MGV.ReshapeLeaveOrigin;
        setBorder := SetBorderV;
        shape     := ShapeV;
        setRoot   := SetRootV;
        init      := InitV;
      END;

REVEAL
  SubTree = PublicSubTree BRANDED OBJECT
            OVERRIDES
              init            := InitSubTree;
              addChild        := AddChildDefault;
              removeChild     := RemoveChildDefault;
              route           := RouteDefault;
              link            := LinkError;
              succ            := SuccError;
              pred            := PredDefault;
              nth             := NthDefault;
              iterate         := IterateDefault;
              bounds          := BoundsError;
              calculateSize := CalculateSizeError;
              translate     := TranslateError;
            END;

REVEAL
  GenericTree = GenericTreePublic BRANDED OBJECT
                OVERRIDES
                  bounds          := BoundsGeneric;
                  succ            := SuccGeneric;
                  addChild        := AddChildGeneric;
                  removeChild     := RemoveChildGeneric;
                  link            := LinkGeneric;
                  calculateSize := CalculateSizeGeneric;
                  translate     := TranslateGeneric;
                END;

REVEAL
  Forest = ForestPublic BRANDED OBJECT
           OVERRIDES
             init := InitForest;
           END;

PROCEDURE InitV (v: V): MG.V =
  BEGIN
    EVAL MG.V.init(v);
    LOCK v.mu DO
      IF v.root # NIL THEN
        v.displayList.addBefore(v, v.root);
      END;
    END;
    RETURN v;
  END InitV;

PROCEDURE ShapeV (v: V; axis: Axis.T;  <* UNUSED *>n: CARDINAL):
  VBT.SizeRange =
  VAR
    sr  : VBT.SizeRange;
    size: REAL;
  BEGIN
    LOCK v.mu DO
      IF v.root = NIL THEN
        size := 0.0;
      ELSIF axis = Axis.T.Hor THEN
        size := v.root.width
      ELSE
        size := v.root.height
      END;
      sr.pref := Pts.ToScreenPixels(v, 2.0 * v.border[axis] + size, axis)
    END;
    sr.lo := 0;
    sr.hi := MAX(sr.pref + 1, VBT.DefaultShape.hi);
    RETURN sr;
  END ShapeV;

PROCEDURE SetRootV (v: V; root: SubTree) =
  VAR bounds: R2Box.T;
  BEGIN
    LOCK v.mu DO
      IF v.root # NIL THEN v.displayList.remove(v, v.root) END;
      v.root := root;
      IF root # NIL THEN
        bounds := root.graphic.appearance.boundingBox(root.graphic, v);
        v.displayList.addBefore(v, v.root);
        root.setVisible(v, 0.0);
      END;
    END;
    VBT.NewShape(v);
    VBT.Mark(v);
  END SetRootV;

PROCEDURE SetBorderV (v: V; border: ARRAY Axis.T OF REAL) =
  BEGIN
    LOCK v.mu DO v.border := border END;
    VBT.NewShape(v);
    VBT.Mark(v);
  END SetBorderV;

PROCEDURE RelayoutAncestors (node: SubTree; v: V) =
  BEGIN
    WHILE node # NIL DO
      node.dirty := TRUE;
      node.calculateSize(v);
      node := node.parent;
    END;
    v.root.translate(
      v, v.nw[1] - v.border[Axis.T.Ver], v.nw[0] + v.border[Axis.T.Hor]);
  END RelayoutAncestors;

<* LL < v.mu *>
PROCEDURE SetRoot (root: SubTree; v: V; ) =
  BEGIN
    v.setRoot(root);
    LOCK v.mu DO RelayoutAncestors(root, v); END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END SetRoot;

<* LL < v.mu *>
PROCEDURE AddChild (node: SubTree; v: V; pred, new: SubTree) =
  BEGIN
    LOCK v.mu DO
      <* ASSERT((pred = NIL OR pred.parent = node) AND new.parent = NIL) *>
      node.addChild(v, pred, new);
      RelayoutAncestors(node, v);
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END AddChild;

PROCEDURE RemoveChild (node: SubTree; v: V; child: SubTree) =
  BEGIN
    LOCK v.mu DO
      <* ASSERT( child.parent = node) *>
      node.removeChild(v, child);
      RelayoutAncestors(node, v);
    END;
    VBT.NewShape(v);
    MGV.Animation(v);
  END RemoveChild;

PROCEDURE Route (ancestor: SubTree; v: V; descendant: SubTree): MG.Group =
  BEGIN
    LOCK v.mu DO RETURN ancestor.route(v, descendant); END;
  END Route;

PROCEDURE Succ (node: SubTree; v: V; pred: SubTree): SubTree =
  BEGIN
    LOCK v.mu DO <* ASSERT (pred = NIL OR pred.parent = node) *>
      RETURN node.succ(v, pred);
    END;
  END Succ;

PROCEDURE Pred(node: SubTree; v: V; succ: SubTree): SubTree =
  BEGIN
    LOCK v.mu DO <* ASSERT (succ = NIL OR succ.parent = node) *>
      RETURN node.pred(v, succ);
    END;
  END Pred;

PROCEDURE Nth (node: SubTree; v: V; n: CARDINAL): SubTree =
  BEGIN
    LOCK v.mu DO RETURN node.nth(v, n); END;
  END Nth;

PROCEDURE NumChildren (node: SubTree; v: V): INTEGER =
  BEGIN
    LOCK v.mu DO RETURN node.numChildren; END;
  END NumChildren;

PROCEDURE Parent (node: SubTree; v: V): SubTree =
  BEGIN
    LOCK v.mu DO RETURN node.parent; END;
  END Parent;

PROCEDURE Iterate (node: SubTree; v: V; iter: ChildrenIterator) =
  BEGIN
    LOCK v.mu DO node.iterate(v, iter); END;
  END Iterate;

PROCEDURE InitSubTree (node: SubTree; v: V; graphic: MG.T): SubTree =
  BEGIN
    EVAL MG.Group.init(node);
    IF node.id # MG.NoID THEN
      MGPublic.Register(v, node.id, node);
    END;
    LOCK v.mu DO
      IF node.linker = NIL THEN node.linker := linkerDefault END;
      node.graphic := graphic;
      node.addBefore(v, graphic);
      node.calculateSize(v);
      MG.TranslateToLocked(graphic, v, R2.Origin, TRUE);
      MG.SetPosLocked(node, R2.Origin, v);
      node.setVisible(v, 0.0);
    END;
    RETURN node
  END InitSubTree;

PROCEDURE LinkerForest (<* UNUSED *> l            : Linker;
                        <* UNUSED *> v            : V;
                        <* UNUSED *> parent, child: SubTree ): LinkerRec =
  BEGIN
    RETURN LinkerRec{NIL, NIL};
  END LinkerForest;

CONST
  R2Epsilon = R2.T{0.001, 0.001};

PROCEDURE InitForest (node: Forest; v: V): Forest =
  BEGIN
    node.linker := NEW(Linker, new := LinkerForest);
    RETURN GenericTree.init(node, v, NEW(MG.Rectangle, visible := 0.0,
                                         weight := 0.0).init(
                                       R2.Origin, R2Epsilon))
  END InitForest;

PROCEDURE BoundsError (<*UNUSED *>               node      : SubTree;
                       <*UNUSED *>               v         : MG.V): R2Box.T =
  BEGIN
    RAISE Fatal
  END BoundsError;

PROCEDURE BoundsGeneric (              node      : SubTree;
                                       v         : MG.V): R2Box.T =
  VAR
    pos := MG.PosLocked(node, v);
    bounds := node.graphic.appearance.boundingBox(node.graphic, v);
    w := pos[0] - node.width / 2.0;
    n := pos[1] + (bounds[1].hi - bounds[1].lo) / 2.0;
    e := w + node.width;
    s := n - node.height;
  BEGIN
    RETURN R2Box.FromEdges(w, e, s, n);
  END BoundsGeneric;

PROCEDURE AddChildDefault (             node : SubTree;
                                        v    : V;
                           <* UNUSED *> pred : SubTree;
                                        child: SubTree  ) =
  BEGIN
    child.parent := node;
    child.setVisible(v, 1.0);
    INC(node.numChildren);
  END AddChildDefault;

PROCEDURE Center (node: GenericTree; v: V): R2.T =
  BEGIN
    RETURN R2Box.Middle(node.graphic.bounds(v));
  END Center;

PROCEDURE LinkerNewDefault (<* UNUSED *> l            : Linker;
                                         v            : V;
                                         parent, child: SubTree ):
  LinkerRec =
  VAR
    link := NEW(MG.Line, weight := 2.0).init(
              to := Center(parent, v), from := Center(child, v));
  BEGIN
    RETURN LinkerRec{parentLink := NEW(MG.LineEnd, line := link,
                                       controlsFrom := FALSE).init(),
                     childLink := NEW(MG.LineEnd, line := link,
                                      controlsFrom := TRUE).init()}
  END LinkerNewDefault;

(* CAUTION: Don't change without also changing AddChildForest *)
PROCEDURE AddChildGeneric (node: GenericTree; v: V; pred, child: SubTree) =
  VAR
    predTail: RefList.T;
    new              := NARROW(child, GenericTree);
    nlr              := node.linker.new(v, node, child);
  BEGIN
    SubTree.addChild(node, v, pred, child);
    IF pred = NIL THEN
      node.children := RefList.Cons(child, node.children);
    ELSE
      predTail := FindGenericChild(node, pred);
      predTail.tail := RefList.Cons(child, predTail.tail);
    END;
    (* assumes if one end is NIL then both are *)
    IF nlr.parentLink # NIL THEN
      new.linkEndParent := nlr.parentLink;
      new.linkEnd := nlr.childLink;
      new.linkEnd.setVisible(v, new.visible);
      (* painting order should be node.graphic, new.graphic, new.linkend
         linkEndParent doesn't get painted *)
      node.addAfter(v, new.linkEndParent); (* bottom *)
      new.addAfter(v, new.linkEnd); (* bottom *)
    END;
    node.addBefore(v, new, node.graphic); (* below graphic *)
  END AddChildGeneric;

PROCEDURE RemoveChildDefault (             node : SubTree;
                              <* UNUSED *> v    : V;
                                           child: SubTree  ) =
  BEGIN
    child.parent := NIL;
    DEC(node.numChildren);
  END RemoveChildDefault;

PROCEDURE RemoveChildGeneric (node: GenericTree; v: V; child: SubTree) =
  VAR ch := NARROW(child, GenericTree);
  BEGIN
    SubTree.removeChild(node, v, child);
    RefListUtils.DeleteQ(node.children, child);
    (* assumes if one end is NIL then both are *)
    IF ch.linkEndParent # NIL THEN
      node.remove(v, ch.linkEndParent);
      ch.remove(v, ch.linkEnd);
    END;
    node.remove(v, ch);
  END RemoveChildGeneric;

PROCEDURE CalculateSizeError (<* UNUSED *> node: SubTree; <* UNUSED *> v: V) =
  BEGIN                         RAISE Fatal
  END CalculateSizeError;

PROCEDURE CalculateSizeGeneric (node: GenericTree; v: V) =
  VAR
    width, height       := 0.0;
    bounds: R2Box.T;
    size: R2.T;
    child               := node.succ(v, NIL);
  BEGIN
    WHILE child # NIL DO
      width := width + child.width;
      height := MAX(height, child.height);
      child := node.succ(v, child);
    END;
    IF node.numChildren > 0 THEN height := height + node.dyChildren END;
    width := width + FLOAT(MAX(0, node.numChildren - 1)) * node.dxChildren;
    bounds := node.graphic.appearance.boundingBox(node.graphic, v);
    size := R2Box.Size(bounds);
    node.width := MAX(size[0], width);
    node.height := size[1] + height;
  END CalculateSizeGeneric;

PROCEDURE TranslateError (<* UNUSED *> node       : SubTree;
                          <* UNUSED *> v          : V;
                          <* UNUSED *> north, west: REAL     ) =
  BEGIN                         RAISE Fatal
  END TranslateError;

TYPE
  FromOrigin = Animate.Linear OBJECT
               OVERRIDES
                 length := FOLength;
                 doStep := FODoStep;
               END;

PROCEDURE FOLength (<* UNUSED *> fo: FromOrigin;
                    <* UNUSED *> v : MG.V;
                    <* UNUSED *> mg: MG.T        ): INTEGER =
  BEGIN
    RETURN 1
  END FOLength;

PROCEDURE FODoStep (fo      : FromOrigin;
                    time    : REAL;
                    timePrev: REAL;
                    v       : MG.V;
                    mg      : MG.T        ) =
  BEGIN
    IF timePrev = 0.0 AND time # 0.0 THEN
      MG.RTranslateLocked(mg, v, fo.vector);
    END;
    IF time = 1.0 AND timePrev # 1.0 THEN
      mg.setVisible(v, 1.0);
    END;
  END FODoStep;

CONST
  Epsilon = 0.01;

PROCEDURE LinearAnimation (v: V; vector: R2.T; mg: SubTree): BOOLEAN =
  VAR a: Animate.T;
  BEGIN
    IF ABS(vector[0]) > Epsilon OR ABS(vector[1]) > Epsilon THEN
      IF v.animations = NIL THEN
        v.animations := NEW(Animate.Group).init()
      END;
      IF MG.PosLocked(mg, v) = R2.Origin THEN
        a := NEW(FromOrigin, vector := vector).init();
      ELSE
        a := NEW(Animate.Linear, vector := vector).init()
      END;
      v.animations.add(v, NEW(Animate.Composite, t := a, mg := mg));
      RETURN TRUE;
    ELSE
      RETURN mg.dirty;
    END;
  END LinearAnimation;

PROCEDURE ParentPos (parent: SubTree; v: V): R2.T =
  BEGIN
    IF parent = NIL THEN
      RETURN R2.Origin;
    ELSE
      RETURN MG.PosLocked(parent, v);
    END;
  END ParentPos;

(* We need to compute the vector which will move node to the correct north,
   west (relative to the parent) *)
PROCEDURE TranslateGeneric (node: GenericTree; v: V; north, west: REAL) =
  VAR
    westCh, northCh: REAL;
    child                 := node.succ(v, NIL);
    ppos                  := ParentPos(node.parent, v);
    bounds := node.graphic.appearance.boundingBox(node.graphic, v);
    size          := R2Box.Size(bounds);
    middle        := R2Box.Middle(bounds);
  BEGIN
    IF LinearAnimation(
         v, R2.T{ppos[0] + west + node.width / 2.0 - middle[0],
                 ppos[1] + north + size[1] / 2.0 - bounds[1].hi}, node) THEN
      (* translate each child so top is dyChildren below graphic's south
         and left edge is dxChildren from prev's right edge *)
      northCh := -size[1] - node.dyChildren;
      westCh := -node.width / 2.0;
      WHILE child # NIL DO
        child.translate(v, northCh, westCh);
        westCh := westCh + child.width + node.dxChildren;
        child := node.succ(v, child);
      END;
    END;
  END TranslateGeneric;

PROCEDURE RouteDefault (node: SubTree; v: V; descendant: SubTree):
  MG.Group =
  VAR group := NEW(MG.Group).init();
  BEGIN
    WHILE descendant # node DO
      group.addBefore(v, descendant);
      VAR link := descendant.link(v);
      BEGIN
        IF link # NIL THEN group.addAfter(v, descendant.link(v)); END;
      END;
      descendant := descendant.parent;
    END;
    group.addBefore(v, node);
    RETURN group;
  END RouteDefault;

PROCEDURE LinkError (<* UNUSED *> node: SubTree; <* UNUSED *> v: V): MG.T =
  BEGIN             RAISE Fatal
  END LinkError;

PROCEDURE LinkGeneric (node: GenericTree; <* UNUSED *> v: V): MG.T =
  BEGIN
    RETURN node.linkEnd
  END LinkGeneric;

PROCEDURE SuccError (<* UNUSED *> node: SubTree;
                     <* UNUSED *> v   : V;
                     <* UNUSED *> pred: SubTree  ): SubTree =
  BEGIN                        RAISE Fatal;
  END SuccError;

(* RefList.First(FindGenericChild(node: Generic; ch: SubTree)) = ch *)
PROCEDURE FindGenericChild (node: GenericTree; ch: SubTree): RefList.T =
  VAR children := node.children;
  BEGIN
    WHILE children.head # ch DO
      children := children.tail;
    END;
    RETURN children;
  END FindGenericChild;

PROCEDURE SuccGeneric (node: GenericTree;  <* UNUSED *>v: V; pred: SubTree):
  SubTree =
  VAR predTail: RefList.T;
  BEGIN
    IF pred = NIL THEN
      predTail := node.children
    ELSE
      predTail := FindGenericChild(node, pred).tail;
    END;
    IF predTail = NIL THEN RETURN NIL ELSE RETURN predTail.head END;
  END SuccGeneric;

PROCEDURE PredDefault (node: SubTree; v: V; succ: SubTree): SubTree =
  VAR
    pred: SubTree := NIL;
    next: SubTree := node.succ(v, NIL);
  BEGIN
    WHILE next # succ DO pred := next; next := node.succ(v, pred); END;
    RETURN pred
  END PredDefault;

PROCEDURE NthDefault (node: SubTree; v: V; n: CARDINAL): SubTree =
  VAR ch := node.succ(v, NIL);
  BEGIN
    FOR i := 1 TO n DO ch := node.succ(v, ch); END;
    RETURN ch;
  END NthDefault;

PROCEDURE IterateDefault (node: SubTree; v: V; iter: ChildrenIterator) =
  VAR ch := node.succ(v, NIL);
  BEGIN
    WHILE ch # NIL AND iter.proc(ch) DO ch := node.succ(v, ch); END;
  END IterateDefault;

BEGIN
  linkerDefault := NEW(Linker, new := LinkerNewDefault);
END GenericTree.
