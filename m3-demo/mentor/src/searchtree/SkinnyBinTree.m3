(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep 17 14:12:31 PDT 1992 by heydon                   *)
<* PRAGMA LL *>

MODULE SkinnyBinTree;

IMPORT MG, GenericTree, R2Box, R2;

REVEAL
  T = TPublic BRANDED "SkinnyBinTree.T" OBJECT
    OVERRIDES
      calculateSize := CalculateSize;
      translate := Translate;
    END;

PROCEDURE ChildSizes(p: T; v: GenericTree.V; VAR (*OUT*) width,height: REAL) =
(* Set "width" to the sum of the widths of the children of the parent node
   "p", and set "height" to the maximum value of their heights. *)
  VAR ch: T := p.succ(v, NIL); BEGIN
    width := 0.0;
    height := 0.0;
    WHILE ch # NIL DO
      width := width + ch.width;
      height := MAX(height, ch.height);
      ch := p.succ(v, ch);
    END;
  END ChildSizes;

PROCEDURE CalculateSize(node: T; v: GenericTree.V) =
  VAR width, height: REAL; BEGIN
    (* Compute width and height of all children *)
    ChildSizes(node, v, width, height);

    (* add extra space for dxChildren and dyChildren *)
    IF node.numChildren > 0 THEN
      width := width + FLOAT(node.numChildren) * node.dxChildren / 2.0;
      height := height + node.dyChildren
    END;

    (* finally, add space for size of root and dyAbove *)
    WITH size = R2Box.Size(node.graphic.appearance.boundingBox(
                  node.graphic, v)) DO
      IF node.numChildren = 1
        THEN node.width := width + size[0] / 2.0
        ELSE node.width := MAX(size[0], width)
      END;
      node.height := height + size[1] + node.dyAbove;
    END;
  END CalculateSize;

PROCEDURE Translate(node: T; v: GenericTree.V; north, west: REAL) =
  VAR
    dx, dy, northCh: REAL;
    ppos          := GenericTree.ParentPos(node.parent, v);
    leftChWidth   := 0.0;
    bounds        := node.graphic.appearance.boundingBox(node.graphic, v);
    size          := R2Box.Size(bounds);
    middle        := R2Box.Middle(bounds);
  BEGIN
    <* LL = v.mu *>
    IF node.l # NIL
      THEN leftChWidth := node.l.width + (node.dxChildren / 2.0)
      ELSE leftChWidth := size[0] / 2.0
    END;
    dx := ppos[0] + west + leftChWidth - middle[0];
    dy := ppos[1] + north - node.dyAbove - bounds[1].hi;
    IF GenericTree.LinearAnimation(v, R2.T{dx, dy}, node) THEN
      northCh := - ((size[1] / 2.0) + node.dyChildren);
      IF node.l # NIL THEN
        node.l.translate(v, northCh, -leftChWidth);
      END;
      IF node.r # NIL THEN
        node.r.translate(v, northCh, node.dxChildren / 2.0);
      END;
    END;
  END Translate;

BEGIN
END SkinnyBinTree.
