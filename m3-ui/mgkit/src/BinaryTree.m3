(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Sun Jul 26 00:55:19 1992 by steveg   *)
(*      modified on Sat Jul 18 15:50:40 PDT 1992 by harrison *)


<*PRAGMA LL*>

MODULE BinaryTree;

IMPORT GenericTree, MG, R2, R2Box;

REVEAL
  V = GenericTree.V BRANDED OBJECT END;
  T = PublicT BRANDED OBJECT
      OVERRIDES
        calculateSize := CalculateSize;
        translate := Translate;
        removeChild := RemoveChild;
        set := Set;
      END;

TYPE
  GV = GenericTree.V;
  GST = GenericTree.SubTree;

PROCEDURE MaxChildSize (node: T; v: GV; VAR (* out *) width, height: REAL) =
  VAR ch: T := node.succ(v, NIL);
  BEGIN
    width := 0.0;
    height := 0.0;
    WHILE ch # NIL DO
      width := MAX(width, ch.width);
      height := MAX(height, ch.height);
      ch := node.succ(v, ch);
    END;
  END MaxChildSize;

(* layout with room for two max-sized children *)
PROCEDURE CalculateSize (node: T; v: GV) =
  VAR height, width: REAL;
  BEGIN
    MaxChildSize(node, v, width, height);
    IF node.numChildren > 0 THEN
      width := 2.0 * width + node.dxChildren;
      height := height + node.dyChildren
    END;
    WITH size = R2Box.Size(
                  node.graphic.appearance.boundingBox(node.graphic, v)) DO
      node.width := MAX(size[0], width);
      node.height := size[1] + height;
    END;
  END CalculateSize;

(* special invariant, node.width of all siblings is same.  So maintain, set
   width of children to same value (parent.width - dxChildren) / 2.0 *)
PROCEDURE Translate (node: T; v: GV; north, west: REAL) =
  VAR
    northCh: REAL;
    ppos          := GenericTree.ParentPos(node.parent, v);
    width         := node.width;
    chWidth       := (width - node.dxChildren) / 2.0;
    bounds        := node.graphic.appearance.boundingBox(node.graphic, v);
    size          := R2Box.Size(bounds);
    middle        := R2Box.Middle(bounds);
  BEGIN
    IF GenericTree.LinearAnimation(
         v, R2.T{ppos[0] + west + width / 2.0 - middle[0],
                 ppos[1] + north + size[1] / 2.0 - bounds[1].hi}, node) THEN
      (* translate each child so top is dyChildren below graphic's south
         and left edge is dxChildren from prev's right edge *)
      northCh := -size[1] - node.dyChildren;
      IF node.l # NIL THEN
        node.l.width := chWidth;
        node.l.translate(v, northCh, -width / 2.0);
      END;
      IF node.r # NIL THEN
        node.r.width := chWidth;
        node.r.translate(v, northCh, node.dxChildren / 2.0);
      END;
    END;
  END Translate;

PROCEDURE RemoveChild (node: T; v: GV; ch: GST) =
  BEGIN
    IF node.l = ch THEN
      node.l := NIL
    ELSE <* ASSERT ch = node.r *>
      node.r := NIL
    END;
    GenericTree.GenericTree.removeChild(node, v, ch);
  END RemoveChild;

PROCEDURE Set(node: T; v: V; lr: LR; new: T)= 
  BEGIN
    IF lr = LR.Left THEN
      IF node.l # NIL THEN node.removeChild(v, node.l) END;
      IF new # NIL THEN node.addChild(v, NIL, new) END;
      node.l := new;
    ELSE
      IF node.r # NIL THEN node.removeChild(v, node.r) END;
      IF new # NIL THEN node.addChild(v, node.l, new) END;
      node.r := new;
    END;
  END Set;

BEGIN
END BinaryTree.
