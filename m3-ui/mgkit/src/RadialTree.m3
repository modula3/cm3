(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Sat Jul 18 15:35:20 PDT 1992 by harrison*)
(*      modified on Wed May 20 01:44:58 1992 by steveg *)

<*PRAGMA LL*>

MODULE RadialTree;

(* A TreeView provides the basic structure for a tree.  A tree must be
   further sub-typed to provide more appropriate or efficient
   representations for specific kinds of trees *)

IMPORT GenericTree, Math, MG, R2, R2Box;

CONST
  Pi    = FLOAT(Math.Pi, LONGREAL);
  TwoPi = 2.0D0 * Pi;

TYPE
  GV = GenericTree.V;
  GT = GenericTree.GenericTree;
  GST = GenericTree.SubTree;

REVEAL
  (* parent of a Tree must be a Tree.  angle field is set if child is a
     tree *)
  V = GV BRANDED OBJECT END;
  T = GT BRANDED OBJECT
            depth  := 0;
            angle  := 0.0D0;
            radius := 0.0;
          OVERRIDES
            init          := Init;
            calculateSize := Size;
            translate     := Translate;
          END;

(* calculate the maximum child radius, then the actual radius of node.
   Must be MAX(2 * max child radius + node's graphic's radius, max child
   radius / sin(2 * pi/num children)) latter is the chord distance between
   children *)
PROCEDURE Size (node: T; v: GV) =
  VAR
    maxChildDiameter, chordalDiameter := 0.0;
    diameter: REAL;
    ch := node.succ(v, NIL);
    bounds := node.graphic.appearance.boundingBox(node.graphic, v);
    size := R2Box.Size(bounds);
  BEGIN
    node.radius := MAX(size[0], size[1]) / 2.0;
    IF node.numChildren > 0 THEN
      WHILE ch # NIL DO
        maxChildDiameter := MAX(maxChildDiameter, ch.width);
        ch := node.succ(v, ch);
      END;
      IF node.numChildren > 2 THEN
        chordalDiameter :=
          maxChildDiameter / FLOAT(
            Math.sin(Pi / FLOAT(node.numChildren, LONGREAL)));
      END;
      node.radius :=
        node.radius + MAX(node.dxChildren + maxChildDiameter / 2.0,
                          chordalDiameter / 2.0);
    END;
    diameter := 2.0 * node.radius + maxChildDiameter;
    node.width := diameter;
    node.height := diameter;
  END Size;

(* center the node at north - diameter/2, west + diameter *)
PROCEDURE Translate (node: T; v: GV; north, west: REAL) =
  VAR
    ppos    := GenericTree.ParentPos(node.parent, v);
    xCenter := ppos[0] + west + node.width / 2.0;
    yCenter := ppos[1] + north - node.height / 2.0;
    ch      := node.succ(v, NIL);
    pos     := MG.PosLocked(node, v);
  BEGIN
    IF node.parent = NIL THEN
      node.depth := 0;
    ELSE
      node.depth := NARROW(node.parent, T).depth + 1;
    END;
    IF GenericTree.LinearAnimation(
         v, R2.T{xCenter - pos[0], yCenter - pos[1]}, node) THEN
      VAR
        alpha := TwoPi / FLOAT(MAX(2, node.numChildren), LONGREAL);
        theta := node.angle;
      BEGIN
        IF node.depth # 0 THEN
          theta := theta + Pi + alpha / 2.0D0;
          IF theta > TwoPi THEN theta := theta - TwoPi END;
        END;
        WHILE ch # NIL DO
          TYPECASE ch OF
          | T (treeChild) => treeChild.angle := theta;
          ELSE
          END;
          ch.translate(
            v, node.radius * FLOAT(Math.sin(theta)) + ch.height / 2.0,
            node.radius * FLOAT(Math.cos(theta)) - ch.width / 2.0);
          ch := node.succ(v, ch);
          theta := theta + alpha;
        END;
      END;
    END;
  END Translate;

PROCEDURE Init (t: T; v: GV; graphic: MG.T): GST =
  BEGIN
    EVAL GT.init(t, v, graphic);
    WITH diameter  = MAX(t.width, t.height),
         dChildren = MAX(t.dxChildren, t.dyChildren) DO
      t.width := diameter;
      t.height := diameter;
      t.dxChildren := dChildren;
      t.dyChildren := dChildren;
    END;
    RETURN t;
  END Init;

BEGIN
END RadialTree.

