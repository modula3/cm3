(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun 22 11:50:52 PDT 1993 by steveg   *)

<*PRAGMA LL*>

INTERFACE GenericTree;

(* A SubTree provides the basic structure for a tree.
   A GenericTree provides the layout methods for a general tree.
   A tree can be further sub-typed to provide more appropriate
   or efficient representations for specific kinds of trees
*)

IMPORT Axis, RefList, MG, R2;

TYPE
  V <: PublicV;
  PublicV = MG.V OBJECT
              <* LL = self.mu *>
              root  : SubTree := NIL;
            METHODS
              <* LL < self.mu *>
              setRoot   (root: SubTree);
              setBorder (border: ARRAY Axis.T OF REAL);
            END;
(* A vbt displaying a tree.  A forest can be displayed using a forest root
   node (below) *)

TYPE
  SubTree <: PublicSubTree;
  PublicSubTree =
    MG.Group OBJECT
      <* LL = v.mu *>
      graphic    : MG.T    := NIL;
      parent     : SubTree := NIL;
      numChildren: INTEGER := 0;

      width, height: REAL;
      (* MGPublic.Pos(self) is the "control point" of the subtree's
         rectangle.  "width" and "height" are the dimensions of the
         subtree.

         The node is responsible for maintaining its own width and height
         as children are added and removed.  The parent of the node
         provides the north and west (relative to the parent's control
         point) when it calls translate. *)

      linker: Linker := NIL;
      (* self.linker.new(parent, child) returns a graphical element that
         acts as a link connecting parent and child.  The link ends should
         be individually controllable so the parent and child can move
         separately.

         If linker = NIL, linkerDefault is used and returns a MG.Line with
         MG.LineEnds at parent and child.  The visibility of the child
         controls the visibility of the default link. *)
    METHODS
      <* LL < v.mu *>
      init (v: V; graphic: MG.T): SubTree;
      (* adds graphic to the node, sets the linker to the default linker
         (if it is NIL), calculates the node's size, centers the node (and
         graphic) around the origin, sets visibility to 0.0 (invisible). *)

      <* LL = v.mu *>

      addChild    (v: V; pred, child: SubTree);
      removeChild (v: V; child: SubTree);
      (* add or remove child

         A subtype must supply "addChild" and "removeChild" methods.  For
         addChild, if pred = NIL then add as the first child.

         The default methods adjust numChildren and sets child's parent.

         The subtype is responsible for adjusting its data structures.

         The caller is responsible for calling calculateSize and translate
         soon. *)

      route (v: V; descendant: SubTree): MG.Group;
      (* Return a group of the graphical elements (nodes and links)
         connecting the current node to the given descendant (inclusive).

         The default method constructs the group using link and the parent
         links up from descendant. *)

      succ (v: V; pred: SubTree): SubTree;
      (* A subtype must override the "succ" method.  The default methods
         for "pred", "nth", and "iterate" are all built out of "succ". *)

      pred (v: V; succ: SubTree): SubTree;
      nth  (v: V; n: CARDINAL): SubTree;
      (* return the subtree with n predecessors *)

      iterate (v: V; iter: ChildrenIterator);
      (* iterates locks v.mu, and calls iter.proc for each child of the
         node *)

      calculateSize (v: V);
      (* force a calculation of the bounding box size.  Subtypes may
         override this method to provide different layout policies.  The
         parent will later call translate *)

      translate (v: V; north, west: REAL);
      (* move the sub tree so that its bounding box's north west corner is
         offset R2.T{west, north} from the parent's control point *)

      link (v: V): MG.T;
      (* return the graphical element (could be a line, curve, group, etc)
         that connects the current node and its parent.

         A subtype must supply a "link" method. *)
    END;

TYPE
  LinkerRec = RECORD parentLink, childLink: MG.T END;
  Linker = OBJECT METHODS new (v: V; parent, child: SubTree): LinkerRec END;
(* NewLink.proc returns a pair of MG.T elements controlling a graphical
   link between parent and child. *)

VAR linkerDefault: Linker;
(* If subTree.linker = NIL, linkerDefault is used and returns a MG.Line
   with MG.LineEnds at parent and child.  The visibility of the child
   controls the visibility of the default link. *)

TYPE
  ChildrenIterator = OBJECT
                       v: V;
                     METHODS
                       <* LL = v.mu *>
                       proc (child: SubTree): (* more *) BOOLEAN
                     END;

TYPE
  GenericTree <: GenericTreePublic;
  GenericTreePublic =
    SubTree OBJECT
      children              : RefList.T     := NIL;
      linkEndParent, linkEnd: MG.T := NIL;
      dxChildren, dyChildren: REAL       := 5.0;
      (* the space horizontally between children and vertically between
         parents and children, in pts *)
    END;
(* A tree with an indeterminate number of children stored as a singly
   linked list *)

TYPE
  Forest <: ForestPublic;
  ForestPublic = GenericTree OBJECT METHODS init (v: V): Forest; END;
(* like a normal tree, excpet that the node does not display and no links
   are drawn to the children. *)

PROCEDURE SetRoot(root: SubTree; v: V);
<* LL < v.mu *>
(*| equivalent to:
    LOCK v.mu 
      v.setRoot(root);
      RelayoutAncestors(root);
    END;
    VBT.NewShape(v);
    Animation(v);
*)

PROCEDURE AddChild(node: SubTree; v: V; pred, new: SubTree);
<* LL < v.mu *>
(*| equivalent to:
    LOCK v.mu 
      node.addChild(v, pred, new); 
      RelayoutAncestors(node, v);
    END;
    VBT.NewShape(v);
    Animation(v);
*)

PROCEDURE RemoveChild(node: SubTree; v: V; child: SubTree);
<* LL < v.mu *>
(*| equivalent to:
    LOCK v.mu 
      node.removeChild(v, child); 
      RelayoutAncestors(node, v);
    END;
    VBT.NewShape(v);
    Animation(v);
*)

PROCEDURE Route(ancestor: SubTree; v: V; descendant: SubTree): MG.Group;
<* LL < v.mu *>

PROCEDURE Succ (node: SubTree; v: V; pred: SubTree): SubTree;
<* LL < v.mu *>

PROCEDURE Pred (node: SubTree; v: V; succ: SubTree): SubTree;
<* LL < v.mu *>

PROCEDURE Nth(node: SubTree; v: V; n: CARDINAL): SubTree;
<* LL < v.mu *>

PROCEDURE Iterate (node: SubTree; v: V; iter: ChildrenIterator);
<* LL < v.mu *>

PROCEDURE NumChildren(node: SubTree; v: V): INTEGER;
<* LL < v.mu *>

PROCEDURE Parent(node: SubTree; v: V): SubTree;
<* LL < v.mu *>

PROCEDURE ParentPos(parent: SubTree; v: V): R2.T;
<* LL = v.mu *>

PROCEDURE LinearAnimation (v: V; vector: R2.T; mg: SubTree): BOOLEAN;
<* LL = v.mu *>
(* Creates a linear animation to move "mg" by "vector", adding the
   animation to v's animations.

   If Pos(mg) = R2.Origin, then a special animation is produced which moves
   mg by vector at time 0.0 and makes mg visible at time 1.0.

   Returns TRUE if vector # R2.Origin or mg has been altered in some way
   (child added, transformed, etc).  Children don't have to be relayed out
   if LinearAnimation return FALSE *)

PROCEDURE RelayoutAncestors(node: SubTree; v: V);
<* LL = v.mu *>

END GenericTree.
