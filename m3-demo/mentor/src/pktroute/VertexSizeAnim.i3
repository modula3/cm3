(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sat Aug 14 15:25:59 PDT 1993 by heydon                   *)
<* PRAGMA LL *>

INTERFACE VertexSizeAnim;

IMPORT GraphVBT, R2;

TYPE
  Animation = OBJECT METHODS
    size(t: REAL): R2.T
  END;

(* Clients should not instantiate an "Animation" directly; instead, they
   should instantiate a subtype that overrides the "size" method.

   The "size" method returns the width and height of a vertex as a function of
   the time parameter "t". *)

PROCEDURE Register(v: GraphVBT.Vertex; anim: Animation); <* LL = v.graph.mu *>
(* Register the animation "anim" on vertex "v". On the next call to
   "v.graph.animate(t0, t1)", a series of calls "v.setSize(anim.size(t))" is
   made as an internal time parameter "t" varies between "t0" and "t1". *) 

PROCEDURE Linear(v: GraphVBT.Vertex; newW, newH: REAL); <* LL = 0 *>
(* Register an animation that causes the size of vertex "v" to change linearly
   from its current size to the new size "(newW, newH)" on the next call to
   "v.graph.animate(0.0, 1.0)". *)

END VertexSizeAnim.
