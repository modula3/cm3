(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jun 23 12:07:25 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:17:10 PDT 1992 by mhb *)

INTERFACE GraphAnim;

IMPORT GraphVBT, RefList;

<* PRAGMA LL *>

(* This interface contains a collection of procedures for animating motions on
   a collection of "GraphVBT.Vertex".   Even if a vertex is mentioned multiple
   times in the list, it is only moved once.  *)

PROCEDURE Rotate (center     : GraphVBT.Vertex;
                  angle : REAL;
                  vertices   : RefList.T (* of GraphVBT.Vertex *));
<* LL.sup =  RefList.First(SELF.vertices).graph.mu *>
(* Rotate "vertices" around "center" by "angle" degrees.  Positive
   degrees move in the clockwise direction when x increases to the right
   and y increases downward.  Reversing the polarity of x or y reverses
   the direction of rotation.  Reversing the polarity of both x and y,
   leaves the direction of rotation unchanged. *)

PROCEDURE MoveAlongEdges (edges   : RefList.T (* of GraphVBT.Edge *);
                          vertices: RefList.T (* of GraphVBT.Vertex *));
<* LL.sup =  RefList.First(SELF.vertices).graph.mu *>
(* Move "vertices" along "edges".  The time spent moving along each edge is
   roughly proportionate to the edge's length. *)

END GraphAnim.
