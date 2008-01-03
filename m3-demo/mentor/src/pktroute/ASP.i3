(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Thu Aug 12 11:42:39 PDT 1993 by heydon                   *)

INTERFACE ASP;

(* This interface defines procedures for computing all the shortest paths in
   a "Graph.T". *)

IMPORT Graph;

TYPE
  Unweighted <: UnweightedPub;
  UnweightedPub = ROOT BRANDED OBJECT METHODS
    dist(READONLY from, to: CARDINAL): INTEGER;
  END;

  Weighted <: WeightedPub;
  WeightedPub = ROOT BRANDED OBJECT METHODS
    dist(READONLY from, to: CARDINAL): REAL;
  END;

(* The methods have the following specifications:

   If "uw: Unweighted", then "dist(from, to)" returns the length of the
   shortest path from the node with index "from" to the node with index "to"
   if there is such a path, and "-1" otherwise. The length of a path is the
   number of edges along that path, independent of their weights. The "dist"
   method takes O(1) time.

   If "w: Weighted", then "dist(from, to)" returns the weight of the minimum
   weight path from the node with index "from" to the node with index "to" if
   there is such a path, and "IEEE.Infinity" otherwise. The weight of a path
   is the sum of the weights along it. The "dist" method takes O(1) time. *)

PROCEDURE UnweightedFromGraph(g: Graph.Sparse): Unweighted;
(* Returns an "Unweighted" object for determining the shortest path between
   any pair of nodes in "g". *)

PROCEDURE WeightedFromGraph(g: Graph.Sparse): Weighted;
(* Returns a "Weighted" object for determining the minimum-weight path between
   any pair of nodes in "g". *)

END ASP.
