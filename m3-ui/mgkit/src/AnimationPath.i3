(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jun 23 11:52:09 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:17:10 PDT 1992 by mhb *)

INTERFACE AnimationPath;

IMPORT GraphVBT, RefList, R2;

(* This interface contains a collection of animation paths for
   moving a "GraphVBT.Vertex". *)

TYPE
  T = GraphVBT.AnimationPath;

  (* "StraightPath" is a linear path from "p0" to "p1". *)
  StraightPath <: StraightPathPublic;
  StraightPathPublic =
    T OBJECT METHODS init (p0, p1: R2.T): T END;

  (* "BezierPath" is a path along the Bezier curve from "p0" to
     "p3", with control points "p1" and "p2". *)
  BezierPath <: BezierPathPublic;
  BezierPathPublic =
    T OBJECT METHODS init (p0, p1, p2, p3: R2.T): T END;


  (* "EdgePath" is the path traced out by edge "e".  If the
     vertices that define "e" (the two endpoints, with or without
     two addition control points) are also moving, the path
     traced by "e" adjusts dynamically. *)
  EdgePath <: EdgePathPublic;
  EdgePathPublic =
    T OBJECT METHODS init (e: GraphVBT.Edge): T END;

(* "MultipleEdgePath" is the path traced out by the sequence of edges
    in "edges".  If the any of the vertices (the two endpoints, with or without
    two addition control points) that define any of the edges
    are also moving, the path traced adjusts dynamically.  The time spent
    moving along each edge is roughly proportional to the length of the 
    edge.  The edges can be disjoint.
*)
  MultipleEdgePath <: MultipleEdgePathPublic;
  MultipleEdgePathPublic =
    T OBJECT METHODS init (edges: RefList.T (* of GraphVBT.Edge *)): T END;
  
END AnimationPath.


