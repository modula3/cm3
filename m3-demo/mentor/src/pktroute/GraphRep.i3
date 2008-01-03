(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 30 09:03:31 PDT 1993 by heydon                   *)

INTERFACE GraphRep;

(* The "GraphRep" interface reveals the representation of the default
   implementations declared in the "Graph" interface. *)

IMPORT Graph;

REVEAL
  Graph.T <: TRep;

TYPE
  TRep = Graph.TPublic BRANDED OBJECT
    nodeCnt: CARDINAL
  END;

(* The allocated nodes are "[0, nodeCnt)", so "nodeCnt" is the total number
   of allocated nodes. *)

TYPE
  NodeList = REF RECORD
    node: CARDINAL;
    weight: REAL;
    next: NodeList := NIL
  END;

REVEAL
  Graph.Sparse <: SparseRep;

TYPE
  SparseRep = Graph.SparsePublic BRANDED OBJECT
    adj: REF ARRAY OF NodeList;
  END;

(* A "Graph.Sparse" is represented by adjacency lists. For any node "i" in the
   sparse graph "g", "g.adj[i]" is a list of the adjacent nodes (and the
   weight of the corresponding edges) to "i". *)

END GraphRep.    
