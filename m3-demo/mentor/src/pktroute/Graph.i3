(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 30 09:03:31 PDT 1993 by heydon                   *)

INTERFACE Graph;

TYPE
  T <: TPublic;
  TPublic = ROOT BRANDED OBJECT METHODS
    newNode(): CARDINAL;
    numNodes(): CARDINAL;
    nodeName(id: CARDINAL): TEXT;
    newEdge(id1, id2: CARDINAL; weight: REAL := 1.0);
    neighbors(id: CARDINAL): Iterator;	 (* adjacent nodes *)
  END;

  (* A "Graph.T" is an undirected graph G = (V, E). In a graph with "n" nodes,
     the nodes are assigned identifiers "[0..(n-1)]". The type "Graph.T" is
     defined for sub-typing purposes only; graphs are created by instantiating
     one of the subtype implementations below.

     The call "g.newNode()" allocates a new node in "g" and returns the
     identifier of the new node. Identifiers are allocatated sequentially
     starting from 0.

     The call "g.numNodes()" returns the total number of nodes in "g".

     The call "g.nodeName(id)" returns the name of the node "id" as a text. By
     default, this method simply returns "Fmt.Int(id)", but clients may wish
     to override this method for graphs with regular topologies. It is an
     unchecked run-time error for "id" not to be in the range
     "[0..g.numNodes()-1]".

     The call "g.newEdge(id1, id2, wt)" adds an edge in the graph with weight
     "wt" between the node with identifier "id1" and the node with identifier
     "id2". It is an unchecked run-time error for either "id1" or "id2" not to
     be in the range "[0..g.numNodes()-1]". It is a checked run-time error for
     "weight < 0.0".

     The call "g.neighbors(id)" returns an iterator object that can be used to
     enumerate the neighbors of the node "id" in "g" and the weights of the
     connecting edges. *)

  Iterator = ROOT BRANDED OBJECT METHODS
    next(VAR (*OUT*) id: CARDINAL; VAR (*OUT*) weight: REAL): BOOLEAN;
  END;

  (* If "it" is an iterator produced by the call "g.neighbors(id)", then
     successive calls of the form "it.next(id, weight)" set "id" and "weight"
     to the name of the next neighbor of "id" and the weight of the connecting
     edge, respectively, and return TRUE. If there are no more neighbors,
     "it.next(id, weight)" returns FALSE. In this case, the values of "id" and
     "weight" after the call are undefined. *)

  Sparse <: SparsePublic;
  SparsePublic = T BRANDED OBJECT METHODS
    init(sizeHint: CARDINAL := 10): T
  END;

  (* A "Graph.Sparse" is a graph implemented using an adjacency matrix
     representation. It is intended for sparse graphs, namely those where the
     number of edges is O(V log V), where "V" is the number of nodes in the
     graph.

     "NEW(Graph.Sparse).init(sizeHint)" creates a new sparse graph. The
     "sizeHint" specifies the expected total number of nodes. This is only a
     hint; more nodes than the number specified by the "sizeHint" parameter
     can be added to the graph.

     For a "Graph.Sparse" "g", the calls "g.asp()" and "g.aswp()" take O((E+V)
     V log V) time and O(V^2) space, where "E" and "V" are the number of edges
     and nodes in "g". *)

END Graph.
