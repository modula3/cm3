(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 30 09:03:31 PDT 1993 by heydon                   *)

MODULE Graph EXPORTS Graph, GraphRep;

IMPORT Fmt;

REVEAL
  T = TRep BRANDED OBJECT OVERRIDES
    nodeName := NodeName
  END;

PROCEDURE NodeName(<*UNUSED*> g: T; id: CARDINAL): TEXT =
  BEGIN RETURN Fmt.Int(id) END NodeName;

REVEAL
  Sparse = SparseRep BRANDED OBJECT OVERRIDES
    init      := Init;
    newNode   := NewNode;
    numNodes  := NumNodes;
    newEdge   := NewEdge;
    neighbors := Neighbors;
  END;

PROCEDURE Init(g: Sparse; sizeHint: CARDINAL := 10): T =
  BEGIN
    g.adj := NEW(REF ARRAY OF NodeList, sizeHint);
    g.nodeCnt := 0;
    RETURN g
  END Init;

PROCEDURE NewNode(g: Sparse): CARDINAL =
  VAR next := g.nodeCnt; BEGIN
    IF next > LAST(g.adj^) THEN
      VAR new := NEW(REF ARRAY OF NodeList, g.nodeCnt * 2); BEGIN
        SUBARRAY(new^, 0, g.nodeCnt) := g.adj^;
        g.adj := new
      END
    END;
    g.adj[next] := NIL;
    INC(g.nodeCnt);
    RETURN next
  END NewNode;

PROCEDURE NumNodes(g: Sparse): CARDINAL =
  BEGIN RETURN g.nodeCnt END NumNodes;

PROCEDURE NewEdge(g: Sparse; id1, id2: CARDINAL; weight: REAL := 1.0) =
  PROCEDURE AddEdge(adj: REF ARRAY OF NodeList; from, to: CARDINAL) =
    VAR new := NEW(NodeList, node := to, weight := weight); BEGIN
      new.next := adj[from];
      adj[from] := new
    END AddEdge;
  BEGIN
    <* ASSERT weight >= 0.0 *>
    AddEdge(g.adj, id1, id2);
    AddEdge(g.adj, id2, id1)
  END NewEdge;

TYPE
  SparseIt = Iterator BRANDED OBJECT
    curr: NodeList
  OVERRIDES
    next := SparseItNext
  END;

PROCEDURE Neighbors(g: Sparse; id: CARDINAL): Iterator =
  BEGIN
    RETURN NEW(SparseIt, curr := g.adj[id])
  END Neighbors;

PROCEDURE SparseItNext(
    it: SparseIt;
    VAR (*OUT*) id: CARDINAL;
    VAR (*OUT*) weight: REAL):
    BOOLEAN =
  BEGIN
    WITH curr = it.curr DO
      IF curr = NIL THEN RETURN FALSE END;
      id := curr.node;
      weight := curr.weight;
      curr := curr.next
    END;
    RETURN TRUE
  END SparseItNext;

BEGIN
END Graph.
