(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Jan 31 14:53:50 PST 1995 by kalsow   *)
(*      modified on Tue May 24 09:10:33 PDT 1994 by najork   *)
(*      modified on Mon Jul 27 02:50:53 1992 by karsenty     *)

MODULE MFGraph;

(***************************************************************************)
(*   Author:  Solange Karsenty						   *)
(***************************************************************************)

(* $Revision: 1.1.1.1 $ *)

REVEAL
  T = TPublic BRANDED OBJECT
     OVERRIDES
        init      := InitGraph;
        createVertex  := CreateVertex;
        deleteVertex  := DeleteVertex;
        createEdge := CreateEdge;
        deleteEdge := DeleteEdge;
        edgeApply  := EdgeApply;
        vertexApply := VertexApply;
  END;
    
REVEAL
  Vertex = TVertexPublic BRANDED OBJECT
     OVERRIDES
    init := initVertex;
    delete := VertexDelete;
    applyToNeighbors := ApplyToNeighbors;
  END;

REVEAL
  Edge = TEdgePublic BRANDED OBJECT
     OVERRIDES
    init := initEdge;
    otherVertex := OtherVertex;
    delete := EdgeDelete;
  END;

PROCEDURE InitGraph(self: T): T RAISES {}=
  BEGIN
    self.vertices := NIL;
    self.edges := NIL;
    RETURN self;
  END InitGraph;

PROCEDURE initEdge(self: Edge; graph: T): Edge RAISES {}=
  BEGIN
    GraphInsertEdge (graph, self);
    RETURN self;
  END initEdge;

PROCEDURE initVertex(self: Vertex; graph: T): Vertex RAISES {}=
  BEGIN
    self.myGraph := graph;
    GraphInsertVertex (graph, self);
    RETURN self;
  END initVertex;

PROCEDURE EdgeApply(self: T; p: Proc; arg: REFANY): Edge RAISES {}=
  VAR
    e: EdgeList := self.edges;
  BEGIN
    WHILE e # NIL DO
      p (e.edge, arg);
      e := e.next;
    END;
    RETURN NIL;
  END EdgeApply;

PROCEDURE VertexApply(self: T; p: Proc; arg: REFANY): Vertex RAISES {}=
  VAR
    n: VertexList := self.vertices;
  BEGIN
    WHILE n # NIL DO
      p (n.vertex, arg);
      n := n.next;
    END;
    RETURN NIL;
  END VertexApply;

PROCEDURE ApplyToNeighbors(self: Vertex; p: Proc; arg: REFANY): Vertex RAISES {}=
  VAR
    neighbor: Vertex;
    e: EdgeList := self.edges;
  BEGIN
    WHILE e # NIL DO
      neighbor := OtherVertex (e.edge, self);
      p (neighbor, arg);
      e := e.next;
    END;
    RETURN NIL;
  END ApplyToNeighbors;

(******************************** VERTICES ************************************)

PROCEDURE FindVertex (g: T; n: Vertex): VertexList RAISES {}=
  VAR
    p: VertexList := g.vertices;
  BEGIN
    WHILE p # NIL DO
      IF p.vertex = n THEN RETURN p; END;
    END;
    RETURN NIL;
  END FindVertex;

PROCEDURE GraphInsertVertex(g: T; n: Vertex) RAISES {}=
  VAR
    el: VertexList := NEW (VertexList, vertex := n, next := g.vertices, prev := NIL);
  BEGIN
    IF (g.vertices # NIL) THEN g.vertices.prev := el; END;
    g.vertices := el;
  END GraphInsertVertex;

PROCEDURE GraphDeleteVertex(g: T; n: VertexList) RAISES {}=
  BEGIN
    IF n = NIL THEN RETURN END;
    (* vertex to delete is in head *)
    IF g.vertices = n THEN
      g.vertices := n.next;
      n.next.prev := NIL;
      n.next := NIL;
      RETURN;
    END;
    (* standard case *)
    n.prev.next := n.next;
    n.next.prev := n.prev;
  END GraphDeleteVertex;

PROCEDURE CreateVertex(self:T; u: REFANY): Vertex RAISES {}=
  VAR
    n: Vertex;
  BEGIN
    n := NEW (Vertex, data := u, myGraph := self);
    GraphInsertVertex (self, n);
    RETURN n;
  END CreateVertex;

PROCEDURE DeleteEdgesOfVertex (vertex: Vertex) RAISES {}=
  BEGIN
    vertex.edges := NIL; (* GC should do the rest ? *)
  END DeleteEdgesOfVertex;

(* delete the vertex in the graph list, and delete the edges connected to it *)
PROCEDURE DeleteVertex(self:T; n: Vertex) RAISES {NoSuchVertexInGraph}=
  BEGIN
    IF n.myGraph # self THEN RAISE NoSuchVertexInGraph END;
    DeleteEdgesOfVertex (n);    (* start deleting the edges *)
    GraphDeleteVertex (self, FindVertex (self, n));    (* now the vertex itself *)
  END DeleteVertex;

PROCEDURE VertexDelete(self: Vertex) RAISES {}=
  <* FATAL NoSuchVertexInGraph *>
  BEGIN
    DeleteVertex (self.myGraph, self);
  END VertexDelete;

(**************************** EDGES ***************************************)

PROCEDURE GraphInsertEdge(g: T; e: Edge) RAISES {}=
  VAR
    (* prepend the new edge to the graph edge list = el *)
    gel: EdgeList := NEW (EdgeList, edge:= e, next:= g.edges, prev:= NIL);

    fe: EdgeList := NEW (EdgeList, edge := e, prev := NIL);
    te: EdgeList := NEW (EdgeList, edge := e, prev := NIL);
    el: EdgeList;
  BEGIN
    IF (g.edges # NIL) THEN g.edges.prev := gel; END;
    g.edges := gel;

    (* insert in the list of edges of each vertex (from, to) *)
    el := e.from.edges;
    fe.next := el;
    IF (el # NIL) THEN el.prev := fe; END;
    e.from.edges := fe;
    el := e.to.edges;
    te.next := el; 
    IF (el # NIL) THEN el.prev := te; END;
    e.to.edges := te;
  END GraphInsertEdge;

PROCEDURE CreateEdge(self:T; f: Vertex; t: Vertex; u: REFANY): Edge RAISES {MissingVertex}=
  VAR
    e : Edge := NEW (Edge, from := f, to := t, 
                     data := u, myGraph := self);
  BEGIN
    IF f = NIL OR t = NIL THEN RAISE MissingVertex END;
    GraphInsertEdge (self, e);
    RETURN e;
  END CreateEdge;


(* find an edge in the graph.edges *)
PROCEDURE FindEdge (elist: EdgeList; n: Edge): EdgeList RAISES {}=
  VAR
    p: EdgeList := elist;
  BEGIN
    WHILE p # NIL DO
      IF p.edge = n THEN RETURN p; END;
      p := p.next;
    END;
    RETURN NIL;
  END FindEdge;

PROCEDURE DeleteEdge(self:T; edge: Edge) RAISES {NoSuchEdgeInGraph}=
  VAR
    e: EdgeList;
    vertex: Vertex;
  BEGIN
      IF (edge.myGraph # self) THEN RAISE NoSuchEdgeInGraph END;
      e := FindEdge (self.edges, edge);
      IF e.prev = NIL THEN
      	self.edges := e.next;
      	self.edges.prev := NIL;
      ELSE
        e.prev.next := e.next;
      	e.next.prev := e.prev;
      END;

      (* delete in the list of edge.edge.{from,to} *)
      vertex := edge.from;
      e := FindEdge (vertex.edges, edge);
      IF e.prev = NIL THEN
      	vertex.edges := e.next;
      	IF (vertex.edges # NIL) THEN vertex.edges.prev := NIL; END;
      ELSE
        e.prev.next := e.next;
      	e.next.prev := e.prev;
      END;
      vertex := edge.to;
      e := FindEdge (vertex.edges, edge);
      IF e.prev = NIL THEN
      	vertex.edges := e.next;
      	IF (vertex.edges # NIL) THEN vertex.edges.prev := NIL; END;
      ELSE
        e.prev.next := e.next;
      	e.next.prev := e.prev;
      END;
  END DeleteEdge;

PROCEDURE EdgeDelete(self: Edge) RAISES {}=
  <* FATAL NoSuchEdgeInGraph *>
  BEGIN
    DeleteEdge (self.myGraph, self);
  END EdgeDelete;

PROCEDURE OtherVertex(self: Edge; n: Vertex): Vertex RAISES {}=
  BEGIN
    IF (self.from = n) THEN
      RETURN self.to;
    ELSE
      IF (self.to = n) THEN RETURN self.from;
      ELSE RETURN NIL; END;
    END;
  END OtherVertex;


BEGIN
END MFGraph.

