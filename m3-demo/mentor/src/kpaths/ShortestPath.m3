(* Copyright 1993 by Digital Equipment Corp. *)

MODULE ShortestPath;

IMPORT Atom, RefSeq, RefList, AtomVertexTbl, AtomEdgeTbl, Edge, Vertex;
IMPORT Math, EdgeSeq, VertexSeq, Rd, Sx;
IMPORT Algorithm, Thread, ZeusPanel, Rsrc;
IMPORT ShortestPathAlgClass, ShortestPathIE;

TYPE
  ZeusT = ShortestPathAlgClass.T BRANDED OBJECT
    OVERRIDES
      run := Run
    END;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN NEW(ZeusT (* , data := ZeusPanel.NewForm("input.fv") *)).init()
  END New;

PROCEDURE Run (alg: ZeusT) RAISES {Thread.Alerted} =
  <* FATAL Rsrc.NotFound *>
  VAR 
    graphRd := Rsrc.Open("graph.sx", ZeusPanel.GetPath());
  BEGIN
    (*
    LOCK VBT.mu DO
      N := FormsVBT.GetInteger(alg.data, "N");
      K := FormsVBT.GetInteger(alg.data, "K");
    END;
    *)
    LoadGraph(alg, graphRd)
  END Run;

VAR 
  EdgeSym := Atom.FromText("Edge");
  VertexSym := Atom.FromText("Vertex");
  QuerySym := Atom.FromText("Find");

PROCEDURE LoadGraph (alg: Algorithm.T; rd: Rd.T) RAISES {Thread.Alerted} =
  VAR t := NEW(T, alg := alg).init(3);
  BEGIN
    TRY
      LOOP
        VAR sx: RefList.T := Sx.Read(rd);
        BEGIN
          IF sx = NIL THEN
            RAISE Sx.ReadError("Because!")
          ELSIF sx.head = EdgeSym THEN
            t.addEdge(sx.tail)
          ELSIF sx.head = VertexSym THEN
            t.addVertex(sx.tail)
          ELSIF sx.head = QuerySym THEN
            EVAL
              t.shortestPath(sx.tail.head, sx.tail.tail.head,
                             NARROW(sx.tail.tail.tail.head, REF INTEGER)^);
            EXIT
          END
        END
      END
    EXCEPT
      Rd.EndOfFile =>            (* SKIP *)
    | Sx.ReadError =>
    END
  END LoadGraph;

REVEAL 
  Vertex.T = BRANDED REF RECORD
    name: Atom.T;
    edges: EdgeSeq.T;
    numPaths: CARDINAL;
    paths: REF ARRAY OF PathRec; 
    qpos: INTEGER;
    x, y: REAL
  END;

TYPE
  PathRec = RECORD
    lastEdge: Edge.T;
    sourceIndex: CARDINAL;
    cost: REAL;
  END;

REVEAL  
  Edge.T = BRANDED REF RECORD
    name: Atom.T;
    cost: REAL;
    source, target: Vertex.T;
  END;

(* For an edge "e", "e.name" and "e.cost" are its name and cost;
   and "e.source" and "e.target" are the vertices it connects.

   For a vertex "v", "v.name" is its name; "v.edges" is the sequence
   (in any order) of edges for which "v" is the source, "v.numPaths"
   is the number of ranks of paths that have been computed ending
   in "v", and for "i < v.numPaths", the "PathRec" "v.paths[i].lastEdge"
   specifies the last edge of the "ith" shortest path ending in
   "v", "v.paths[i].sourceIndex" specifies the rank of the maximal
   proper prefix of this path, and "v.paths[i].cost" specifies the
   cost of this path.  

   The entries in "v.paths[k]" for "k >= v.numPaths" are also relevant
   if their "cost" is less than infinity: they are the candidates
   that have been computed for the path of rank "v.numPaths", in
   order of increasing cost.  These are the so-called candidate
   paths.

   The value of "v.qpos" is the index of "v" in the priority
   queue (described below), or "-1" if "v" is not in the
   queue.

   "NUMBER(v.paths)" is at least equal to "t.ranklimit", where "t"
   is the unique "ShortestPath.T" whose "addVertex" method created
   "v". *)

REVEAL
  T = Public BRANDED OBJECT
    alg: Algorithm.T := NIL;
    ranklimit: CARDINAL;
    source: Vertex.T;
    vertices: AtomVertexTbl.T;
    edges: AtomEdgeTbl.T;
    pqueue: VertexSeq.T;
    pendingEdges: RefSeq.T;
  OVERRIDES
    init := Init;
    addVertex := AddVertex;
    addEdge := AddEdge;
    shortestPath := ShortestPath;
    weight := Weight
  END;

(* If "t" is a "ShortestPath.T", then "t.ranklimit" is its rank limit,
   "q.source" is the last vertex that was the source of any shortest
   path query, or "NIL" if no such queries have been performed,
   "t.vertices" and "t.edges" map vertex and edge names to
   vertices and edges; and "pqueue" is a priority queue represented
   as a Floyd heap.  The vertices in the queue are those with a 
   non-infinite candidate cost (i.e., the priority is 
   "v.paths[v.numPaths].cost", if this is defined).
   
   The sequence "v.pendingEdges" contains the "RefList.T" that
   describes edges that have been created by "addEdge" but not
   yet added to the data structure.  It is necessary because
   edges can be added before their vertices.  *)

PROCEDURE Init(t: T; k: CARDINAL): T =
  BEGIN
    t.ranklimit := k;
    t.source := NIL;
    t.vertices := NEW(AtomVertexTbl.Default).init();
    t.edges := NEW(AtomEdgeTbl.Default).init();
    t.pqueue := NEW(VertexSeq.T).init();
    t.pendingEdges := NEW(RefSeq.T).init();
    RETURN t
  END Init;

PROCEDURE AddVertex (t: T; rl: RefList.T) RAISES {Thread.Alerted} =
  VAR
    v := NEW(Vertex.T, name := rl.head, edges := NEW(EdgeSeq.T).init(),
             numPaths := 0, x := NARROW(rl.tail.head, REF REAL)^,
             y := NARROW(rl.tail.tail.head, REF REAL)^,
             paths := NEW(REF ARRAY OF PathRec, t.ranklimit), qpos := -1);
  BEGIN
    FOR i := FIRST(v.paths^) TO LAST(v.paths^) DO
      v.paths[i].cost := LAST(REAL)
    END;
    EVAL t.vertices.put(rl.head, v);
    ShortestPathIE.NewVertex(t.alg, Atom.ToText(v.name), v.x, v.y)
  END AddVertex;

PROCEDURE AddEdge(t: T; rl: RefList.T) = 
  BEGIN
    t.pendingEdges.addhi(rl)
  END AddEdge;


PROCEDURE ProcessPending(t: T) RAISES {Thread.Alerted} =
  VAR i := 0; BEGIN
    WHILE i # t.pendingEdges.size() DO
      (* The first "i" pending edges refer to absent vertices and
         therefore must be retained. *)
      VAR 
        rl: RefList.T := t.pendingEdges.get(i);
        name: Atom.T := rl.head;
        sourceName: Atom.T := rl.tail.head;
        targetName: Atom.T := rl.tail.tail.head;
        orientation: Atom.T := rl.tail.tail.tail.head;
        source, target: Vertex.T;
        cost: REAL;
        e: Edge.T;
      BEGIN
        IF t.vertices.get(sourceName, source) AND
          t.vertices.get(targetName, target) 
        THEN
          cost :=
              FLOAT(Math.hypot(FLOAT(source.x - target.x, LONGREAL),
                               FLOAT(source.y - target.y, LONGREAL)), REAL);
          (* IF rl.tail.tail.tail # NIL THEN
            cost := NARROW(rl.tail.tail.tail.head, REF REAL)^
          ELSE
            cost := 1.0
          END;
          *)
          e := NEW(Edge.T, name := name, source := source, 
            target := target, cost := cost);
          ShortestPathIE.NewEdge(
            t.alg, Atom.ToText(name), Atom.ToText(sourceName),
            Atom.ToText(targetName), Atom.ToText(orientation));
          source.edges.addhi(e);
          t.source := NIL;
          IF i # t.pendingEdges.size() - 1 THEN
            t.pendingEdges.put(i, t.pendingEdges.remhi())
          ELSE
            EVAL t.pendingEdges.remhi()
          END
        ELSE
          INC(i)
        END
      END
    END
  END ProcessPending;

PROCEDURE ShortestPath (t                     : T;
                        sourceName, targetName: Atom.T;
                        rank                  : CARDINAL): RefList.T
  RAISES {Thread.Alerted} =
  VAR source, target: Vertex.T;
  BEGIN
    ProcessPending(t);
    IF NOT t.vertices.get(sourceName, source)
         OR NOT t.vertices.get(targetName, target) THEN
      RETURN NIL
    END;
    ShortestPathIE.StartFind(
      t.alg, Atom.ToText(sourceName), Atom.ToText(targetName));
    IF source # t.source OR rank >= t.ranklimit THEN
      t.source := source;
      t.ranklimit := MAX(rank + 1, t.ranklimit);
      Reset(t)
    END;
    WHILE target.numPaths <= rank AND t.pqueue.size() # 0 DO
      PromoteCandidate(t)
    END;
    IF target.numPaths > rank THEN
      RETURN ConsPath(t, target, rank)
    ELSE
      RETURN NIL
    END
  END ShortestPath;


PROCEDURE Reset(t: T) =
(* The values "t.source" and/or "t.ranklimit" have changed: set the 
   "numPaths", "paths", and "qpos" fields of each vertex, and the "pqueue" 
   field of "t", to satisfy the invariants listed above.  *)
  VAR iter := t.vertices.iterate(); 
    name: Atom.T;
    vertex: Vertex.T;
  BEGIN
    WHILE iter.next(name, vertex) DO
      IF vertex.paths = NIL OR NUMBER(vertex.paths^) < t.ranklimit THEN
        vertex.paths := NEW(REF ARRAY OF PathRec, t.ranklimit)
      END;
      FOR i := FIRST(vertex.paths^) TO LAST(vertex.paths^) DO
        vertex.paths[i].cost := LAST(REAL)
      END;
      vertex.numPaths := 0;
      vertex.qpos := -1
    END;
    t.source.paths[0].cost := 0.0;
    t.source.paths[0].lastEdge := NIL;
    t.source.paths[0].sourceIndex := 0;
    t.pqueue := NEW(VertexSeq.T).init();
    t.pqueue.addhi(t.source);
    t.source.qpos := 0
  END Reset;

PROCEDURE PromoteCandidate (t: T) RAISES {Thread.Alerted} =
  (* Promote the lowest-cost candidate path to be an answer path.  Requires
     "t.pqueue" not be empty. *)
  VAR v := DeleteMin(t);
  BEGIN
    IF v.paths[v.numPaths].lastEdge # NIL THEN
      ShortestPathIE.Promote(
        t.alg, Atom.ToText(v.paths[v.numPaths].lastEdge.name),
        v.paths[v.numPaths].sourceIndex, v.numPaths)
    END;
    INC(v.numPaths);
    IF v.numPaths < NUMBER(v.paths^)
         AND v.paths[v.numPaths].cost < LAST(REAL) THEN
      SetPriority(t, v)
    END;
    FOR i := 0 TO v.edges.size() - 1 DO ConsiderEdge(t, v.edges.get(i)) END
  END PromoteCandidate;

PROCEDURE ConsiderEdge(t: T; e: Edge.T) RAISES {Thread.Alerted} =
 (* Change "e.target.paths" as
   necessary to reflect the fact that a candidate path ending
   in "e.source" has just been promoted to an answer path. *)
  VAR 
    source := e.source;
    target := e.target;  
    sourceIndex := source.numPaths - 1;
    newCost := e.cost + source.paths[sourceIndex].cost;
  BEGIN
    IF target.numPaths >= t.ranklimit 
       OR newCost >= target.paths[t.ranklimit-1].cost
    THEN
      ShortestPathIE.Consider(t.alg, Atom.ToText(e.name), sourceIndex, -1);
      RETURN
    END;
    VAR k := t.ranklimit-1; BEGIN
      WHILE k > target.numPaths AND target.paths[k-1].cost > newCost DO
        target.paths[k] := target.paths[k - 1];
        k := k - 1
      END;
      WITH pathRec = target.paths[k] DO
        pathRec.lastEdge := e;
        pathRec.sourceIndex := sourceIndex;
        pathRec.cost := newCost
      END;
      ShortestPathIE.Consider(t.alg, Atom.ToText(e.name), sourceIndex, k);
      IF k = target.numPaths THEN
        SetPriority(t, target)
      END
    END
  END ConsiderEdge;
  
PROCEDURE DeleteMin(t: T): Vertex.T =
 (* Delete from "t.queue" and return the lowest cost vertex. 
   Requires "t.pqueue" not be empty. *)
  VAR 
    res: Vertex.T := t.pqueue.get(0); 
    oldLast: Vertex.T := t.pqueue.remhi();
    oldCost := oldLast.paths[oldLast.numPaths].cost;
    i := 0;
    n := t.pqueue.size();
  PROCEDURE Cost(i: INTEGER): REAL =
    BEGIN
      IF i >= n THEN 
        RETURN LAST(REAL) 
      ELSE
        VAR v: Vertex.T := t.pqueue.get(i); BEGIN
          RETURN v.paths[v.numPaths].cost
        END
      END
    END Cost;
  BEGIN
    res.qpos := -1;
    IF n = 0 THEN RETURN res END;
    LOOP
      VAR lCost := Cost(2 * i + 1); rCost := Cost(2 * i + 2); BEGIN
        IF lCost <= rCost AND lCost < oldCost THEN
          VAR ch: Vertex.T := t.pqueue.get(2*i+1); BEGIN
            t.pqueue.put(i, ch);
            ch.qpos := i;
            i := 2 * i + 1
          END
        ELSIF rCost <= lCost AND rCost < oldCost THEN
          VAR ch: Vertex.T := t.pqueue.get(2*i+2); BEGIN
            t.pqueue.put(i, ch);
            ch.qpos := i;
            i := 2 * i + 2
          END
        ELSE
          EXIT
        END
      END
    END;
    t.pqueue.put(i, oldLast);
    oldLast.qpos := i;
    RETURN res
  END DeleteMin;

PROCEDURE SetPriority(t: T; v: Vertex.T) =
(* Insert "v" in "t"'s priority queue if it is not present,
   or adjust its priority if it is. If "v" is already present,
   it is required that its new cost be at most its old cost. *)
  BEGIN
    IF v.qpos < 0 THEN
      t.pqueue.addhi(v);
      v.qpos := t.pqueue.size() - 1
    END;
    VAR 
      i := v.qpos; 
      parent: CARDINAL; 
      vCost := v.paths[v.numPaths].cost;
      parentV: Vertex.T; 
    BEGIN
      WHILE i > 0 DO
        parent := (i - 1) DIV 2;
        parentV := t.pqueue.get(parent);
        IF parentV.paths[parentV.numPaths].cost > vCost THEN
          t.pqueue.put(i, parentV);
          parentV.qpos := i;
          i := parent
        ELSE
          EXIT
        END
      END;
      t.pqueue.put(i, v);
      v.qpos := i
    END
  END SetPriority;
   
PROCEDURE ConsPath (t: T; target: Vertex.T; rank: CARDINAL): RefList.T
  RAISES {Thread.Alerted} =
  (* Return the list representing the path from "t.source" to "target" of
     rank "k".  Requires that this path be present in "target.paths". *)
  VAR
    cur       := target;
    res       := RefList.Cons(cur.name, NIL);
    nextEdge  := cur.paths[rank].lastEdge;
    prevIndex := rank;
    nextIndex := cur.paths[rank].sourceIndex;
  BEGIN
    WHILE nextEdge # NIL DO
      (* "res" is the reverse of the path from "cur" to "target", including
         its vertices. *)
      ShortestPathIE.Traceback(
        t.alg, Atom.ToText(nextEdge.name), nextIndex, prevIndex);
      res := RefList.Cons(nextEdge.name, res);
      cur := nextEdge.source;
      res := RefList.Cons(cur.name, res);
      nextEdge := cur.paths[nextIndex].lastEdge;
      prevIndex := nextIndex;
      nextIndex := cur.paths[nextIndex].sourceIndex
    END;
    RETURN res
  END ConsPath;

PROCEDURE Weight(t: T; edgeName: Atom.T): REAL =
  VAR e: Edge.T; BEGIN
    IF t.edges.get(edgeName, e) THEN
      RETURN e.cost
    ELSE
      RETURN -1.0
    END
  END Weight;
  
BEGIN 
  ZeusPanel.RegisterAlg(New, "Shortest Path", "ShortestPath");
END ShortestPath.
