(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Tue Jan 31 15:39:06 PST 1995 by kalsow   *)
(*      modified on Tue Jan 24 14:16:18 PST 1995 by mhb      *)
(*      modified on Wed May  4 11:09:17 PDT 1994 by najork   *)
(*      modified on Wed Aug  5 12:48:55 PDT 1992 by karsenty *)
(*      modified on Tue Jul 21 06:24:57 1992 by steven       *)
<* PRAGMA LL *>
MODULE MFAlgs;

IMPORT Algorithm, MaxflowAlgClass, MaxflowIE, FloatMode, FormsVBT, Thread,
       VBT, ZeusPanel, MFGraph, RefList, ZeusCodeView, R2, Pickle,
       Text, FileWr, Wr, Rd, Scan, Fmt, Lex, Rsrc, OSError;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

(***********************************************************
  On the data structure:
  T contains a MFGraph.T that is the graph on which we will
  apply the algorithm. This graph has a "twin" graphical
  graph which is a GraphVBT.T in some of the views.
  The relation between these two graphs is maintained on the
  view side. A graphical vertex/edge maintains a pointer
  to its twin MFGraph.Vertex/MFGraph.Edge
************************************************************)
 

TYPE 
  T = MaxflowAlgClass.T BRANDED OBJECT 
      graph : MFGraph.T;
      source, sink : MFVertex;

    OVERRIDES 

      <* LL = 0 *>
      run := Run;

      <* LL = VBT.mu *>
(*      reactivity := Reactivity; *)
      feAddVertex := AddVertex;
      feAddEdge := AddEdge;

    END;

VAR VX : INTEGER;

(************************************************************)
(*                     OVERRIDES                            *)
(************************************************************)
(*
PROCEDURE Reactivity (alg: T; enable: BOOLEAN) =
  <* LL = VBT.mu *>
  BEGIN
    IF enable THEN
      FormsVBT.MakeActive (alg.data, "DoEdge");
    ELSE
      FormsVBT.MakePassive (alg.data, "DoEdge");
    END;
  END Reactivity;
*)

(* edges and vertices use the data field as a pointer
   to their GraphVBT twin *)

PROCEDURE AddVertex (alg: T; p: R2.T) =
  <* LL = VBT.mu *>
  <* FATAL Thread.Alerted *>
  VAR mfv := NEW (MFVertex, data := NIL).init(alg.graph);
      n : TEXT;
  BEGIN
    n := FormsVBT.GetText(alg.data, "vertexname") 
             & Fmt.Int (VX);
    INC (VX);
    MaxflowIE.AddVBTVertex (alg, mfv, p, n);
  END AddVertex;

PROCEDURE AddEdge (alg: T; v0, v1: MFGraph.Vertex) =
  <* LL = VBT.mu *>
  <* FATAL Thread.Alerted, FloatMode.Trap *>
  VAR mfed : MFEdge;
  BEGIN
    IF v0 # NIL AND v1 # NIL THEN
      mfed := NEW (MFEdge, from := v0, to := v1, data := NIL).init(alg.graph);
      TRY
        mfed.capacity :=  Scan.Real (FormsVBT.GetText(alg.data, "capacity"));
      EXCEPT
      | Lex.Error =>
        mfed.capacity := 3.0;
        FormsVBT.PutText(alg.data, "capacity", "3.0")
      END;
      mfed.flow := 0.0;
      (* the last param says that it's an UPDATE event *)
      MaxflowIE.AddVBTEdge (alg, mfed);
    END;
  END AddEdge;

CONST
     end = SET OF CHAR {'.'};
     any = SET OF CHAR {'0'..'9','A'..'Z','a'..'z'};
     arrow = SET OF CHAR {'-', '>', ' '};
     space = SET OF CHAR {' '};
     WSize = 1000.0;

(* dirty quick hack to input beautified graphs: *)
(* we assume the world coordinate is (0, 0, 1000, 1000) *)
(* with origin at the top left corner *)
(* example of formatted graph (one space for delimiter):
node_14 199 148
node_16 237 229
node_14 -> node_16 1.7
*)

PROCEDURE Open (filename: TEXT): Rd.T =
  <* FATAL Rsrc.NotFound *>
  BEGIN
    RETURN Rsrc.Open(filename, ZeusPanel.GetPath())
  END Open;


PROCEDURE ReadAsciiGraph (filename: TEXT) : MFGraph.T RAISES {Thread.Alerted} =
 <* FATAL FloatMode.Trap, Lex.Error *>
 VAR rd : Rd.T;
    g : MFGraph.T := NEW(MFGraph.T).init();
    t, next, name, to : TEXT;
    v0 : MFVertex;
    x : REAL;
  BEGIN
    TRY
      rd := Open (filename);

      t := Lex.Scan (rd, end);
      WHILE Text.Equal (t, "") DO
        name := Lex.Scan (rd, any);
        next := Lex.Scan (rd, arrow);

        IF Text.Equal (next, " ") THEN (* read a position (x, y) *)
          v0 := NARROW (NEW (MFVertex, data := NIL).init(g), MFVertex);
          v0.label := name;
          EVAL Lex.Scan (rd, space);
          x := FLOAT (Lex.Int (rd)) / WSize;
          EVAL Lex.Scan (rd, space);
          (* the origin for GraphVBT is the BOTTOM left corner *)
          v0.pos := R2.T{x, 1.0 - FLOAT (Lex.Int (rd)) / WSize};
        ELSE                          (* read an edge (name, to)*)
          to := Lex.Scan (rd, any);
          EVAL Lex.Scan (rd, space);
          x := Lex.Real (rd); (* the capacity *)
          EVAL NEW (MFEdge, capacity := x,
                    from := FindVertex (g, name), 
                    to := FindVertex (g, to), data := NIL).init(g);
        END;
        EVAL Rd.GetLine (rd);
        t := Lex.Scan (rd, end);
      END;
    EXCEPT
    | Rd.Failure, Rd.EndOfFile => RETURN g;
    END;    

    RETURN g;
  END ReadAsciiGraph;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  <* FATAL Rd.EndOfFile, Pickle.Error *>
  VAR
    v0: MFVertex;
    rd : Rd.T;
    g : MFGraph.T;
    vlist : MFGraph.VertexList;
    mfv : MFVertex;
    FinAnim : BOOLEAN;

BEGIN
    (* try opening the file, if fails then create a source/sink *)

    TRY
      WITH f = FormsVBT.GetText(alg.data, "asciifilename"), 
           l = Text.Length(f) 
      DO
        FinAnim := Text.Equal ("data.fin", 
                               Text.Sub (f, MAX(0,l-8), 8));
        g := ReadAsciiGraph(f)
      END;

      IF (g.vertices = NIL) THEN
        LOCK VBT.mu DO 
          rd := Open (FormsVBT.GetText(alg.data, "filename"));
        END;
        g := Pickle.Read (rd);
      END;

      alg.graph := g;
      InitFlow(alg.graph);

      (* find source and sink *)
      vlist := g.vertices;
      WHILE (vlist # NIL) DO
        mfv := NARROW(vlist.vertex, MFVertex);
        IF (Text.Equal(mfv.label, "Source")) THEN
          alg.source := vlist.vertex;
        ELSIF (Text.Equal(mfv.label, "Sink")) THEN
          alg.sink := vlist.vertex;
        END;
        vlist := vlist.next;
      END;

      MaxflowIE.Setup(alg, alg.graph, alg.source, alg.sink);

    EXCEPT
    | Rd.Failure =>
        FormsVBT.PutText(alg.data, "filename", "");
        alg.graph := NEW(MFGraph.T).init();

        v0 := NARROW(NEW(MFVertex, data := NIL).init(alg.graph), MFVertex);
        v0.pos := R2.T{0.05, 0.5};
        v0.label := "Source";
        alg.source := v0;

        v0 := NARROW(NEW(MFVertex, data := NIL).init(alg.graph), MFVertex);
        v0.pos := R2.T{0.95, 0.5};
        v0.label := "Sink";
        alg.sink := v0;

        MaxflowIE.Setup(alg, alg.graph, alg.source, alg.sink);
    END;

    TRY
      FindFlow(alg);
      IF FinAnim THEN
        MaxflowIE.FinalResult (alg, TRUE);
      ELSE
        MaxflowIE.FinalResult (alg, FALSE);
      END;
    EXCEPT
    | Thread.Alerted =>
    END;
  END Run;

(************************************************************)
(*           Attached Procs in the Zeus panel               *)
(************************************************************)

PROCEDURE DoEdge (<* UNUSED *>f: FormsVBT.T; <* UNUSED *> event: TEXT; 
                  cl:REFANY; <* UNUSED *> ts: VBT.TimeStamp) =
  VAR alg : T;
      v0, v1: MFGraph.Vertex;
  BEGIN
    alg := NARROW (cl, T);

    v0 := FindVertex (alg.graph, FormsVBT.GetText(alg.data, "vertex0"));
    v1:= FindVertex (alg.graph, FormsVBT.GetText(alg.data, "vertex1"));

    AddEdge (alg, v0, v1);

  END DoEdge;

PROCEDURE SaveGraph( <*UNUSED*> f     : FormsVBT.T;
                     <*UNUSED*> event : TEXT; 
                                cl    :REFANY;
                     <*UNUSED*> ts    : VBT.TimeStamp) =
  VAR alg : T;
      wr : Wr.T;
  BEGIN
    alg := NARROW (cl, T);
    TRY
      wr := FileWr.Open (FormsVBT.GetText(alg.data, "filename"));
      Pickle.Write (wr, alg.graph);
    EXCEPT
      | OSError.E, Wr.Failure, Pickle.Error, Thread.Alerted =>
        FormsVBT.PutText(alg.data, "filename", "Cannot write!");
    END;
  END SaveGraph;

(************************************************************)
(*                      UTILS                               *)
(************************************************************)

PROCEDURE FindVertex(g: MFGraph.T; label: TEXT): MFGraph.Vertex =
  VAR vl : MFGraph.VertexList := g.vertices;
  BEGIN
    WHILE (vl # NIL) DO
      IF Text.Equal (NARROW(vl.vertex, MFVertex).label, label)
         THEN RETURN vl.vertex; END;
      vl := vl.next;
    END;
    RETURN NIL;
  END FindVertex;

PROCEDURE InitFlow (g: MFGraph.T) =
  VAR
    e : MFGraph.EdgeList;
    ed: MFEdge;

  BEGIN
    e := g.edges;
    WHILE e # NIL DO
      ed := e.edge;
      ed.flow := 0.0;
      e := e.next;
    END;
  END InitFlow;

(************************************************************)
(*            Print methods for the event view              *)
(************************************************************)
PROCEDURE NullText (<* UNUSED*> g: MFGraph.T) : TEXT =
  BEGIN RETURN "--"; END NullText;

PROCEDURE PrintText(t: TEXT) : TEXT = BEGIN RETURN t END PrintText;

(************************************************************)
(*                 algorithm specific                       *)
(************************************************************)
PROCEDURE NewAlg (): Algorithm.T =
  VAR
    fv := ZeusPanel.NewForm("maxflowinput.fv");
    alg : T;
  BEGIN
    alg := NEW(T, data := fv, 
               varRsrc := "maxflowdata.fv",
               codeViews := RefList.List1 (
                                RefList.List2 ("Code View", "maxflowcode.m3")),
               graph := NIL).init();
    (* the last arg is a parameter that will be passed to the attach proc *)
    FormsVBT.AttachProc (fv, "DoEdge", DoEdge, alg);
    FormsVBT.AttachProc (fv, "Save", SaveGraph, alg);
    RETURN alg;
  END NewAlg;

PROCEDURE FindFlow (alg: T) RAISES {Thread.Alerted} =

  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN 
      ZeusCodeView.Event(alg, line); 
    END At;

  VAR
    p : RefList.T;
    number: CARDINAL;
    c, maxC: REAL;
    ed: MFEdge;
    curvert: MFVertex;
    tflow: REAL <* TRACE alg.varView.setReal *>;
    tcapacity: REAL <* TRACE alg.varView.setReal *>;
    

  BEGIN
    
           ZeusCodeView.Event(alg, procedureName := "Maxflow");
           p := FindPath(alg, c, maxC);
           WHILE p # NIL DO
At (1); 
At (2);      curvert:= alg.source;
             MaxflowIE.HighlightPath (alg, p, maxC);
At (3);      number := 0;
At (4);      WHILE p # NIL DO
At (5);        MaxflowIE.ShowEdge (alg, number, 0);
               ed := p.head;
               p := p.tail;
               tcapacity := ed.capacity;
               tflow:= ed.flow;

At (6);        IF (ed.from = curvert) THEN
                 MaxflowIE.ShowEdge (alg, number, 1);
                 ed.flow := ed.flow + c;
                 tflow := ed.flow;
At (7);          MaxflowIE.IncFlow(alg, ed, ed.flow, number, maxC, ed.capacity);
At (8);          curvert:= ed.to;
At (9);        ELSE
                 MaxflowIE.ShowEdge(alg, number, 2);
                 ed.flow := ed.flow - c;
                 tflow := ed.flow;
At (10);         MaxflowIE.DecFlow(alg, ed, c + ed.flow, ed.flow, number, maxC, ed.capacity);
At (11);         curvert:= ed.from;
               END;
               tflow:= 0.0;
               tcapacity:= 0.0;
               INC(number);
             END;
             MaxflowIE.ShowEdge (alg, number, 3);
             MaxflowIE.RemoveHighlight (alg, alg.sink);
             p := FindPath(alg, c, maxC);
           END;

  END FindFlow;

(* Returns a list of edges that form a path from source to sink,
  with nonzero residual capacity.
  cap is the minimum residual capacity of all edges
  in the path *)
PROCEDURE FindPath (alg: T; VAR capChange: REAL; VAR maxCap: REAL): RefList.T =

  (* Remove the last element of l from l and return it. *)
  PROCEDURE Dequeue (VAR l : RefList.T) : REFANY =
    VAR
      r : REFANY;
    BEGIN
      <* ASSERT l # NIL *>
      IF l.tail = NIL THEN
        r := l.head;
        l := NIL;
        RETURN r;
      ELSE
        RETURN Dequeue (l.tail);
      END;
    END Dequeue;

  VAR
    queue                                     : RefList.T           := NIL;
    path                                      : RefList.T           := NIL;
    v                                         : MFGraph.Vertex;
    neighbours                                : MFGraph.EdgeList;
    neighbour, current_edge                   : MFEdge;
    current_vertex, neighbourto, neighbourfrom: MFVertex;
    residual_capacity                         : REAL;
    found_the_sink                            : BOOLEAN          := FALSE;
    vlist                                     : MFGraph.VertexList;
    vert                                      : MFVertex;
  BEGIN
    (* do a bfs starting at the source *)

    vlist := alg.graph.vertices;
    WHILE vlist # NIL DO
      vert := vlist.vertex;
      vlist := vlist.next;
      vert.marked := FALSE;
    END;

    queue := RefList.Cons (alg.source, queue);
    WHILE queue # NIL AND NOT found_the_sink DO

      (* pull a vertex v off the queue *)
      v := Dequeue (queue);
      neighbours := v.edges;

      (* go through the neighbors of v and see if their residual capacity
         is nonzero *)
      WHILE (neighbours # NIL) AND NOT found_the_sink DO
        neighbour := neighbours.edge;
        neighbours := neighbours.next;
        neighbourto := NARROW(neighbour.to, MFVertex);
        neighbourfrom := NARROW(neighbour.from, MFVertex);

        (* first check if edge is a forward edge *)
        IF (neighbourfrom = v) AND (NOT neighbourto.marked)
             AND (neighbour.flow < neighbour.capacity) THEN
          neighbourto.marked := TRUE;
          neighbourto.wherefrom := neighbour;
          (* as soon as find sink, can stop the bfs *)
          IF neighbourto = alg.sink THEN found_the_sink := TRUE; END;
          queue := RefList.Cons (neighbourto, queue);
        END;

        (* then check if edge is back edge *)
        IF (neighbourto = v) AND (NOT neighbourfrom.marked)
             AND (neighbour.flow > 0.0) THEN
          neighbourfrom.marked := TRUE;
          neighbourfrom.wherefrom := neighbour;
          queue := RefList.Cons (neighbourfrom, queue);
        END;
      END;
    END;

    (* once find sink, reconstruct the path *)
    maxCap := 0.0;
    IF found_the_sink THEN
      current_edge := alg.sink.wherefrom;
      capChange := current_edge.capacity - current_edge.flow;
      current_vertex := alg.sink;
      WHILE (alg.source # current_vertex) DO
        path := RefList.Cons (current_edge, path);
        IF current_edge.from = current_vertex THEN
          (* The edge is a back edge, reduce the flow *)
          residual_capacity := current_edge.flow;
          current_vertex := current_edge.to;
        ELSE
          (* The edge is a forward edge, increase the flow *)
          residual_capacity := current_edge.capacity - current_edge.flow;
          current_vertex := current_edge.from;
        END;
        IF current_edge.capacity > maxCap THEN maxCap := current_edge.capacity; END;
        IF residual_capacity < capChange THEN capChange := residual_capacity; END;
        current_edge := current_vertex.wherefrom;
      END;
    END;

    RETURN path;
  END FindPath;


(************************************************************)
(*        Pkl stuff to avoid outputing the View part        *)
(************************************************************)
(*
PROCEDURE WriteREF(<* UNUSED *> r: REFANY) : TEXT =
  BEGIN
    RETURN "R";
  END WriteREF;
*)

(*
PROCEDURE ReadREF (<* UNUSED *> READONLY byte: ARRAY OF CHAR) : REFANY =
  BEGIN
    RETURN NIL;
  END ReadREF;
*)


(************************************************************)

BEGIN
  VX := 0;
  (* we dont want to write the data field from the MFGraph.T *)
(****
  Pkl.RegisterBytesProcs (TYPECODE(GraphVBT.Edge), WriteREF, ReadREF);
  Pkl.RegisterBytesProcs (TYPECODE(GraphVBT.Vertex), WriteREF, ReadREF);
****)

  ZeusPanel.RegisterAlg(NewAlg, "Edmonds-Karp", "Maxflow");
END MFAlgs.


