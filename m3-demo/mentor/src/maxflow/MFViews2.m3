(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Tue Jan 31 14:53:51 PST 1995 by kalsow   *)
(*      modified on Wed May  4 11:02:56 PDT 1994 by najork   *)
(*      modified on Wed Jan  6 15:38:40 PST 1993 by steveg   *)
(*      modified on Wed Aug  5 13:19:51 PDT 1992 by karsenty *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb          *)
<* PRAGMA LL *>

MODULE MFViews2;

IMPORT Filter, GraphVBT, MFAlgs, MFGraph, MaxflowIE, MaxflowViewClass, R2, 
       Rect, RefList, Text, Thread, VBT, View, ZeusPanel;

FROM MFViews IMPORT Color;
FROM MFViews IMPORT BlueColor;

CONST EdgeWidth = 0.04;

TYPE
  T = MaxflowViewClass.T BRANDED OBJECT
	gvbt : GraphVBT.T;
        source, sink: GraphVBT.Vertex;
	nVertices: INTEGER;
	nEdges: INTEGER;

      OVERRIDES
        <* LL=0 *>
        startrun := Startrun;
        oeSetup := Setup;
        oeHighlightPath := HighlightPath;
        oeDecFlow := DecFlow;
        oeIncFlow := IncFlow;
        oeFinalResult := FinalResult;
        oeRemoveHighlight := RemoveHighlight;

        <* LL=VBT.mu *>
        ueAddVBTVertex := AddVBTVertex;
        ueAddVBTEdge := AddVBTEdge;

        mouse := MouseProcess;

      END;

  Vertex = GraphVBT.Vertex BRANDED OBJECT
        myGraphVertex: MFAlgs.MFVertex;
  END;

  Edge = GraphVBT.Edge BRANDED OBJECT
        myGraphEdge: MFAlgs.MFEdge;
  END;

(* find the twins *)
PROCEDURE FindVBTVertex (g: GraphVBT.T; v: MFGraph.Vertex) : Vertex =
  VAR l : RefList.T := g.vertices;
      vbt : Vertex;
  BEGIN
    WHILE l # NIL DO
      vbt := l.head;
      l := l.tail;
      IF (vbt.myGraphVertex = v) THEN RETURN vbt; END;
    END;
    RETURN NIL;
  END FindVBTVertex;

PROCEDURE FindVBTEdge (g: GraphVBT.T; e: MFGraph.Edge) : Edge =
  VAR l : RefList.T := g.vertices;
      le : RefList.T;
      evbt : Edge;
      vvbt : Vertex;
  BEGIN
    (********* tout ca pour recuperer les edges ********)
    WHILE l # NIL DO
      vvbt := l.head;
      l := l.tail;
      le := vvbt.edges;
      WHILE le # NIL DO
        evbt := le.head;
        le := le.tail;
        IF (evbt.myGraphEdge = e) THEN RETURN evbt; END;
      END;
    END;

    RETURN NIL;
  END FindVBTEdge;

(* upon a click create a new vertex *)
PROCEDURE MouseProcess (self: T; READONLY cd: VBT.MouseRec) =
  <*FATAL Thread.Alerted *>
  VAR r : R2.T; (* the world coordinates *)
      rect: Rect.T;
  BEGIN
    IF (cd.clickType = VBT.ClickType.FirstDown) THEN
      CASE cd.whatChanged OF
      | VBT.Modifier.MouseL =>
        rect := VBT.Domain (self);
        r := R2.T {FLOAT(cd.cp.pt.h) / FLOAT(rect.east-rect.west), 
                   1.0 - (FLOAT(cd.cp.pt.v) / FLOAT(rect.south-rect.north))};
        MaxflowIE.AddVertex (self, r);
      ELSE
      END;
    END;
  END MouseProcess;

PROCEDURE AddVBTVertex (self: T; v: MFAlgs.MFVertex; pos: R2.T; name: TEXT) =
  <* LL = VBT.mu *>
  VAR vbt: GraphVBT.Vertex;
  BEGIN
    CreateVBTVertex (v, self.gvbt);

    vbt := FindVBTVertex (self.gvbt, v);
    vbt.move (pos, FALSE);
    vbt.setLabel (name);

    v.label := vbt.label;
    v.pos := pos;
    self.gvbt.redisplay();
  END AddVBTVertex;

PROCEDURE AddVBTEdge (self: T; e: MFAlgs.MFEdge) =
  <* LL = VBT.mu *>
  BEGIN
    CreateVBTEdge (e, self.gvbt);
    self.gvbt.redisplay();
  END AddVBTEdge;

PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, NEW(GraphVBT.T).init());
    (* call the superclass startrun in ZeusClass.T *)
    MaxflowViewClass.T.startrun(view); 
  END Startrun;

PROCEDURE CreateVBTVertex (obj, arg: REFANY) =
  VAR v : MFGraph.Vertex;
      vbt : Vertex;
      g := NARROW (arg, GraphVBT.T);
      stdsize : R2.T;
  BEGIN
    stdsize := R2.T{0.08,0.08};   
    v := NARROW (obj, MFGraph.Vertex);
    vbt := NEW (Vertex, graph := g, 
                pos := R2.T {0.5, 0.5},
	        size := stdsize,
                shape := GraphVBT.VertexShape.Ellipse).init();

    vbt.myGraphVertex := v;
    vbt.setFont (g.font(family := "Helvetica", weight := "bold",
                       slant := GraphVBT.Slant.Roman, size := 0.05));
    vbt.setFontColor (Color("Black"));
    vbt.setColor(Color ("gray"));

    EVAL  NEW (GraphVBT.VertexHighlight, vertex := vbt,
                color := Color ("gray"),
                border := R2.T{0.01, 0.01}).init();	

  END CreateVBTVertex;

PROCEDURE CreateVBTEdge (obj: REFANY; arg: REFANY) =
  VAR e : MFGraph.Edge;
      ebt : Edge;
      arrow := ARRAY [0 .. 1] OF BOOLEAN{FALSE, TRUE};
      g := NARROW (arg, GraphVBT.T);
  BEGIN
    e := NARROW (obj, MFGraph.Edge);
    ebt := NARROW (NEW (Edge, 
                        vertex0 := FindVBTVertex (g, e.from),
                        vertex1 := FindVBTVertex (g, e.to)).init(),
                   Edge);
    (* initialize it *)
    ebt.setColor(Color("gray"));
    ebt.setWidth (EdgeWidth);

    ebt.myGraphEdge := e;

    (*** the arrow edge ***)
    ebt := NEW (Edge, 
                vertex0 := ebt.vertex0, vertex1 := ebt.vertex1).init();
    ebt.setWidth (0.005);
    ebt.toFront();
    ebt.setArrow (arrow);

  END CreateVBTEdge;

(* beautify retrieve the position and label in MFGraph.Vertex and 
   applies it to the twin GraphVBT.Vertex *)

PROCEDURE Beautify (gvbt: GraphVBT.T) =
  VAR v: RefList.T := gvbt.vertices;
      vert: Vertex;
    
  BEGIN
    WHILE v # NIL DO
      vert := v.head;
      v := v.tail;
      vert.move (vert.myGraphVertex.pos, FALSE);
      vert.setLabel (vert.myGraphVertex.label);
    END;  
  END Beautify;


(* create a GraphVBT.T equivalent to MFGraph.T *)
PROCEDURE GraphToGraphVBT (g: MFGraph.T) : GraphVBT.T =
  VAR
      wc := GraphVBT.WorldRectangle{
            w := 0.0, s := 0.0, e := 1.0, n := 1.0};
      gvbt : GraphVBT.T;

  BEGIN
    gvbt := NEW(GraphVBT.T, world := wc).init();

    EVAL g.vertexApply (CreateVBTVertex, gvbt);
    EVAL g.edgeApply (CreateVBTEdge, gvbt);

    Beautify (gvbt);

    RETURN gvbt;
  END GraphToGraphVBT;

(* initialize the graph with the number of edges and vertices *)
PROCEDURE Setup (view: T; g: MFGraph.T; source, sink: MFGraph.Vertex) 
    RAISES {Thread.Alerted} =
  BEGIN
    view.gvbt := GraphToGraphVBT(g);

    view.source := FindVBTVertex (view.gvbt, source);
    view.sink := FindVBTVertex (view.gvbt, sink);

    view.source.setColor(Color ("cornflowerblue"));
    view.source.setLabel ("Source");
    view.source.setSize (R2.T{0.1, 0.1});

    view.sink.setColor(Color ("cornflowerblue"));
    view.sink.setLabel ("Sink");
    view.sink.setSize (R2.T{0.1, 0.1});

    EVAL Filter.Replace(view, view.gvbt);
    view.gvbt.redisplay();
    view.gvbt.animate(0.0, 1.0);
  END Setup;

PROCEDURE New (): View.T =
  VAR a : T :=  NEW(T).init(NEW(GraphVBT.T).init());
  BEGIN
    a.gvbt := NIL;
    RETURN a;
  END New;

PROCEDURE HighlightPath (view: T; path : RefList.T; <* UNUSED *> maxC: REAL)=
  VAR vh: GraphVBT.VertexHighlight;
      curvert: MFGraph.Vertex;
      curedge: MFGraph.Edge;
      p: RefList.T;
      vbt_vertex: Vertex;

  BEGIN
    curedge := path.head;
    p := path.tail;
    curvert := curedge.to;

    vbt_vertex := FindVBTVertex (view.gvbt, curedge.from);
    vh := vbt_vertex.vertexHighlights.head;
    vh.setColor(Color("yellow"));

    vbt_vertex := FindVBTVertex (view.gvbt, curedge.to);
    vh := vbt_vertex.vertexHighlights.head;
    vh.setColor(Color("black"));
    
    WHILE p # NIL DO
      curedge := p.head;
      p := p.tail;
      IF (curedge.from = curvert) THEN
        vbt_vertex := FindVBTVertex (view.gvbt, curedge.to);
        vh := vbt_vertex.vertexHighlights.head;
        vh.setColor(Color("black"));
        curvert := curedge.to;
      ELSE
        vbt_vertex := FindVBTVertex (view.gvbt, curedge.from);
        vh := vbt_vertex.vertexHighlights.head;
        vh.setColor(Color("black"));
        curvert := curedge.from;
      END;
    END;
    view.gvbt.redisplay();
  END HighlightPath;

(* reminder: edge.vertex0 / 1 = from / to *)
PROCEDURE IncFlow(view: T; edge: MFGraph.Edge; flow: REAL;
                  <*UNUSED*> number: CARDINAL;
                  <*UNUSED*> maxC: REAL;  capacity: REAL) 
    RAISES {Thread.Alerted} =
  VAR 
    e : GraphVBT.Edge;
    vh: GraphVBT.VertexHighlight;
    evbt : Edge := FindVBTEdge (view.gvbt, edge);

  BEGIN
    (* animate the flowing thru the edge e *)
    e := NEW (GraphVBT.Edge,  
              vertex0 := evbt.vertex0, vertex1 := evbt.vertex0).init();
    e.setWidth (EdgeWidth);
    e.move (e.vertex0, evbt.vertex1, NIL, NIL, TRUE);
    e.setColor (BlueColor (flow / capacity));

    (* place the arrow on top the animated edge *)
    e.toBack ();
    evbt.toBack();

    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);

    e.remove();

    (* the new flow/edge *)
    IF (flow = capacity) THEN
      evbt.setColor(Color("magenta"));
    ELSE
      evbt.setColor(BlueColor (flow / capacity));
    END;

    vh:= evbt.vertex0.vertexHighlights.head;
    vh.setColor(Color("gray"));
    vh:= evbt.vertex1.vertexHighlights.head;
    vh.setColor(Color("yellow"));

    view.gvbt.redisplay();
END IncFlow;

PROCEDURE DecFlow(view: T;
                  edge: MFGraph.Edge;
       <*UNUSED*> oldflow: REAL;
                  newflow: REAL;
       <*UNUSED*> number: CARDINAL;
       <*UNUSED*> maxC: REAL;
                  capa: REAL) 
    RAISES {Thread.Alerted} =
  VAR 
    e : GraphVBT.Edge;
    vh: GraphVBT.VertexHighlight;
    evbt : Edge := FindVBTEdge (view.gvbt, edge);
  BEGIN

    e := NEW (GraphVBT.Edge,  
              vertex0 := evbt.vertex0, vertex1 := evbt.vertex1).init();
    e.setWidth (EdgeWidth);
    e.move (evbt.vertex0, evbt.vertex0, NIL, NIL, TRUE);
    e.setColor(Color ("limegreen"));
    e.toFront ();

    evbt.setColor(BlueColor (newflow / capa));

    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);

    e.remove();

    vh:= evbt.vertex0.vertexHighlights.head;
    vh.setColor(Color("yellow"));
    vh:= evbt.vertex1.vertexHighlights.head;
    vh.setColor(Color("gray"));

    view.gvbt.redisplay();
  END DecFlow;

PROCEDURE RemoveHighlight(view: T; <*UNUSED*> sinkvertex: MFGraph.Vertex) =
  VAR vh: GraphVBT.VertexHighlight;
  BEGIN
    vh:= view.sink.vertexHighlights.head;
    vh.setColor(Color("gray"));    
  END RemoveHighlight;


PROCEDURE FinalResult(view: T; fin: BOOLEAN) RAISES {Thread.Alerted} =
  BEGIN
    view.gvbt.redisplay();
    IF (fin) THEN Fin (view); END;
  END FinalResult;

PROCEDURE Fin (view: T) RAISES {Thread.Alerted} =
  VAR l : RefList.T := view.gvbt.vertices;
      vbt : Vertex;
      r : R2.T;
  BEGIN
    WHILE l # NIL DO
      vbt := l.head;
      l := l.tail;
      IF Text.Equal (vbt.label, "0") THEN r := R2.T{0.1,0.2};
        ELSIF Text.Equal (vbt.label, "1") THEN r := R2.T{0.1,0.5};
        ELSIF  Text.Equal (vbt.label,"2") THEN r := R2.T{0.3,0.5};
        ELSIF Text.Equal (vbt.label,"3") THEN r := R2.T{0.1,0.7};
        ELSIF Text.Equal (vbt.label, "4") THEN r := R2.T{0.3,0.7};
        ELSIF Text.Equal (vbt.label, "5") THEN r := R2.T{0.5,0.7};
        ELSIF Text.Equal (vbt.label, "6") THEN r := R2.T{0.5,0.2};
        ELSIF Text.Equal (vbt.label,"7") THEN r := R2.T{0.7,0.2};
        ELSIF Text.Equal (vbt.label, "8") THEN r := R2.T{0.7,0.7};
        ELSIF Text.Equal (vbt.label, "9") THEN r := R2.T{0.9,0.2};
        ELSIF Text.Equal (vbt.label, "10") THEN r := R2.T{0.9,0.7};
        ELSIF Text.Equal (vbt.label, "Source") THEN r := R2.T{0.5,0.9};
        ELSIF Text.Equal (vbt.label, "Sink") THEN r := R2.T{1.1,0.2};
        END;
      vbt.move (r, TRUE);
    END;
    view.gvbt.redisplay ();
    view.gvbt.animate (0.0, 3.0);
  END Fin;

BEGIN
  ZeusPanel.RegisterView (New, "Animated Maxflow 2", "Maxflow");

END MFViews2.
