(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* Last modified on Tue Jan 31 14:53:52 PST 1995 by kalsow   *)
(*      modified on Wed May  4 11:08:40 PDT 1994 by najork   *)
(*      modified on Wed Jan  6 15:28:10 PST 1993 by steveg   *)
(*      modified on Wed Aug  5 12:12:02 PDT 1992 by karsenty *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb          *)
<* PRAGMA LL *>
MODULE MFViews;

IMPORT MaxflowViewClass, Filter, GraphVBT, R2, ColorName, View, ZeusPanel, 
       MFGraph, RefList, MFAlgs, PaintOp, VBT, MaxflowIE, Rect, ZeusDataView,
       Thread;

CONST ScalingFactor = 0.015;

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
        oeRemoveHighlight := RemoveHighlight;
        oeDecFlow := DecFlow;
        oeIncFlow := IncFlow;
        oeFinalResult := FinalResult;

        <* LL=VBT.mu *>
        ueAddVBTVertex := AddVBTVertex;
        ueAddVBTEdge := AddVBTEdge;

        mouse := MouseProcess;

      END;

(* upon a click create a new vertex *)
PROCEDURE MouseProcess(self: T; READONLY cd: VBT.MouseRec) =
  <* FATAL Thread.Alerted *>
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
    vbt := v.data;
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

PROCEDURE PrintEdge( <* UNUSED *> e: MFGraph.Edge): TEXT =
  BEGIN
    RETURN "rien a dire";
  END PrintEdge;

PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB (color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

(* sat is the blue saturation 0 for light, 1 for full saturation *)
PROCEDURE BlueColor(sat: REAL): PaintOp.T =
  VAR r : REAL := 0.6  -  (0.6 * sat);
  BEGIN
    RETURN PaintOp.FromRGB (r, r, 1.0);
  END BlueColor;

PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, NEW(GraphVBT.T).init());
    (* call the superclass startrun in ZeusClass.T *)
    MaxflowViewClass.T.startrun(view); 
  END Startrun;

PROCEDURE CreateVBTVertex (obj, arg: REFANY) =
  VAR
    v      : MFGraph.Vertex;
    vbt    : GraphVBT.Vertex;
    g                        := NARROW(arg, GraphVBT.T);
    stdsize: R2.T;
  BEGIN
    stdsize := R2.T{0.08, 0.08};
    v := NARROW(obj, MFGraph.Vertex);
    vbt :=
      NEW(GraphVBT.Vertex, graph := g, pos := R2.T{0.5, 0.5},
          size := stdsize, shape := GraphVBT.VertexShape.Ellipse).init();

    v.data := vbt;
    vbt.setFont(g.font(family := "Helvetica", weight := "bold",
                       slant := GraphVBT.Slant.Roman, size := 0.05));
    vbt.setFontColor(Color("Black"));
    vbt.setColor(Color("gray"));

    EVAL NEW(GraphVBT.VertexHighlight, vertex := vbt,
             color := Color("gray"), border := R2.T{0.01, 0.01}).init();

  END CreateVBTVertex;

PROCEDURE CreateVBTEdge (obj: REFANY; <* UNUSED *> arg: REFANY) =
  VAR e : MFGraph.Edge;
      mfe : MFAlgs.MFEdge;
      ebt : GraphVBT.Edge;
  BEGIN
    e := NARROW (obj, MFGraph.Edge);
    ebt := NEW (GraphVBT.Edge,  
                vertex0 := NARROW (e.from.data, GraphVBT.Vertex), 
                vertex1 := NARROW (e.to.data, GraphVBT.Vertex)).init();
    ebt.setColor(Color("cornflowerblue"));
    ebt.setWidth (0.0);
    e.data := ebt;

    (* create the capacity edge : in order to simulate borders, we
       create two edges, a black one and a white one on top, with width
       slightly smaller *)

    ebt := NEW (GraphVBT.Edge,  
                vertex0 := ebt.vertex0, vertex1 := ebt.vertex1).init();
    mfe := NARROW (obj, MFAlgs.MFEdge);

    ebt.setWidth ((mfe.capacity * ScalingFactor) + 1.0);
    ebt.toFront();
    ebt.setColor(Color ("black"));
    ebt := NEW (GraphVBT.Edge,  
                vertex0 := ebt.vertex0, vertex1 := ebt.vertex1).init();
    ebt.setWidth (mfe.capacity * ScalingFactor);
    ebt.toFront();
    ebt.setColor(Color ("white"));

  END CreateVBTEdge;

(* beautify retrieve the position and label in MFGraph.Vertex and 
   applies it to the twin GraphVBT.Vertex *)

PROCEDURE Beautify (g: MFGraph.T) =
  VAR v: MFGraph.VertexList := g.vertices;
      vert: MFAlgs.MFVertex;
    
  BEGIN
    WHILE v # NIL DO
      vert := v.vertex;
      NARROW (vert.data, GraphVBT.Vertex).move (vert.pos, FALSE);
      NARROW (vert.data, GraphVBT.Vertex).setLabel (vert.label);
      v := v.next;
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

    Beautify (g);

    RETURN gvbt;
  END GraphToGraphVBT;

(* initialize the graph with the number of edges and vertices *)
PROCEDURE Setup (view: T; g: MFGraph.T; source, sink: MFGraph.Vertex) 
    RAISES {Thread.Alerted} =
  BEGIN
    view.gvbt := GraphToGraphVBT(g);

    view.source := source.data;
    view.sink := sink.data;

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
  VAR vh, sourcevh: GraphVBT.VertexHighlight;
      curvert: MFGraph.Vertex;
      curedge: MFGraph.Edge;
      p: RefList.T;
      vbt_vertex: GraphVBT.Vertex;

  BEGIN
    curedge := path.head;
    p := path.tail;
    curvert := curedge.to;
    vbt_vertex := NARROW(curedge.from.data, GraphVBT.Vertex);
    vh := vbt_vertex.vertexHighlights.head;
    sourcevh := vh;
    vh.setColor(Color("black"));
    vbt_vertex := NARROW(curedge.to.data, GraphVBT.Vertex);
    vh := vbt_vertex.vertexHighlights.head;
    vh.setColor(Color("black"));
    
    WHILE p # NIL DO
      curedge := p.head;
      p := p.tail;
      IF (curedge.from = curvert) THEN
        vbt_vertex := NARROW(curedge.to.data, GraphVBT.Vertex);
        vh := vbt_vertex.vertexHighlights.head;
        vh.setColor(Color("black"));
        curvert := curedge.to;
      ELSE
        vbt_vertex := NARROW(curedge.from.data, GraphVBT.Vertex);
        vh := vbt_vertex.vertexHighlights.head;
        vh.setColor(Color("black"));
        curvert := curedge.from;
      END;
    END;

    sourcevh.setColor(Color("yellow"));
    view.gvbt.redisplay();
  END HighlightPath;

(* reminder: edge.vertex0 / 1 = from / to *)
PROCEDURE IncFlow(view: T; edge: MFGraph.Edge; flow: REAL;
                  <*UNUSED*> number: CARDINAL;
                  <*UNUSED*> maxC: REAL;
                             capacity: REAL) 
    RAISES {Thread.Alerted} =
  VAR 
    e : GraphVBT.Edge;
    vh: GraphVBT.VertexHighlight;
    evbt : GraphVBT.Edge := edge.data;

  BEGIN
    (* animate the flowing thru the edge e *)
    e := NEW (GraphVBT.Edge,  
              vertex0 := evbt.vertex0, vertex1 := evbt.vertex0).init();
    e.setWidth(flow * ScalingFactor);
    e.move (e.vertex0, evbt.vertex1, NIL, NIL, TRUE);
    e.setColor (Color("cornflowerblue"));
    e.toFront ();

    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);

    e.remove();

    (* the new flow/edge *)
    evbt.setWidth(flow * ScalingFactor);
    evbt.toFront();
    IF (flow = capacity) THEN
      evbt.setColor(Color("magenta"));
    ELSE
      evbt.setColor(Color("cornflowerblue"));
    END;

    vh:= evbt.vertex0.vertexHighlights.head;
    vh.setColor(Color("gray"));
    vh:= evbt.vertex1.vertexHighlights.head;
    vh.setColor(Color("yellow"));

    view.gvbt.redisplay();
END IncFlow;

PROCEDURE DecFlow(view: T;
                  edge: MFGraph.Edge;
                  oldflow, newflow: REAL;
       <*UNUSED*> number: CARDINAL;
       <*UNUSED*> maxC, capa: REAL) RAISES {Thread.Alerted} =
  VAR 
    e : GraphVBT.Edge;
    vh: GraphVBT.VertexHighlight;
    evbt : GraphVBT.Edge := edge.data;

  BEGIN

    e := NEW (GraphVBT.Edge,  
              vertex0 := evbt.vertex0, vertex1 := evbt.vertex1).init();
    e.setWidth(oldflow * ScalingFactor);
    e.move (evbt.vertex0, evbt.vertex0, NIL, NIL, TRUE);
    e.setColor(Color ("limegreen"));
    e.toFront ();

    evbt.setWidth(newflow * ScalingFactor);
    evbt.setColor(Color("cornflowerblue"));

    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);

    e.remove();

    vh:= evbt.vertex0.vertexHighlights.head;
    vh.setColor(Color("yellow"));
    vh:= evbt.vertex1.vertexHighlights.head;
    vh.setColor(Color("gray"));

    view.gvbt.redisplay();
  END DecFlow;

PROCEDURE RemoveHighlight(<*UNUSED*> view: T; sinkvertex: MFGraph.Vertex) =
  VAR
     v: GraphVBT.Vertex := sinkvertex.data;
    vh: GraphVBT.VertexHighlight;

  BEGIN
    vh:= v.vertexHighlights.head;
    vh.setColor(Color("gray"));    
  END RemoveHighlight;


PROCEDURE FinalResult(view: T; <* UNUSED *> b: BOOLEAN) =
  BEGIN
    view.gvbt.redisplay();
  END FinalResult;

BEGIN
  ZeusPanel.RegisterView (New, "Animated Maxflow", "Maxflow");
  ZeusPanel.RegisterView (ZeusDataView.New, "Data View", "Maxflow");

END MFViews.
