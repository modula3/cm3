(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jan 31 14:53:51 PST 1995 by kalsow     *)
(*      modified on Wed May  4 11:08:12 PDT 1994 by najork     *)
(*      modified on Wed Jan  6 16:49:41 PST 1993 by steveg     *)
(*      modified on Wed Aug  5 12:12:21 PDT 1992 by karsenty   *)
(*      modified on Tue Aug  4 15:13:35 PDT 1992 by karlin     *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb            *)

MODULE MFBarView;

IMPORT MaxflowViewClass, Filter, GraphVBT, R2, ColorName, View, ZeusPanel, 
       MFGraph, RefList, MFAlgs, Fmt, PaintOp, Thread;

TYPE
  T = MaxflowViewClass.T BRANDED OBJECT
	gvbt : GraphVBT.T;
        pathNodes : REF ARRAY OF GraphVBT.Vertex; (* rectangles representing flow *)
        nameOfEdge : REF ARRAY OF GraphVBT.Vertex; (* name of edge,   *)
        pathCapacityNodes : REF ARRAY OF GraphVBT.Vertex; (* rectangles representing capacity *)
        capacityLabels: REF ARRAY OF GraphVBT.Vertex; (* value of capacity *)
        font: GraphVBT.WorldFont;

      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
        oeHighlightPath := HighlightPath;
        oeDecFlow := DecFlow;
        oeIncFlow := IncFlow;
        oeShowEdge := ShowEdge;
        oeFinalResult := FinalResult;
      END;


PROCEDURE Color (color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;


PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, NEW(GraphVBT.T).init());
    (* call the superclass startrun in ZeusClass.T *)
    MaxflowViewClass.T.startrun(view); 
  END Startrun;


PROCEDURE Setup (view: T; <*UNUSED*> g: MFGraph.T;
                 <*UNUSED*> source, sink: MFGraph.Vertex) =
  VAR
    wc := GraphVBT.WorldRectangle{w := 0.0, s := 0.0, e := 1.0, n := 1.0};
  BEGIN
    view.gvbt := NEW(GraphVBT.T, world := wc).init();
    view.font :=
      view.gvbt.font(family := "Helvetica", weight := "bold",
                     slant := GraphVBT.Slant.Roman, size := 0.05);
    EVAL Filter.Replace(view, view.gvbt);
    view.gvbt.redisplay();
  END Setup;

PROCEDURE New (): View.T =
  VAR a : T :=  NEW(T).init(NEW(GraphVBT.T).init());
  BEGIN
    RETURN a;
  END New;

PROCEDURE HighlightPath (view: T; path : RefList.T; maxCap : REAL)=
  VAR 
      len : CARDINAL;
      width, h,  normalizingFactor, theFlow, theCapacity: REAL;
      i : CARDINAL := 0;
      curedge : MFAlgs.MFEdge;
      from, to, curvertex : MFAlgs.MFVertex;
 BEGIN
    view.gvbt.clear();
    view.gvbt.redisplay();
    normalizingFactor := 0.8/maxCap;
    len := RefList.Length(path);
    width := 1.0/FLOAT(len);
    view.pathNodes := NEW(REF ARRAY OF GraphVBT.Vertex, len);
    view.nameOfEdge  := NEW(REF ARRAY OF GraphVBT.Vertex, len);
    view.pathCapacityNodes := NEW(REF ARRAY OF GraphVBT.Vertex, len);
    view.capacityLabels := NEW(REF ARRAY OF GraphVBT.Vertex, len);
    WHILE path # NIL DO
      h :=(width/2.0) + FLOAT(i)*width;
      curedge := path.head;
      from := NARROW( curedge.from, MFAlgs.MFVertex);
      to := NARROW( curedge.to, MFAlgs.MFVertex);
      theFlow :=curedge.flow*normalizingFactor;
      theCapacity := curedge.capacity*normalizingFactor;
      view.nameOfEdge[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                            pos := R2.T{h, 0.05},
                            shape := GraphVBT.VertexShape.Rectangle,
                            size := R2.T{width, 0.1},
                            color := PaintOp.Bg,
                            fontColor := Color("black"),
                            font := view.font,
                            label :=  "(" & from.label & "," 
                                          & to.label & ")").init();
      view.pathNodes[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                           pos := R2.T{h, 0.1 +(theFlow/2.0)},
                           shape := GraphVBT.VertexShape.Rectangle,
                           size := R2.T{width, theFlow},
                           color := Color("cornflowerblue"),
                           fontColor := Color("black"),
                         font := view.font,
                           label :="flow: " & Fmt.Real(curedge.flow),
                           border := 0.001,
                           borderColor := Color("black")).init();
      IF (i # 0) AND (to = curvertex) THEN 
        view.pathNodes[i].setColor(Color("limegreen")); 
        curvertex := from;
      ELSE 
        curvertex := to; 
      END;
      view.pathCapacityNodes[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                                   shape := GraphVBT.VertexShape.Rectangle,
                                   size := R2.T{width, theCapacity},
                                   pos := R2.T{h,  0.1 + ( theCapacity/2.0)},
                                   color := Color("gray"),
                                   fontColor := Color("black"),
                                   border := 0.001,
                                   borderColor := Color("black")).init(); 
      view.pathCapacityNodes[i].toBack();
      view.capacityLabels[i] := NEW (GraphVBT.Vertex, graph := view.gvbt,
                                pos := R2.T{h, 0.15 + theCapacity},
                                size := R2.T{width, 0.1},
                                color := PaintOp.Bg,
                                fontColor := Color("black"),
                                font := view.font,
                                label := "cap: " &Fmt.Real(curedge.capacity )).init();   
      view.capacityLabels[i].toFront();
      INC(i); 
      path := path.tail;
    END;
    view.gvbt.redisplay();
  END HighlightPath;

(* reminder: edge.vertex0 / 1 = from / to *)
PROCEDURE IncFlow(view: T; <*UNUSED*> edge: MFGraph.Edge; flow: REAL; 
                  number: CARDINAL; maxCap, capa: REAL) 
    RAISES {Thread.Alerted} =
  VAR 
    v : GraphVBT.Vertex;
    posx, posy : REAL;
    sizeh, theFlow: REAL;

  BEGIN
    theFlow := (0.8/maxCap)*flow;
    view.nameOfEdge[number].toFront();
    v:= view.pathNodes[number];
    posx:= v.pos[0];
    posy:= v.pos[1]; (* half the old flow + 0.1 *)
    sizeh:= v.size[0];
    v.setSize(R2.T{sizeh, theFlow});
    v.setColor(Color("cornflowerblue"));
    v.setLabel("");
    v.move(R2.T{posx, 2.0*posy - theFlow/2.0 - 0.1 });
    v.move(R2.T{posx, 0.1 +(theFlow/2.0)}, TRUE);
    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);
    IF flow = capa THEN v.setColor(Color("magenta")); END;
    v.setLabel("flow: " & Fmt.Real(flow));
    view.gvbt.redisplay();
  END IncFlow;

PROCEDURE DecFlow(view: T;
       <*UNUSED*> edge: MFGraph.Edge;
       <*UNUSED*> oldflow: REAL;
                  newflow: REAL; 
                  number: CARDINAL;
                  maxCap: REAL;
       <*UNUSED*> capa : REAL) 
    RAISES {Thread.Alerted} =
  VAR 
    v: GraphVBT.Vertex;
    posx, posy : REAL;
    sizeh, theFlow : REAL;

  BEGIN
    theFlow := (0.8/maxCap)*newflow;
    v:= view.pathNodes[number];
    posx:= v.pos[0];
    posy:= v.pos[1]; (* half the old flow + 0.1 *)
    sizeh:= v.size[0];
    view.nameOfEdge[number].toFront();
    v.setLabel("");
    v.move(R2.T{posx, 0.2 + theFlow - posy }, TRUE);
    view.gvbt.redisplay(); 
    view.gvbt.animate(0.0, 1.0);
    v.setLabel("flow: " & Fmt.Real(newflow));
    v.setSize(R2.T{sizeh, theFlow});
    v.move(R2.T{posx, 0.1 +(theFlow/2.0)});
    view.gvbt.redisplay();
  END DecFlow; 

PROCEDURE ShowEdge (view: T; number, typeOfEdge : CARDINAL) =
  BEGIN
    IF (number = 0) AND (typeOfEdge # 3)THEN
      IF (typeOfEdge = 0) THEN
        view.nameOfEdge[number].setFontColor(Color("magenta"));
      ELSE
        view.nameOfEdge[number].setFontColor(Color("cornflowerblue"));
      END
    ELSE
      CASE typeOfEdge OF <*NOWARN*>
       | 0 =>   view.nameOfEdge[number].setFontColor(Color("magenta"));
       | 1 =>  
            view.nameOfEdge[number].setFontColor(Color("cornflowerblue"));
            view.nameOfEdge[number-1].setFontColor(Color("black"));            
       | 2 =>  
            view.nameOfEdge[number].setFontColor(Color("limegreen"));
            view.nameOfEdge[number-1].setFontColor(Color("black"));
       | 3 =>  
            view.nameOfEdge[number-1].setFontColor(Color("black"));
      END;
    END;
    view.gvbt.redisplay();
  END ShowEdge;

PROCEDURE FinalResult(view: T; <* UNUSED *> b: BOOLEAN) =
  BEGIN
    view.gvbt.redisplay();
  END FinalResult;

BEGIN
  ZeusPanel.RegisterView (New, "Path View", "Maxflow");

END MFBarView.
