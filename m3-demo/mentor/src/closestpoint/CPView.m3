(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE CPView;

IMPORT CPViewClass, ColorName, Filter, GraphVBT, Math, PaintOp, R2, RefList, 
       TextVBT, Thread, View, ZeusPanel;

TYPE
  T = CPViewClass.T BRANDED OBJECT
        graph                        : GraphVBT.T;
        minfixedp, minmovep, minlabel: GraphVBT.Vertex;
        minedge                      : GraphVBT.Edge;
        mingraphedge                 : GraphVBT.Edge;
        activeRpoint                 : GraphVBT.Vertex;
        closeRpoint                  : GraphVBT.Vertex;
        barleft                      : GraphVBT.Vertex   := NIL;
        barright                     : GraphVBT.Vertex   := NIL;
        baredge                      : GraphVBT.Edge     := NIL;
      OVERRIDES
        startrun              := Startrun;
        oeSetup               := Setup;
        oeAddPoint            := AddPoint;
        oeNotProcessed        := NotProcessed;
        oeRemoveNotProcessed  := RemoveNotProcessed;
        oeActiveR             := ActiveR;
        oeRemoveActiveR       := RemoveActiveR;
        oeCloseR              := CloseR;
        oeRemoveCloseR        := RemoveCloseR;
        oeSplit               := Split;
        oeSplitRemove         := SplitRemove;
        oeNewMin              := NewMin;
        oeNoChangeMin         := NoChangeMin;
        oeSelectTrialPoint    := SelectTrialPoint;
        oeDeselectTrialPoint  := DeselectTrialPoint;
        oeSelectTargetPoint   := SelectTargetPoint;
        oeDeselectTargetPoint := DeselectTargetPoint;
        oeDrawBar             := DrawBar;
        oeRemoveBar           := RemoveBar;
      END;

CONST
  worldsize = GraphVBT.WorldRectangle{
                  w := -0.2, e := 1.325, n := 1.325, s := -0.2};
  minfixedpos    = R2.T{0.35, 1.25};
  newminfixedpos = R2.T{0.35, 1.20};
  minlabelpos    = R2.T{0.15, 1.25};

  mincolor          = "verystrongred";
  minnodecolor      = "verystrongred";
  nodecolor         = "darkgray";
  closercolor       = "verylightyellowgreen";
  splitcolor        = "cadetblue";
  trialcolor        = "skyblue";
  targetcolor       = "green";
  targetedgecolor   = "lightfirebrick";
  activecolor       = "verylightcyan";
  notprocessedcolor = "ratherlightgrey";
  barcolor          = "ratherstrongbrown";

VAR
  pointsize : REAL;


PROCEDURE Color (col: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(col);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

EXCEPTION NotFound;

PROCEDURE FindVertex (view: T; p: R2.T): GraphVBT.Vertex
  RAISES {NotFound} =
  VAR
    l: RefList.T          := view.graph.vertices;
    c: GraphVBT.Vertex;
  BEGIN
    WHILE l # NIL DO
      c := l.head;
      l := l.tail;
      IF c.pos = p THEN RETURN c; END (* IF *);
    END (* WHILE *);
    RAISE NotFound;
  END FindVertex;

PROCEDURE Startrun (view: T) =
  (* Marc's sleazy hack: remove the old GraphVBT and just ignore it; heck,
     what else are VM and GC good for? *)
  BEGIN
    EVAL Filter.Replace(view, TextVBT.New(""));
    CPViewClass.T.startrun(view);
  END Startrun;

PROCEDURE Setup (view: T) =
  VAR font: GraphVBT.WorldFont;
  BEGIN
    view.graph :=
      NEW(GraphVBT.T, world := worldsize, aspect := 1.0).init();
    font := view.graph.font(size := 0.02);
    view.minlabel :=
      NEW(GraphVBT.Vertex, graph := view.graph, pos := minlabelpos,
          color := Color("white"), font := font, fontColor := Color("VeryDarkOrchid"),
          label := "Current Min", size := R2.T{0.40, 0.08}).init();
    view.minfixedp :=
      NEW(GraphVBT.Vertex, graph := view.graph, pos := minfixedpos,
          color := Color("white"), size := R2.T{0.001, 0.001}).init();
    view.minmovep :=
      NEW(GraphVBT.Vertex, graph := view.graph, pos := minfixedpos,
          color := Color("white"), size := R2.T{0.001, 0.001}).init();
    view.minedge := NEW(GraphVBT.Edge, vertex0 := view.minfixedp,
                        vertex1 := view.minmovep, width := 0.001,
                        color := Color(mincolor)).init();
    view.barleft := NIL;
    view.barright := NIL;
    view.baredge := NIL;
    EVAL Filter.Replace(view, view.graph);
  END Setup;

PROCEDURE AddPoint (view: T; p: R2.T; N: INTEGER; <* UNUSED *> big: BOOLEAN) =
  VAR
    curP: GraphVBT.Vertex;
    lab : TEXT;
  (* pointsize is global *)
  BEGIN
    (* add point to the Graph *)
    IF (N < 30) THEN
      pointsize := FLOAT(MIN(20 DIV N + 1, 8)) / 100.0;
      lab := "";
    ELSE
      pointsize := 0.025;
      lab := "";
    END (* IF *);
    pointsize := 0.03;
    curP := NEW(GraphVBT.Vertex, graph := view.graph, pos := p,
                shape := GraphVBT.VertexShape.Ellipse, label := lab,
                color := Color(nodecolor),
                size := R2.T{pointsize, pointsize}).init();
    view.graph.redisplay();
  END AddPoint;

PROCEDURE Split (view: T; x: REAL; <* UNUSED *> big: BOOLEAN) =
  (* Draw line at this x coord in splitcolor *)
  VAR
    dp1, dp2: GraphVBT.Vertex;
    e       : GraphVBT.Edge;
  BEGIN
      dp1 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x, -0.15},
               size := R2.T{0.001, 0.001}, color := PaintOp.Bg).init();
      dp2 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x, 1.15},
               size := R2.T{0.001, 0.001}, color := PaintOp.Bg).init();
      e :=
      NEW(
        GraphVBT.Edge, vertex0 := dp1, vertex1 := dp2, width := 0.010).init();
      e.setColor(Color(splitcolor));
     e.vertex0.graph.redisplay();
  END Split;

PROCEDURE SplitRemove (view: T; x: REAL; <* UNUSED *> big: BOOLEAN) =
  (* deletes splitting line at this x coord *)
  VAR c: GraphVBT.Vertex;
  BEGIN
    TRY
      (* find a vertex that has points {x, -0.2} and remove it from the
         graph *)
      c := FindVertex(view, R2.T{x, -0.15});
      c.remove();
      c := FindVertex(view, R2.T{x, 1.15});
      c.remove();
      view.graph.redisplay();
    EXCEPT
      NotFound =>
    END (* TRY *);
  END SplitRemove;


PROCEDURE NotProcessed (view: T; x1, x2: REAL; <* UNUSED *> big: BOOLEAN) =
  VAR
    p1, p2, p3, p4: GraphVBT.Vertex;
    poly          : GraphVBT.Polygon;
    verts         : RefList.T;
  BEGIN
    p1 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x1, -0.1},
              shape := GraphVBT.VertexShape.Rectangle,
              size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    p2 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x1, 1.1},
              shape := GraphVBT.VertexShape.Rectangle,
              size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    p3 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x2, 1.1},
              shape := GraphVBT.VertexShape.Rectangle,
              size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    p4 := NEW(GraphVBT.Vertex, graph := view.graph, pos := R2.T{x2, -0.1},
              shape := GraphVBT.VertexShape.Rectangle,
              size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    verts := RefList.Cons (p1, RefList.Cons (p2, 
                                 RefList.Cons (p3, RefList.Cons (p4, NIL))));
    poly := NEW(GraphVBT.Polygon, 
                color    := Color (notprocessedcolor),
                vertices := verts).init();
    view.graph.redisplay();
  END NotProcessed;

PROCEDURE RemoveNotProcessed (view: T; x1, x2: REAL; 
                              <* UNUSED *> big: BOOLEAN; ) =
  VAR p: GraphVBT.Vertex;
  BEGIN
    TRY
      p := FindVertex(view, R2.T{x1, -0.1});
      p.remove();
      p := FindVertex(view, R2.T{x1, 1.1});
      p.remove();
      p := FindVertex(view, R2.T{x2, -0.1});
      p.remove();
      p := FindVertex(view, R2.T{x2, 1.1});
      p.remove();
    EXCEPT
      NotFound =>
    END (* TRY *);
    view.graph.redisplay();
  END RemoveNotProcessed;

PROCEDURE ActiveR (view: T; x1, x2: REAL; <* UNUSED *> big: BOOLEAN) =
  (* Shade the currently active region of the graph *)
  VAR
    dp1 : GraphVBT.Vertex;
    vh     : GraphVBT.VertexHighlight;
  BEGIN
    dp1 := NEW(GraphVBT.Vertex, graph := view.graph, 
               pos := R2.T{(x1+x2)/2.0, 0.5},
               shape := GraphVBT.VertexShape.Rectangle,
               size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    
    vh := NEW(GraphVBT.VertexHighlight, color := Color(activecolor),
                vertex := dp1, border := R2.T{(x2-x1)/2.0+0.01, 0.65}).init();

    dp1.graph.redisplay();
    view.activeRpoint := dp1;
  END ActiveR;

PROCEDURE RemoveActiveR (             view  : T; 
                         <* UNUSED *> x1, x2: REAL; 
                         <* UNUSED *> big   : BOOLEAN) =
  (* remove the vertex showing the active region *)
  BEGIN
    view.activeRpoint.remove();
    view.graph.redisplay();
  END RemoveActiveR;

PROCEDURE CloseR (view: T; x1, x2: REAL; <* UNUSED *> big: BOOLEAN) =
  (* Shade the currently "close" region of the graph *)
  VAR
    dp1 : GraphVBT.Vertex;
    vh     : GraphVBT.VertexHighlight;
  BEGIN
    dp1 := NEW(GraphVBT.Vertex, graph := view.graph, 
               pos := R2.T{(x1+x2)/2.0, 0.5},
               shape := GraphVBT.VertexShape.Rectangle,
               size := R2.T{0.0, 0.0}, color := PaintOp.Bg).init();
    
    vh := NEW(GraphVBT.VertexHighlight, color := Color(closercolor),
                vertex := dp1, border := R2.T{(x2-x1)/2.0, 0.6}).init();

    vh.toFront();
    dp1.graph.redisplay();
    view.closeRpoint := dp1;
  END CloseR;

PROCEDURE RemoveCloseR (             view   : T; 
                        <* UNUSED *> x1, x2 : REAL; 
                        <* UNUSED *> big    : BOOLEAN) =
  (* remove the vertex showing the "close" region *)
  BEGIN
    view.closeRpoint.remove();
    view.graph.redisplay();
  END RemoveCloseR;

PROCEDURE NewMin (             view   : T; 
                               p1, p2 : R2.T; 
                  <* UNUSED *> c1, c2 : R2.T; 
                  <* UNUSED *> big    : BOOLEAN) 
    RAISES {Thread.Alerted} =
    (* p1,p2 are the new min pair and c1, c2 are the old pair *)
  VAR dist: REAL;
      dp1, dp2, temp1, temp2: GraphVBT.Vertex;
      e,tempedge: GraphVBT.Edge;
      firstmin: BOOLEAN := TRUE;
    (* shows comparison and updates min *)
    (* generate copies of both nodes and edges, move them to the top, make
       a comparison and generate a new min line *)
    (* generate a new point the min distance from minfixedp *)
  BEGIN
    IF (view.mingraphedge # NIL) THEN
      view.mingraphedge.vertex0.setColor(Color(nodecolor));
      view.mingraphedge.vertex1.setColor(Color(nodecolor));
      view.mingraphedge.remove(); 
      view.graph.redisplay();
      view.mingraphedge:=NIL;
      firstmin := FALSE;
    END (* IF *);
   
    dist :=
      FLOAT(
        Math.sqrt(FLOAT((p1[0] - p2[0]) * (p1[0] - p2[0])
                  + (p1[1] - p2[1]) * (p1[1] - p2[1]),
                  LONGREAL)));

    TRY
      (* find vertices that are the new min *)
      dp1 := FindVertex(view, p1);
      dp2 := FindVertex(view, p2);
      temp1 := NEW(GraphVBT.Vertex, graph := view.graph, 
                   shape := GraphVBT.VertexShape.Ellipse,
                   pos := newminfixedpos, color := Color(nodecolor), 
                   size := R2.T{0.0, 0.0}).init();
      temp2 := NEW(GraphVBT.Vertex, graph := view.graph, 
                   shape := GraphVBT.VertexShape.Ellipse,
                   pos := R2.T{newminfixedpos[0]+dist,newminfixedpos[1]}, 
                   color := Color(nodecolor), 
                   size := R2.T{0.0, 0.0}).init();
      tempedge := NEW(GraphVBT.Edge, vertex0 := dp1, vertex1 := dp2, 
               color:=Color(targetedgecolor), width := 0.010).init();

      view.graph.redisplay();
      tempedge.move (temp1, temp2, animated := TRUE);
      view.graph.animate(0.0, 5.0);
      IF NOT firstmin THEN 
        Thread.Pause (0.5d0);  
      END (* IF *);
      temp1.remove();
      temp2.remove();

      e := NEW(GraphVBT.Edge, vertex0 := dp1, vertex1 := dp2, 
               color:=Color(mincolor), width := 0.010).init();
      e.vertex0.setColor(Color(minnodecolor));
      e.vertex1.setColor(Color(minnodecolor));
      view.mingraphedge := e;
    EXCEPT
      NotFound => 
    END (* TRY *);

    view.minmovep.move(R2.T{minfixedpos[0]+dist,minfixedpos[1]});
    view.graph.redisplay();
  END NewMin;

PROCEDURE NoChangeMin (<*UNUSED *> view           : T; 
                       <*UNUSED *> p1, p2, c1, c2 : R2.T; 
                       <*UNUSED *> big            : BOOLEAN) =
  BEGIN
    (* shows comparisson without updating min *)
    (* NOT DONE *)
  END NoChangeMin;

PROCEDURE SelectTrialPoint (view: T; x: R2.T; <* UNUSED *> big: BOOLEAN) =
  (* color point trial color *)
  VAR
    c      : GraphVBT.Vertex;
    vh     : GraphVBT.VertexHighlight;
    bsize  : REAL;                     (* size of the border *)
  BEGIN
    TRY
      c := FindVertex(view, x);
      bsize := c.size[0] * 0.20;
      vh := NEW(GraphVBT.VertexHighlight, color := Color(trialcolor),
                vertex := c, border := R2.T{bsize, bsize}).init();
      vh.toFront();
      view.graph.redisplay();
    EXCEPT
      NotFound =>
    END (* TRY *);
  END SelectTrialPoint;

PROCEDURE DeselectTrialPoint (view: T; x: R2.T; <* UNUSED *> big: BOOLEAN) =
  (* uncolor point trial color *)
  VAR
    c  : GraphVBT.Vertex;
    vhl: RefList.T;
    vh : GraphVBT.VertexHighlight;
  BEGIN
    TRY
      c := FindVertex(view, x);
      vhl := c.vertexHighlights;
      WHILE (vhl # NIL) DO
        vh := vhl.head;
        vh.remove();
        vhl := vhl.tail;
      END (* WHILE *);
      view.graph.redisplay();
    EXCEPT
      NotFound =>
    END (* TRY *);
  END DeselectTrialPoint;

PROCEDURE SelectTargetPoint (view: T; trialp, targp: R2.T; 
                             <* UNUSED *> big : BOOLEAN) =
  (* if not dummy then color point target color *)
  VAR
    targv, trialv: GraphVBT.Vertex;
    e            : GraphVBT.Edge;
    vh           : GraphVBT.VertexHighlight;
    bsize      : REAL;                     (* size of the border *)
  BEGIN
    TRY
      targv := FindVertex(view, targp);
      trialv := FindVertex(view, trialp);
      bsize := targv.size[0] * 0.20;
      vh :=
        NEW(GraphVBT.VertexHighlight, color := Color(targetcolor),
            vertex := targv, border := R2.T{bsize, bsize}).init();
      e := NEW(GraphVBT.Edge, vertex0 := trialv, vertex1 := targv,
               width := 0.010).init();
      e.setColor(Color(targetedgecolor));
      view.graph.redisplay();
    EXCEPT
      NotFound =>
    END (* TRY *);
  END SelectTargetPoint;

PROCEDURE DeselectTargetPoint (view: T; trialp, targp: R2.T; 
                               <* UNUSED *> big: BOOLEAN) =
  (* if not dummy then decolor point and remove edge *)
  VAR
    el           : RefList.T;
    trialv, targv: GraphVBT.Vertex;
    e            : GraphVBT.Edge;
    vhl          : RefList.T;
    vh           : GraphVBT.VertexHighlight;
  BEGIN
    TRY
      targv := FindVertex(view, targp);
      trialv := FindVertex(view, trialp);
      el := targv.edges;
      vhl := targv.vertexHighlights;
      WHILE (vhl # NIL) DO
        vh := vhl.head;
        vh.remove();
        vhl := vhl.tail;
      END (* WHILE *);
      WHILE el # NIL DO
        e := el.head;
        el := el.tail;
        IF (e.vertex0 = trialv) AND (e # view.mingraphedge) THEN 
          e.remove(); 
          EXIT; 
        END (* IF *);
      END (* WHILE *);
      view.graph.redisplay();
    EXCEPT
      NotFound =>
    END (* TRY *);
  END DeselectTargetPoint;

PROCEDURE DrawBar (view: T; y, x1, x2: REAL; <* UNUSED *> big: BOOLEAN) 
    RAISES {Thread.Alerted} =
  VAR v1, v2: GraphVBT.Vertex;
  BEGIN
    v1 := NEW(GraphVBT.Vertex, graph := view.graph,
              shape := GraphVBT.VertexShape.Ellipse,
              pos := R2.T{x1 + 0.001, y}, color := Color("white"),
              size := R2.T{0.0, 0.0}).init();
    v2 := NEW(GraphVBT.Vertex, graph := view.graph,
              shape := GraphVBT.VertexShape.Ellipse,
              pos := R2.T{x2 - 0.001, y}, color := Color("white"),
              size := R2.T{0.0, 0.0}).init();
    IF (view.barleft = NIL AND view.barright = NIL AND view.baredge = NIL) THEN
      view.barleft := v1;
      view.barright := v2;
      view.baredge := NEW(GraphVBT.Edge, vertex0 := view.barleft,
                          vertex1 := view.barright,
                          color := Color(barcolor), width := 0.003).init();
    ELSE
      view.baredge.move(v1, v2, animated := TRUE);
      view.graph.animate(0.0, 1.0);
      view.barleft.remove();
      view.barright.remove();
      view.barleft := v1;
      view.barright := v2;
    END (* IF *);
    view.graph.redisplay();
  END DrawBar;

PROCEDURE RemoveBar (             view      : T; 
                     <* UNUSED *> y, x1, x2 : REAL; 
                     <* UNUSED *> big       : BOOLEAN) =
  BEGIN
    IF (view.barleft # NIL) THEN
      view.barleft.remove();
      view.barleft := NIL;
    END (* IF *);

    IF (view.barright # NIL) THEN
      view.barright.remove();
      view.barright := NIL;
    END (* IF *);
    view.baredge := NIL;
  END RemoveBar;

PROCEDURE New (): View.T =
  VAR graph := NEW(GraphVBT.T, world := worldsize, aspect := 1.0).init();
  BEGIN
    RETURN NEW(T, graph := graph).init(graph);
  END New;

BEGIN
  ZeusPanel.RegisterView(New, "Points View", "CP");
END CPView.

