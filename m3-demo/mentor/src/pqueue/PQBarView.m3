(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed May  4 09:19:05 PDT 1994 by najork     *)
(*      modified on Wed Jan  6 15:47:16 PST 1993 by steveg     *)
(*      modified on Sun Aug  2 11:30:56 PDT 1992 by owicki     *)
(*      modified on Wed Jul 22 01:10:06 1992 by mhb            *)
MODULE PQBarView;

IMPORT PQueueViewClass, Filter, GraphVBT, PQueue, R2, Thread, TextVBT, View, 
       ZeusPanel, RefList, VBT;

CONST OffScreenPos = R2.T{1.0, FLOAT(2*PQueue.MaxElt)};

TYPE
  T = PQueueViewClass.T BRANDED OBJECT
        mg: GraphVBT.T;
        size : INTEGER;
        maxSize: INTEGER;
        currentLevel: INTEGER; (* Level of last node in tree *)
        lastLevel: INTEGER;
        nodes: REF ARRAY OF GraphVBT.Vertex;
        lastInLevel: REF ARRAY OF INTEGER;
        highlights: REF ARRAY OF GraphVBT.Vertex;
        axisNodes: REF ARRAY OF GraphVBT.Vertex;
        tickMarks: REF ARRAY OF GraphVBT.Vertex;
        arcs : RefList.T;
        hlt1, hlt2, hlt3: GraphVBT.Vertex;
        workNode: INTEGER;
      OVERRIDES
        startrun := Startrun;
        oeSetup := Setup;
        oeInitSort := InitSort;
        oeHeapOpInit := HeapOpInit;
        oeHeapStep := HeapStep;
        oePlaceElement := PlaceElement;
        oeSortStep := SortStep;
        oeInsert := Insert;
        oeRemove := Remove;
        oePause := Pause;
        oeCompare := Compare;
      END;

PROCEDURE Startrun (view: T) =
  (* sleazy hack: remove the old GraphVBT and just ignore it;
     heck, what else are VM and GC good for? *)
  BEGIN
    LOCK VBT.mu DO EVAL Filter.Replace(view, TextVBT.New("")); END;
    PQueueViewClass.T.startrun(view);
  END Startrun;

PROCEDURE Setup (view: T; size: INTEGER; doSort: BOOLEAN) =
  VAR 
    wc:GraphVBT.WorldRectangle;
    nMarks, nodes: INTEGER; 
  BEGIN
    wc := GraphVBT.WorldRectangle{
            w := 0.5, s := -20.0, e := FLOAT(size)+0.5, 
            n := FLOAT(PQueue.MaxElt)};
    view.mg := NEW(GraphVBT.T, world := wc, margin := 0.05,
                   pixelSizeDivisor := ARRAY [0..1] OF CARDINAL {size, 1})
                   .init();
    LOCK VBT.mu DO EVAL Filter.Replace(view, view.mg); END;
    view.nodes := NEW(REF ARRAY OF GraphVBT.Vertex, size+1);
    view.axisNodes := NEW(REF ARRAY OF GraphVBT.Vertex, size+1);
    view.highlights := NEW(REF ARRAY OF GraphVBT.Vertex, size+1);
    FOR node := 1 TO size DO
        view.axisNodes[node] := NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(node), 0.0},
            size := R2.T{0.0, 0.0}).init();
        view.highlights[node] := NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(node), -1.75},
            color := PQueue.HighlightColor,
            size := R2.T{0.8, 3.0});
    END;
    view.arcs := NIL;
    view.hlt1 := view.highlights[1];
    view.hlt2 := view.highlights[1];
    view.hlt3 := view.highlights[1];
    IF doSort THEN view.size := size ELSE view.size := 0 END;
    view.maxSize := size;

    (* Create tick mark vertices, but don't display them *)
    nodes := 1; nMarks := 1;
    WHILE nodes < size DO
      INC(nMarks); nodes := 2*nodes+1;
    END;
    nodes := 0;
    view.lastInLevel := NEW(REF ARRAY OF INTEGER, nMarks+1);
    view.tickMarks := NEW(REF ARRAY OF GraphVBT.Vertex, nMarks+1);
    nodes := 0;
    FOR i := 0 TO nMarks DO
      view.lastInLevel[i] := nodes;
      view.tickMarks[i] := NEW(GraphVBT.Vertex, graph := view.mg,
            pos := R2.T{FLOAT(nodes)+ 0.5, -1.125},
            color := PQueue.Black,
            size := R2.T{0.1, 3.0}); 
      nodes := 2*nodes+1;
    END;
    view.tickMarks[0].pos := OffScreenPos;
    (* Don't dispaly the tickMark at position 0 *)
    view.lastLevel := nMarks;
    view.currentLevel := 0;
  END Setup;

PROCEDURE InitSort(view: T; vals: REF ARRAY OF INTEGER) 
    RAISES {Thread.Alerted} =
  VAR node: INTEGER;
  BEGIN
    FOR node := 1 TO view.maxSize DO
      view.nodes[node] := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{FLOAT(node), FLOAT(vals[node])/2.0},
        color := PQueue.StartColor,
        fontColor := PQueue.Black,
        (* label := Fmt.Int(vals[node]), *)
        border := 0.005,
        borderColor := PQueue.Black,
        size := R2.T{1.0, FLOAT(vals[node])}).init();
    END;
    node := 1;
    FOR i := 1 TO view.lastLevel-1 DO
      EVAL view.tickMarks[i].init();
    END;
    view.mg.redisplay();
    Pause(view);
  END InitSort;

PROCEDURE HeapOpInit(view: T; k: INTEGER) =
  BEGIN
    LOCK view.mg.mu DO 
      view.nodes[k].setColor(PQueue.WorkColor);
    END;
    view.workNode := k;
    view.mg.redisplay();
  END HeapOpInit;

PROCEDURE HeapStep(view: T; k,n: INTEGER; <* UNUSED *> down: BOOLEAN) 
    RAISES {Thread.Alerted} =
  BEGIN
    ClearHighlights(view);
    SwapNodes(view, k, n);
    MakeCurve(view, k, n);
    view.mg.redisplay();
    view.mg.animate(0.0, 1.0);  
END HeapStep;

PROCEDURE MakeCurve(view: T; k, n: INTEGER) =
  CONST yCoord = -20.0 ;
  VAR
    bv0, bv1: GraphVBT.Vertex;
    e1: GraphVBT.Edge;
  BEGIN
    bv0 := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{FLOAT(k)+0.3*FLOAT(n-k), yCoord},
        size := R2.T{0.0, 0.0}).init();
    bv1 := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{FLOAT(k)+0.7*FLOAT(n-k), yCoord},
        size := R2.T{0.0, 0.0}).init();
    e1 := NEW(GraphVBT.Edge, vertex0 := view.axisNodes[k], 
          vertex1 := view.axisNodes[n], control0 := bv0,
          color := PQueue.WorkColor,
          control1 := bv1).init();
    view.arcs := RefList.Cons (e1, view.arcs);
  END MakeCurve;

PROCEDURE PlaceElement(view: T; k: INTEGER) =
  VAR e1: GraphVBT.Edge;
    v1: GraphVBT.Vertex;
  BEGIN
    ClearHighlights(view);
    LOCK view.mg.mu DO 
        view.nodes[k].setColor(PQueue.StartColor);
        WHILE RefList.Length(view.arcs) > 0 DO
          e1 := NARROW (view.arcs.head, GraphVBT.Edge);
          view.arcs := view.arcs.tail;
          v1 := e1.control1;
          e1.control0.remove();
          v1.remove();
        END;
    END;
    view.mg.redisplay();
  END PlaceElement;

PROCEDURE SortStep(view: T; k: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    LOCK view.mg.mu DO 
      view.nodes[1].setColor(PQueue.SortedColor);
    END; 
    SwapNodes(view, k, 1);
    view.mg.animate(0.0, 1.0);    
END SortStep;

PROCEDURE Insert(view: T; k: INTEGER) =
  BEGIN
    INC(view.size);
    view.nodes[view.size] := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{FLOAT(view.size), FLOAT(k)/2.0},
        color := PQueue.StartColor,
        fontColor := PQueue.Black,
        (* label := Fmt.Int(vals[node]), *)
        border := 0.005,
        borderColor := PQueue.Black,
        size := R2.T{1.0, FLOAT(k)}).init();
    IF view.size > view.lastInLevel[view.currentLevel] THEN
      EVAL view.tickMarks[view.currentLevel].init(); 
      INC(view.currentLevel);
    END;
    view.mg.redisplay();
  END Insert;

PROCEDURE Remove(view: T) RAISES {Thread.Alerted} =
  (* On entry, view.size > 0 *)
  VAR
  BEGIN
    LOCK view.mg.mu DO
      view.nodes[1].move(OffScreenPos, animated := TRUE);
      IF view.size > 1 THEN
        view.nodes[view.size].toFront();
        view.nodes[view.size].move(
            R2.T{1.0, view.nodes[view.size].pos[1]}, 
            animated := TRUE);
      END;
    END;
    IF view.size-1 = view.lastInLevel[view.currentLevel-1] THEN
      DEC(view.currentLevel);
      LOCK view.mg.mu DO view.tickMarks[view.currentLevel].remove() END;
    END;
    view.mg.animate(0.0, 1.0);
    LOCK view.mg.mu DO view.nodes[1].remove(); END;
    view.nodes[1] := view.nodes[view.size];
    DEC(view.size);
  END Remove;

PROCEDURE SwapNodes(view: T; k, n: INTEGER) =
  VAR
    saveX: REAL;
    saveNode: GraphVBT.Vertex;
  BEGIN
    IF view.workNode = k THEN view.workNode := n ELSE view.workNode := k END;
    LOCK view.mg.mu DO 
      saveX := view.nodes[k].pos[0];
      view.nodes[k].toFront();
      view.nodes[n].toFront();
      view.nodes[k].move(R2.T{view.nodes[n].pos[0], view.nodes[k].pos[1]},
              animated := TRUE);
      view.nodes[n].move(R2.T{saveX, view.nodes[n].pos[1]},
              animated := TRUE);
    END; 
    saveNode := view.nodes[k]; view.nodes[k] := view.nodes[n];
    view.nodes[n]:= saveNode;
  END SwapNodes;

PROCEDURE Pause(view:T) RAISES {Thread.Alerted} =
  VAR
    offScreenNode: GraphVBT.Vertex;
  BEGIN
    offScreenNode := NEW(GraphVBT.Vertex, graph := view.mg,
        pos := R2.T{-1000.0, -1000.0},
        size := R2.T{0.01, 0.01}).init();
    LOCK view.mg.mu DO
      offScreenNode.move(R2.T{-1001.0, -1001.0}, animated := TRUE);
    END;
    view.mg.animate(0.0, 1.0);
    LOCK view.mg.mu DO offScreenNode.remove(); END;
  END Pause;

PROCEDURE Compare(view: T; k: INTEGER; n: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
      ClearHighlights(view);
      view.hlt1 := view.highlights[k].init(); 
      IF n > 0 THEN
        view.hlt2 := view.highlights[n].init();
      END;
      view.hlt3 :=  view.highlights[view.workNode].init();
    view.mg.redisplay();
    Pause(view);
  END Compare;

PROCEDURE ClearHighlights(view: T) =
  BEGIN
    LOCK view.mg.mu DO
      view.hlt1.remove();
      view.hlt2.remove();
      view.hlt3.remove();
    END;
  END ClearHighlights;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(NEW(GraphVBT.T).init())
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Bar View", "PQueue");
END PQBarView.
