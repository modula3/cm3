(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE GraphView;

IMPORT DGraphViewClass, PaintOp, GraphVBT, View, ZeusPanel,
       DepthToColor, Filter, TextVBT, AdjMatrix, R2;

TYPE
 TI = DGraphViewClass.T BRANDED OBJECT
    g: GraphVBT.T;
    v: REF ARRAY OF GraphVBT.Vertex;
  OVERRIDES
    oeSetup := SetupT1;
  END;

  T = TI OBJECT
    markEdgeSave: MarkEdgeSave;
    markVertexSave: MarkVertexSave;
  OVERRIDES
    oeSetup := SetupT;
    oeAddEdge := AddEdge;
    oeMarkEdge := MarkEdge;
    oeMarkVertex := MarkVertex;
    oeUnMarkEdge := UnMarkEdge;
    oeUnMarkVertex := UnMarkVertex;
  END; (* object *)

  MarkEdgeSave = REF ARRAY OF RECORD from, to: INTEGER; color: PaintOp.T END;
  MarkVertexSave = REF ARRAY OF RECORD index: INTEGER; color: PaintOp.T END;

  MyVertex = GraphVBT.Vertex OBJECT v: INTEGER END;

PROCEDURE SetupT1(t: TI; m: AdjMatrix.T) RAISES {}=
  VAR nVertices := m.nVertices();
  BEGIN
    t.g := NEW(GraphVBT.T).init();
    EVAL Filter.Replace(t, t.g);
    t.v := NEW(REF ARRAY OF GraphVBT.Vertex, nVertices);
    FOR i := 0 TO nVertices-1 DO
      WITH coord = m.coord(i) DO
        t.v[i] := NEW(MyVertex, graph := t.g,
                      pos := coord,
                      color := PaintOp.Bg,
                      shape := GraphVBT.VertexShape.Ellipse,
                      size := R2.T{0.075,0.075},
                      label := m.name(i),
                      fontColor := PaintOp.Fg,
                      border := 0.005,
                      v := i).init();
      END;
    END; (* for *)
    VAR i, j: INTEGER;
     BEGIN
      WITH iter = m.edgeIter() DO
        WHILE iter.next(i, j) DO
          AddEdge(t, i, j);
        END; (* while *)
      END; (* with *)
    END;
    t.g.redisplay();
  END SetupT1;

PROCEDURE SetupT(t: T; m: AdjMatrix.T) RAISES {}=
  VAR nVertices := m.nVertices();
  BEGIN
    SetupT1(t, m);
    t.markEdgeSave := NEW(MarkEdgeSave, nVertices);
    t.markVertexSave := NEW(MarkVertexSave, nVertices);
    FOR i := 0 TO nVertices-1 DO
      WITH entry = t.markEdgeSave[i] DO
        entry.from := -1; entry.to := -1;
      END;
      WITH entry = t.markVertexSave[i] DO
        entry.index := -1;
      END;
    END; (* for *)
  END SetupT;

PROCEDURE AddEdge(t: TI; i, j: INTEGER) RAISES {}=
  VAR edge: GraphVBT.Edge;
  BEGIN
    LOCK t.g.mu DO
      edge := FindEdge(t, i, j);
    END;
    IF edge = NIL THEN
      edge := NEW(GraphVBT.Edge,
                vertex0 := t.v[i], vertex1 := t.v[j],
                arrow := ARRAY [0 .. 1] OF BOOLEAN{FALSE, TRUE},
                color := DepthToColor.Map(-1)).init();
      t.g.redisplay();
    END;
  END AddEdge;

PROCEDURE MarkEdge(t: T; i, j: INTEGER; depth: INTEGER) RAISES {}=
  BEGIN
    LOCK t.g.mu DO
      WITH edge = FindEdge(t, i, j) DO
        IF edge # NIL THEN
          WITH entry = t.markEdgeSave[depth] DO
            entry.from := i; entry.to := j;
            entry.color := edge.color;
            edge.setColor(DepthToColor.Map(depth));
          END;
        END;
      END;
    END; (* lock *)
    t.g.redisplay();
  END MarkEdge;

PROCEDURE UnMarkEdge(t: T; i, j: INTEGER; depth: INTEGER) RAISES {}=
  BEGIN
    LOCK t.g.mu DO
      WITH edge = FindEdge(t, i, j) DO
        IF edge # NIL THEN
          WITH entry = t.markEdgeSave[depth] DO
            IF entry.from >= 0 THEN
              edge.setColor(entry.color);
            END;
          END;
        END;
      END;
    END; (* lock *)
    t.g.redisplay();
  END UnMarkEdge;

PROCEDURE MarkVertex(t: T; i: INTEGER; depth: INTEGER;
                     <* UNUSED *> rcset: AdjMatrix.RCSet) RAISES {}=
  BEGIN
    LOCK t.g.mu DO
      WITH entry = t.markVertexSave[depth] DO
        entry.index := i;
        entry.color := t.v[i].color;
        t.v[i].setColor(DepthToColor.Map(depth));
      END;
    END; (* lock *)
    t.g.redisplay();
  END MarkVertex;

PROCEDURE UnMarkVertex(t: T; i: INTEGER; depth: INTEGER;
                       <* UNUSED *> rcset: AdjMatrix.RCSet) RAISES {}=
  BEGIN
    LOCK t.g.mu DO
      WITH entry = t.markVertexSave[depth] DO
        IF entry.index >= 0 THEN
          t.v[i].setColor(entry.color);
        END;
      END;
    END; (* lock *)
    t.g.redisplay();
  END UnMarkVertex;

PROCEDURE FindEdge(t: TI; i, j: INTEGER): GraphVBT.Edge RAISES {}=
  VAR edgeList := t.v[i].edges;
  BEGIN
    WHILE edgeList # NIL DO
      VAR edge := NARROW (edgeList.head, GraphVBT.Edge);
      BEGIN
        IF edge.vertex0 = t.v[i] AND edge.vertex1 = t.v[j] THEN
          RETURN edge
        END;
      END;
      edgeList := edgeList.tail;
    END; (* while *)
    RETURN NIL; 
  END FindEdge;

PROCEDURE NewT(): View.T RAISES {}=
  VAR dummy := TextVBT.New("Graph");
  BEGIN
    RETURN NEW(T).init(dummy);
  END NewT;

PROCEDURE NewTI(): View.T RAISES {}=
  VAR dummy := TextVBT.New("Initial Graph");
  BEGIN
    RETURN NEW(TI).init(dummy);
  END NewTI;


BEGIN
  ZeusPanel.RegisterView (NewTI, "Initial Graph", "DGraph");
  ZeusPanel.RegisterView (NewT, "Graph", "DGraph");
END GraphView.
