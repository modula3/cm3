(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AdjMatrixView01_K;

IMPORT DGraphViewClass, PaintOp, AdjMatrixVBT, View, ZeusPanel,
       DepthToColor, Filter, TextVBT, AdjMatrix;


TYPE
 T = DGraphViewClass.T BRANDED OBJECT
    x: AdjMatrixVBT.T;
    markEdgeSave: MarkEdgeSave;
    markVertexSave: MarkVertexSave;
  OVERRIDES
    oeSetup := Setup;
    oeAddEdge := AddEdge;
    oeMarkEdge := MarkEdge;
    oeMarkVertex := MarkVertex;
    oeUnMarkEdge := UnMarkEdge;
    oeUnMarkVertex := UnMarkVertex;
  END; (* object *)

  MarkEdgeSave = REF ARRAY OF RECORD from, to: INTEGER; color: PaintOp.T END;
  MarkVertexSave = REF ARRAY OF RECORD
    index: INTEGER; row_color, col_color: PaintOp.T END;

PROCEDURE Setup(t: T; m: AdjMatrix.T) RAISES {}=
  VAR nVertices := m.nVertices();
  BEGIN
    WITH x = NEW(AdjMatrixVBT.T).init(nVertices, grid := FALSE) DO
      t.x := x;
      EVAL Filter.Replace(t, x);
      x.display(FALSE);
      FOR i := 0 TO nVertices-1 DO
        FOR j := 0 TO nVertices-1 DO
          x.setText(i, j, "0");
        END; (* for *)
      END; (* for *)
      VAR i, j: INTEGER;
      BEGIN
        WITH iter = m.edgeIter() DO
          WHILE iter.next(i, j) DO
            AddEdge(t, i, j);
          END; (* while *)
        END; (* with *)
      END;
      x.display(TRUE);
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
    END;
  END Setup;

PROCEDURE AddEdge(t: T; from, to: INTEGER) RAISES {}=
  BEGIN
    t.x.setText(from, to, "1");
  END AddEdge;

PROCEDURE MarkEdge(t: T; from, to: INTEGER; depth: INTEGER := 0) =
  BEGIN
     WITH entry = t.markEdgeSave[depth] DO
       entry.from := from; entry.to := to;
       entry.color := t.x.getHighlight(from, to);
       t.x.setHighlight(from, to, DepthToColor.Map(depth));
     END;
  END MarkEdge;

PROCEDURE UnMarkEdge(t: T; from, to: INTEGER; depth: INTEGER := 0) =
  BEGIN
    IF depth = 1 THEN 
      (* retain *)
      t.x.setColor(from, to, DepthToColor.Map(depth));
    ELSE
     WITH entry = t.markEdgeSave[depth] DO
       IF entry.from >= 0 THEN
         t.x.setHighlight(entry.from, entry.to, entry.color);
         entry.from := -1;
       END; (* if *)
     END;
    END;
  END UnMarkEdge;

PROCEDURE MarkVertex(t: T; v: INTEGER; depth: INTEGER;
                       rcset: AdjMatrix.RCSet;) RAISES {}=
  BEGIN
     WITH entry = t.markVertexSave[depth] DO
       entry.index := v;
       IF AdjMatrix.RC.Row IN rcset THEN
         entry.row_color := t.x.getRowLabel(v);
         t.x.setRowLabel(v, DepthToColor.Map(depth));
       END;
       IF AdjMatrix.RC.Column IN rcset THEN
         entry.col_color := t.x.getColumnLabel(v);
         t.x.setColumnLabel(v, DepthToColor.Map(depth));
       END;
     END;
  END MarkVertex;

PROCEDURE UnMarkVertex(t: T; v: INTEGER; depth: INTEGER;
                       rcset: AdjMatrix.RCSet) RAISES {}=
  BEGIN
     WITH entry = t.markVertexSave[depth] DO
       IF entry.index >= 0 THEN
         IF AdjMatrix.RC.Row IN rcset THEN 
           t.x.setRowLabel(v, entry.row_color);
         END;
         IF AdjMatrix.RC.Column IN rcset THEN 
           t.x.setColumnLabel(v, entry.col_color);
         END;
         entry.index := -1;
       END; (* if *)
     END;
  END UnMarkVertex;


PROCEDURE New(): View.T RAISES {}=
  VAR dummy := TextVBT.New("Adjacency Matrix [01]");
  BEGIN
    RETURN NEW(T).init(dummy);
  END New;


BEGIN
  ZeusPanel.RegisterView (New, "Adjacency Matrix [01] ToDo", "DGraph");
END AdjMatrixView01_K.
