(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE Warshall;

IMPORT Algorithm, DGraphAlgClass, DGraphIE, Thread, RefList,
       ZeusPanel, ZeusCodeView, ReadGraph, AdjMatrix;

CONST C = AdjMatrix.Column; R = AdjMatrix.Row;

TYPE 
  T = DGraphAlgClass.T BRANDED OBJECT 
    OVERRIDES 
      run := Run; 
    END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR 
    m: AdjMatrix.T;
    nVertices: INTEGER;
    xF, yF, jF := TRUE;

  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN
      ZeusCodeView.Event(alg, line); 
    END At;
  PROCEDURE AsNameX(var: TEXT; val: INTEGER) RAISES {}=
    BEGIN
      IF xF THEN xF := FALSE ELSE alg.varView.setText(var, m.name(val)); END;
    END AsNameX;
  PROCEDURE AsNameY(var: TEXT; val: INTEGER) RAISES {}=
    BEGIN
      IF yF THEN yF := FALSE ELSE alg.varView.setText(var, m.name(val)); END;
    END AsNameY;
  PROCEDURE AsNameJ(var: TEXT; val: INTEGER) RAISES {}=
    BEGIN
      IF jF THEN jF := FALSE ELSE alg.varView.setText(var, m.name(val)); END;
    END AsNameJ;

  BEGIN
    m := ReadGraph.In(alg);
    IF m = NIL THEN
      RETURN;
    END;

    nVertices := m.nVertices();

    ZeusCodeView.Event(alg, procedureName := "Warshall");
    DGraphIE.Setup(alg, m);

At(1);FOR y <*TRACE AsNameY*> := 0 TO nVertices-1 DO
                                         DGraphIE.MarkVertex(alg, y, 0, C);
At(2);FOR x <*TRACE AsNameX*> := 0 TO nVertices-1 DO
                               
                                         DGraphIE.MarkEdge(alg, x, y, 1);
At(3);  IF m.getEdge(x, y) THEN          DGraphIE.MarkVertex(alg, x, 1, R);
                                         DGraphIE.MarkVertex(alg, y, 2, R);
At(4);    FOR j <*TRACE AsNameJ*> := 0 TO nVertices-1 DO
                                         DGraphIE.MarkEdge(alg, y, j, 2);
At(5);      IF m.getEdge(y, j) THEN      
At(6);        m.putEdge(x, j, TRUE);     DGraphIE.AddEdge(alg, x, j);
            END; (* if *)                DGraphIE.UnMarkEdge(alg, y, j, 2);
          END; (* for j *)               DGraphIE.UnMarkVertex(alg, y, 2, R);
        END; (* if *)                    DGraphIE.UnMarkEdge(alg, x, y, 1);
                                         DGraphIE.UnMarkVertex(alg, x, 1, R);
      END; (* for x *)
    END; (* for y *)
  END Run;

PROCEDURE New(): Algorithm.T RAISES {}=
  VAR fv := ZeusPanel.NewForm("DGraphinput.fv");
  BEGIN
    WITH cv = RefList.List1 (
                  RefList.List2 ("Modula-3 Code View", "Warshall.m3.cv")) DO
      RETURN NEW(T, data := fv, codeViews := cv,
                 varRsrc := "WarshallVar.fv").init()    
    END; (* with *)
  END New;


BEGIN
  ZeusPanel.RegisterAlg(New, "Warshall", "DGraph");
END Warshall.
