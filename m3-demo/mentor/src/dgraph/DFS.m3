(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE DFS;

IMPORT Algorithm, DGraphAlgClass, DGraphIE, Thread, ZeusPanel, 
       ReadGraph, RefList, ZeusCodeView, AdjMatrix;

CONST C = AdjMatrix.Column;

TYPE 
  T = DGraphAlgClass.T BRANDED OBJECT 
    OVERRIDES 
      run := Run; 
    END;

CONST
  Unseen = -1;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR 
    m: AdjMatrix.T;
    nVertices: INTEGER;
  BEGIN
    m := ReadGraph.In(alg);
    nVertices := m.nVertices();

    DGraphIE.Setup(alg, m);
    Search(alg, m);
  END Run;

PROCEDURE Search(alg: Algorithm.T; m: AdjMatrix.T) RAISES {Thread.Alerted} =
  VAR
    now: INTEGER := Unseen;
    V := m.nVertices();
    val := NEW(REF ARRAY OF INTEGER, V) <* TRACE TraceVal *> ;

  PROCEDURE TraceVal(name: TEXT; val: REF ARRAY OF INTEGER) RAISES {}=
    BEGIN
      IF val # NIL THEN alg.varView.setIntegerArray(name, val^); END;
    END TraceVal;

  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN 
      ZeusCodeView.Event(alg, line); 
    END At;

  PROCEDURE Visit(k: INTEGER) RAISES {Thread.Alerted} =
    VAR 
      pred := -1;
    BEGIN
      ZeusCodeView.Enter(alg, procedureName := "VISIT");
At(1);INC(now); val[k] := now;
                                        DGraphIE.MarkVertex(alg, k, 1, C);
At(2);FOR t := 0 TO V-1 DO
At(3);  IF m.getEdge(k, t) THEN
At(4);   IF val[t] = Unseen THEN   DGraphIE.MarkEdge(alg, k, t, 1);
                                         DGraphIE.AddChild(alg, k, pred, t,
                                                          m.name(t));
At(5);      Visit(t);                   pred := t;
                                        DGraphIE.UnMarkEdge(alg, k, t, 1);
          END; (* if *)
        END; (* if *)
      END; (* for *)                    DGraphIE.MarkVertex(alg, k, 0, C);
                                        
      ZeusCodeView.Exit(alg);
    END Visit;


  BEGIN
    ZeusCodeView.Event(alg, procedureName := "DFS");
At(6);FOR k := 0 TO V-1 DO val[k] := Unseen; END;
At(7);FOR k := 0 TO V-1 DO
At(8);IF val[k] = Unseen THEN       DGraphIE.NewTree(alg, k, m.name(k));
At(9);  Visit(k)
      END;
At(10);
    END;
  END Search;

PROCEDURE New(): Algorithm.T RAISES {}=
  VAR fv := ZeusPanel.NewForm("DGraphinput.fv");
  BEGIN
    WITH cv = RefList.List1(
                  RefList.List2("Modula-3 Code View", "DFS.m3.cv")) DO
      RETURN NEW(T, data := fv, codeViews := cv,
                 varRsrc := "DFSVar.fv").init() 
    END;   
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "Depth First Search", "DGraph");
END DFS.
