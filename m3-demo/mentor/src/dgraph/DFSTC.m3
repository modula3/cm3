(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE DFSTC;

IMPORT Algorithm, DGraphAlgClass, DGraphIE, Thread, ZeusPanel, ReadGraph, 
       RefList, ZeusCodeView, AdjMatrix;

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

    ZeusCodeView.Event(alg, procedureName := "DFS");
    DGraphIE.Setup(alg, m);
    TC(alg, m);
  END Run;

PROCEDURE TC(alg: Algorithm.T; m: AdjMatrix.T) RAISES {Thread.Alerted}=

  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN 
      ZeusCodeView.Event(alg, line); 
    END At;

  VAR
    now: INTEGER := Unseen;
    V := m.nVertices();
    val := NEW(REF ARRAY OF INTEGER, V);
    current: INTEGER;

  PROCEDURE Visit(k: INTEGER) RAISES {Thread.Alerted}=
    BEGIN
      ZeusCodeView.Enter(alg, procedureName := "VISIT");
At(1);INC(now); val[k] := now;          
At(2);m.putEdge(current, k, TRUE);      DGraphIE.AddEdge(alg, current, k);
At(3);FOR t := 0 TO V-1 DO
At(4);  IF m.getEdge(k, t) THEN
At(5);   IF val[t] = Unseen THEN        DGraphIE.MarkEdge(alg, k, t, 0);
At(6);      Visit(t);
                                        DGraphIE.UnMarkEdge(alg, k, t, 0);
          END; (* if *)
        END; (* if *)                
      END; (* for *)
      ZeusCodeView.Exit(alg);
    END Visit;


  BEGIN
    ZeusCodeView.Event(alg, procedureName := "DFSTC");
At(7);FOR k := 0 TO V-1 DO                DGraphIE.MarkVertex(alg, k, 0, C);
At(8);now := Unseen;                      current := k;
At(9);FOR j := 0 TO V-1 DO val[j] := Unseen; END;
At(10);Visit(k)
    END;
  END TC;

PROCEDURE New(): Algorithm.T RAISES {}=
  VAR fv := ZeusPanel.NewForm("DGraphinput.fv");
  BEGIN
    WITH cv = RefList.List1(
                  RefList.List2("Modula-3 Code View", "DFSTC.m3.cv")) DO
      RETURN NEW(T, data := fv, codeViews := cv).init() 
    END;   
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "Transitive Closure (DFS)", "DGraph");
END DFSTC.
