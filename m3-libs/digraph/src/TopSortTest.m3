(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(* Last modified on Thu Aug 22 16:30:56 PDT 1996 by detlefs     *)

MODULE TopSortTest EXPORTS Main;

IMPORT IntIntDiGraph;
IMPORT Wr, Stdio, Fmt;

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted, IntIntDiGraph.DupNode,
        IntIntDiGraph.NoSuchNode, IntIntDiGraph.DupEdge *>

VAR dgn := NEW(IntIntDiGraph.T).init();

PROCEDURE DoTopSort() =
  VAR rai: REF ARRAY OF INTEGER; BEGIN
    IF dgn.topSort(rai) THEN
      Wr.PutText(Stdio.stdout, "No cycle: ");
    ELSE
      Wr.PutText(Stdio.stdout, "Cycle: ")
    END (* IF *);
    FOR i := 0 TO LAST(rai^) DO
      Wr.PutText(Stdio.stdout, Fmt.Int(rai[i]) & "   ")
    END (* FOR *);
    Wr.PutText(Stdio.stdout, "\n")
  END DoTopSort;

BEGIN
  Wr.PutText(Stdio.stdout, "Should have no cycles:\n\n");
  dgn.addNode(1);
  dgn.addNode(2);
  dgn.addNode(3);
  dgn.addEdge(1, 0, 2);
  dgn.addEdge(1, 0, 3);
  dgn.addEdge(3, 0, 2);
  DoTopSort();

  dgn := NEW(IntIntDiGraph.T).init();
  dgn.addNode(1);
  dgn.addNode(2);
  dgn.addNode(3);
  dgn.addNode(4);
  dgn.addNode(5);
  dgn.addNode(6);
  dgn.addNode(7);
  dgn.addNode(8);
  dgn.addEdge(1, 0, 4);
  dgn.addEdge(2, 0, 4);
  dgn.addEdge(2, 0, 5);
  dgn.addEdge(3, 0, 5);
  dgn.addEdge(3, 0, 8);
  dgn.addEdge(4, 0, 5);
  dgn.addEdge(4, 0, 6);
  dgn.addEdge(4, 0, 7);
  dgn.addEdge(5, 0, 7);
  dgn.addEdge(5, 0, 8);
  dgn.addEdge(7, 0, 8);
  DoTopSort();

  Wr.PutText(Stdio.stdout, "\n\nShould have cycles:\n\n");
  dgn := NEW(IntIntDiGraph.T).init();
  dgn.addNode(1);
  dgn.addNode(2);
  dgn.addNode(3);
  dgn.addEdge(3, 0, 3);
  DoTopSort();

  dgn := NEW(IntIntDiGraph.T).init();
  dgn.addNode(1);
  dgn.addNode(2);
  dgn.addNode(3);
  dgn.addEdge(1, 0, 2);
  dgn.addEdge(2, 0, 3);
  dgn.addEdge(3, 0, 1);
  DoTopSort();

  dgn := NEW(IntIntDiGraph.T).init();
  dgn.addNode(10);
  dgn.addNode(1);
  dgn.addNode(2);
  dgn.addNode(3);
  dgn.addEdge(10, 0, 1);
  dgn.addEdge(1, 0, 2);
  dgn.addEdge(2, 0, 3);
  dgn.addEdge(3, 0, 1);
  DoTopSort();
END TopSortTest.
      

