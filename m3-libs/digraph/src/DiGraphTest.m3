(* Copyright (C) 1996, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(* Last modified on Thu Aug 22 16:30:35 PDT 1996 by detlefs     *)

MODULE DiGraphTest EXPORTS Main;

IMPORT IntIntDiGraph, NullEdgeType, IntNullDiGraph, Wr, Fmt;
FROM Stdio IMPORT stdout;

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted*>


PROCEDURE Error(n, m: INTEGER; msg: TEXT := NIL) =
  BEGIN
    IF msg = NIL THEN
      Wr.PutText(stdout,
                 "ERROR in test " & Fmt.Int(n) & "." &
                 Fmt.Int(m) & "\n");
    ELSE
      Wr.PutText(stdout,
                 "ERROR in test " & Fmt.Int(n) & "." &
                 Fmt.Int(m) & ": " & msg & "\n");
    END;
    Wr.Flush(stdout);
    <*ASSERT FALSE*>
  END Error;

PROCEDURE PrintInt(wr: Wr.T; i: INTEGER; width: CARDINAL) =
  BEGIN
    Wr.PutText(wr, Fmt.Pad(Fmt.Int(i), width));
  END PrintInt;

PROCEDURE PrintX(wr: Wr.T; edgeExists: BOOLEAN;
                 <*UNUSED*> edge: NullEdgeType.T; width: CARDINAL) =
  VAR t: TEXT;
  BEGIN
    IF edgeExists THEN t := "X" ELSE t := "" END;
    Wr.PutText(wr, Fmt.Pad(t, width));
  END PrintX;

VAR
  dg: IntIntDiGraph.T;
  dgn: IntNullDiGraph.T;
  k: CARDINAL;
  ni: IntIntDiGraph.NodeIter;
  next: INTEGER;
  nk, ek: INTEGER;

BEGIN
  Wr.PutText(stdout, "Starting test...");
  Wr.Flush(stdout);

  (* Make a new, empty graph and verify that it is empty. *)
  dg := NEW(IntIntDiGraph.T).init();
  IF dg.nodeSize() # 0 THEN Error(1, 1); END;
  IF dg.edgeSize() # 0 THEN Error(1, 2); END;
  IF dg.nodeExists(1) THEN Error(1, 3); END;
  TRY dg.deleteNode(1); Error(1, 4, "No exception"); EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE Error(1, 4, "Wrong exception");
  END;
  IF dg.edgeExists(1, 2) THEN Error(1, 5); END;
  TRY dg.deleteEdge(1, 2); Error(1, 6, "No exception") EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE Error(1, 6, "Wrong exception");
  END;
  TRY
    k := dg.nSucc(1);
    Error(1, 7, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 7, "Wrong exception");
  END;
  TRY
    nk := dg.getSuccN(1, 0);
    Error(1, 8, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 8, "Wrong exception");
  END;
  TRY
    ni := dg.getSuccIter(1);
    Error(1, 9, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 9, "Wrong exception");
  END;
  TRY
    k := dg.nPred(1);
    Error(1, 10, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 10, "Wrong exception");
  END;
  TRY
    nk := dg.getSuccN(1, 0);
    Error(1, 11, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 11, "Wrong exception");
  END;
  TRY
    ni := dg.getSuccIter(1);
    Error(1, 12, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE
    Error(1, 12, "Wrong exception");
  END;

  (* Now add two nodes and an edge between them and see if things look
     right. *)
  TRY dg.addNode(1); EXCEPT ELSE Error(2, 0) END (* TRY *);
  TRY dg.addNode(1); Error(2, 1, "No exception") EXCEPT
  | IntIntDiGraph.DupNode =>
  ELSE Error(2, 1, "Wrong exception");
  END;
  TRY
    dg.addNode(2);
    dg.addEdge(1, 100, 2);
  EXCEPT ELSE Error(2, 2, "Bad exception") END (* TRY *);
  IF dg.nodeSize() # 2 THEN Error(2, 2); END;
  IF dg.edgeSize() # 1 THEN Error(2, 3); END;
  IF NOT dg.nodeExists(1) THEN Error(2, 4) END;
  IF NOT dg.nodeExists(2) THEN Error(2, 5) END;
  IF dg.nodeExists(3) THEN Error(2, 6) END;
  IF NOT dg.edgeExists(1, 2) THEN Error(2, 7) END;
  IF dg.edgeExists(2, 1) THEN Error(2, 8) END;
  TRY
    IF dg.edgeValue(1, 2) # 100 THEN Error(2, 9) END;
  EXCEPT ELSE Error(2, 9, "Exception.") END (* TRY *);
  TRY
    ek := dg.edgeValue(2, 1);
    Error(2, 10, "No exception");
  EXCEPT
  | IntIntDiGraph.NoSuchEdge =>
  ELSE
    Error(1, 10, "Wrong exception");
  END;
  TRY
    IF dg.nPred(1) # 0 THEN Error(2, 11) END;
    IF dg.nSucc(1) # 1 THEN Error(2, 12) END;
    IF dg.nSucc(2) # 0 THEN Error(2, 13) END;
    IF dg.nPred(2) # 1 THEN Error(2, 14) END;
    IF dg.getSuccN(1, 0) # 2 THEN Error(2, 15) END;
    IF dg.getPredN(2, 0) # 1 THEN Error(2, 16) END;
  EXCEPT ELSE Error(2, 16, "Bad exception") END (* TRY *);
  TRY
    nk := dg.getSuccN(1, 1);
    Error(2, 17, "No exception");
  EXCEPT
  | IntIntDiGraph.RangeFault =>
  ELSE
    Error(2, 17, "Wrong exception");
  END;
  TRY
    nk := dg.getPredN(1, 0);
    Error(2, 18, "No exception");
  EXCEPT
  | IntIntDiGraph.RangeFault =>
  ELSE
    Error(2, 18, "Wrong exception");
  END;
  TRY
    nk := dg.getSuccN(2, 0);
    Error(2, 19, "No exception");
  EXCEPT
  | IntIntDiGraph.RangeFault =>
  ELSE
    Error(2, 19, "Wrong exception");
  END;
  TRY
    nk := dg.getPredN(2, 1);
    Error(2, 20, "No exception");
  EXCEPT
  | IntIntDiGraph.RangeFault =>
  ELSE
    Error(2, 20, "Wrong exception");
  END;
  TRY ni := dg.getPredIter(1) EXCEPT ELSE Error(2, 20, "Bad exception.") END;
  IF ni.next(next) THEN Error(2, 21) END;
  TRY ni := dg.getSuccIter(2) EXCEPT ELSE Error(2, 21, "Bad exception.") END;
  IF ni.next(next) THEN Error(2, 22) END;
  TRY ni := dg.getSuccIter(1) EXCEPT ELSE Error(2, 25, "Bad exception.") END;
  IF NOT ni.next(next) THEN Error(2, 23) END;
  IF next # 2 THEN Error(2, 24) END;
  IF ni.next(next) THEN Error(2, 25) END;
  TRY ni := dg.getPredIter(2) EXCEPT ELSE Error(2, 25, "Bad exception.") END;
  IF NOT ni.next(next) THEN Error(2, 26) END;
  IF next # 1 THEN Error(2, 27) END;
  IF ni.next(next) THEN Error(2, 28) END;

  (* Let's start fresh, and make

   TO   1  2  3  4  5

F   1      X  X  X
R   2            X  X
O   3               X
M   4               X

    We'll delete some edges and make sure they go away, then
    delete a node (2) and make sure it and the incident edges go away.
  *)
  dg := NEW(IntIntDiGraph.T).init();
  TRY
    dg.addNode(1);
    dg.addNode(2);
    dg.addNode(3);
    dg.addNode(4);
    dg.addNode(5);
    dg.addEdge(1, 0, 2);
    dg.addEdge(1, 0, 3);
    dg.addEdge(1, 0, 4);
    dg.addEdge(2, 0, 4);
    dg.addEdge(2, 0, 5);
    dg.addEdge(3, 0, 5);
    dg.addEdge(4, 0, 5);
  EXCEPT
  ELSE
    Error(3, 0);
  END (* TRY *);

  IF dg.nodeSize() # 5 THEN Error(3, 1) END;    
  IF dg.edgeSize() # 7 THEN Error(3, 2) END;
  IF NOT dg.edgeExists(1, 3) THEN Error(3, 3) END;
  (* Delete an edge. *)
  TRY dg.deleteEdge(1, 3);
  EXCEPT ELSE Error(3, 3, "Bad exception") END (* TRY *);
  IF dg.edgeExists(1, 3) THEN Error(3, 4) END;
  IF dg.edgeSize() # 6 THEN Error(3, 5) END;
  TRY
    IF dg.nSucc(1) # 2 THEN Error(3, 6) END;
    IF dg.nPred(3) # 0 THEN Error(3, 7) END;
    dg.addEdge(1, 0, 3);
  EXCEPT ELSE Error(3, 7, "Bad exception.") END (* TRY *);
  (* Exercise the exceptions of DeleteEdge. *)
  TRY dg.deleteEdge(6, 1); Error(3, 8, "No exception") EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE Error(3, 8, "Wrong exception")
  END;
  TRY dg.deleteEdge(3, 1); Error(3, 9, "No exception") EXCEPT
  | IntIntDiGraph.NoSuchEdge =>
  ELSE Error(3, 9, "Wrong exception")
  END;
  IF NOT dg.edgeExists(1, 3) THEN Error(3, 10) END;
  (* Exercise changing an edge value. *)
  TRY
    IF dg.edgeValue(1, 3) # 0 THEN Error(3, 11) END;
    dg.changeEdge(1, 100, 3);
    IF dg.edgeValue(1, 3) # 100 THEN Error(3, 12) END;
  EXCEPT ELSE Error(3, 12, "Bad exception.") END (* TRY *);
  (* Now delete a node. *)
  TRY dg.deleteNode(6); Error(3, 13, "No exception") EXCEPT
  | IntIntDiGraph.NoSuchNode =>
  ELSE Error(3, 13, "Wrong exception")
  END;
  TRY dg.deleteNode(2) EXCEPT ELSE Error(3, 13, "Bad exception") END (* TRY *);
  IF dg.nodeSize() # 4 THEN Error(3, 14) END;
  IF dg.edgeSize() # 4 THEN Error(3, 15) END;
  TRY
    IF dg.nSucc(1) # 2 THEN Error(3, 16) END;
    IF dg.nPred(5) # 2 THEN Error(3, 17) END;
  EXCEPT ELSE Error(3, 18) END (* TRY *);

  (* Let's start with a fairly sparse graph, and run transitive closure.

    TO  1  2  3  4  5  6  7  8  9

    1      X
    2         X
F   3            X           X
R   4               X
O   5                  X
M   6                     X
    7
    8                           X
    9                  X

    We expect the result to be

    TO  1  2  3  4  5  6  7  8  9

    1      X  X  X  X  X  X  X  X
    2         X  X  X  X  X  X  X
F   3            X  X  X  X  X  X
R   4               X  X  X
O   5                  X  X
M   6                     X
    7
    8                  X  X     X
    9                  X  X

  *)

  dgn := NEW(IntNullDiGraph.T).init(NullEdgeType.cyclesOK);
  TRY
    dgn.addNode(1);
    dgn.addNode(2);
    dgn.addNode(3);
    dgn.addNode(4);
    dgn.addNode(5);
    dgn.addNode(6);
    dgn.addNode(7);
    dgn.addNode(8);
    dgn.addNode(9);
    dgn.addEdge(1, NullEdgeType.T.Present, 2);
    dgn.addEdge(2, NullEdgeType.T.Present, 3);
    dgn.addEdge(3, NullEdgeType.T.Present, 4);
    dgn.addEdge(3, NullEdgeType.T.Present, 8);
    dgn.addEdge(4, NullEdgeType.T.Present, 5);
    dgn.addEdge(5, NullEdgeType.T.Present, 6);
    dgn.addEdge(6, NullEdgeType.T.Present, 7);
    dgn.addEdge(8, NullEdgeType.T.Present, 9);
    dgn.addEdge(9, NullEdgeType.T.Present, 6);
  EXCEPT ELSE Error(4, 0) END (* TRY *);
  IF dgn.nodeSize() # 9 THEN Error(4, 1) END;    
  IF dgn.edgeSize() # 9 THEN Error(4, 2) END;

  Wr.PutText(stdout, "Before TC:\n\n");
  dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);

  VAR b := dgn.transitiveClose(); BEGIN
    <*ASSERT b*>
  END (* BEGIN *);

  Wr.PutText(stdout, "\nAfter TC:\n\n");
  dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);

  IF dgn.nodeSize() # 9 THEN Error(4, 3) END;    
  IF dgn.edgeSize() # 32 THEN Error(4, 4) END;
  (* Sample a few. *)
  IF NOT dgn.edgeExists(1, 7) THEN Error(4, 5) END;
  IF NOT dgn.edgeExists(1, 4) THEN Error(4, 6) END;
  IF NOT dgn.edgeExists(2, 3) THEN Error(4, 7) END;
  IF NOT dgn.edgeExists(4, 6) THEN Error(4, 8) END;
  IF NOT dgn.edgeExists(3, 9) THEN Error(4, 9) END;
  IF NOT dgn.edgeExists(8, 7) THEN Error(4, 10) END;
  IF dgn.edgeExists(7, 1) THEN Error(4, 11) END;
  IF dgn.edgeExists(8, 3) THEN Error(4, 12) END;
  IF dgn.edgeExists(2, 2) THEN Error(4, 13) END;

  (* Now try again using incremental closure. *)
  dgn := NEW(IntNullDiGraph.T).init(NullEdgeType.cyclesNO);
  VAR b: BOOLEAN; BEGIN
    TRY
      dgn.addNode(1);
      dgn.addNode(2);
      dgn.addNode(3);
      dgn.addNode(4);
      dgn.addNode(5);
      dgn.addNode(6);
      dgn.addNode(7);
      dgn.addNode(8);
      dgn.addNode(9);
      b := dgn.addEdgeAndClose(1, NullEdgeType.T.Present, 2);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(2, NullEdgeType.T.Present, 3);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(3, NullEdgeType.T.Present, 4);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(3, NullEdgeType.T.Present, 8);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(4, NullEdgeType.T.Present, 5);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(5, NullEdgeType.T.Present, 6);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(6, NullEdgeType.T.Present, 7);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(8, NullEdgeType.T.Present, 9);
      <*ASSERT b *>
      b := dgn.addEdgeAndClose(9, NullEdgeType.T.Present, 6);
      <*ASSERT b *>
    EXCEPT ELSE Error(5, 0) END (* TRY *);
  END (* BEGIN *);
  Wr.PutText(stdout, "\nAfter Incremental TC:\n\n");
  dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);

  (* Now verify that we detect cycles. *)
  VAR b := dgn.addEdgeAndClose(6, NullEdgeType.T.Present, 4); BEGIN
    <*ASSERT NOT b*>
  END (* BEGIN *);

  (* Now test undoing. *)
  dgn := NEW(IntNullDiGraph.T).init(NullEdgeType.cyclesNO, undoable := TRUE);
  TRY
    dgn.addNode(1);
    dgn.addNode(2);
    dgn.addNode(3);
    dgn.addNode(4);
    dgn.addNode(5);
    dgn.addNode(6);
    dgn.addNode(7);
    dgn.addNode(8);
    dgn.addEdge(1, NullEdgeType.T.Present, 2);
    dgn.addEdge(2, NullEdgeType.T.Present, 3);
    dgn.addEdge(3, NullEdgeType.T.Present, 4);
    dgn.addEdge(3, NullEdgeType.T.Present, 8);
    dgn.push();
    Wr.PutText(stdout, "\nAdded 8 nodes, 4 edges:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.addNode(9);
    dgn.addEdge(4, NullEdgeType.T.Present, 5);
    dgn.addEdge(5, NullEdgeType.T.Present, 6);
    dgn.addEdge(6, NullEdgeType.T.Present, 7);
    dgn.addEdge(8, NullEdgeType.T.Present, 9);
    dgn.addEdge(9, NullEdgeType.T.Present, 6);
    Wr.PutText(stdout, "\nAdded 9 nodes, 9 edges:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.pop();
    Wr.PutText(stdout, "\nAfter pop(8, 4):\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.push();
    dgn.addNode(9);
    dgn.addEdge(4, NullEdgeType.T.Present, 5);
    dgn.addEdge(5, NullEdgeType.T.Present, 6);
    dgn.addEdge(6, NullEdgeType.T.Present, 7);
    dgn.addEdge(8, NullEdgeType.T.Present, 9);
    dgn.addEdge(9, NullEdgeType.T.Present, 6);
    Wr.PutText(stdout, "\n9 nodes, 9 edges:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.push();
    VAR b := dgn.transitiveClose(); BEGIN
      <*ASSERT b*>
    END (* BEGIN *);
    Wr.PutText(stdout, "\nAfter transitive closure:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.pop();
    Wr.PutText(stdout, "\nAfter pop(9, 9):\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.pop();
    Wr.PutText(stdout, "\nAfter pop(8, 4):\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.push();
    VAR b := dgn.transitiveClose(); BEGIN
      <*ASSERT b*>
    END;
    Wr.PutText(stdout, "\nAfter Transitive-closure:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.addNode(9);
    VAR b: BOOLEAN; BEGIN
      b := dgn.addEdgeAndClose(4, NullEdgeType.T.Present, 5);
      <*ASSERT b*>
      b := dgn.addEdgeAndClose(5, NullEdgeType.T.Present, 6);
      <*ASSERT b*>
      b := dgn.addEdgeAndClose(6, NullEdgeType.T.Present, 7);
      <*ASSERT b*>
      b := dgn.addEdgeAndClose(8, NullEdgeType.T.Present, 9);
      <*ASSERT b*>
      b := dgn.addEdgeAndClose(9, NullEdgeType.T.Present, 6);
      <*ASSERT b*>
    END (* BEGIN *);    
    Wr.PutText(stdout, "\nAfter adding edges and closing:\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
    dgn.pop();
    Wr.PutText(stdout, "\nAfter pop(8, 4):\n\n");
    dgn.printAsMatrix(stdout, PrintInt, PrintX, 2, 4, NullEdgeType.T.Absent);
  EXCEPT ELSE Error(7, 0) END (* TRY *);
  
END DiGraphTest.
