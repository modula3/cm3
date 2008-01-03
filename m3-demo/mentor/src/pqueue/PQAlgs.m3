(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue Jan 31 15:40:29 PST 1995 by kalsow   *)
(*      modified on Thu Jan  5 23:27:41 PST 1995 by najork   *)
(*      modified on Thu Sep 24 12:51:49 PDT 1992 by mhb      *)
(*      modified on Tue Sep  8 21:04:03 PDT 1992 by johnh    *)
(*      modified on Fri Jul 31 18:13:26 PDT 1992 by owicki   *)

MODULE PQAlgs;

IMPORT Algorithm, PQueueAlgClass, PQueueIE, FormsVBT, Random, Thread,
       VBT, ZeusCodeView, ZeusPanel, RefList, PQueue;

FROM PQueue IMPORT PriorityQueue;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
TYPE 
  T = PQueueAlgClass.T BRANDED OBJECT 
      wq: PQueue.WorkQueue
    OVERRIDES 
      run := Run; 
    END;

PROCEDURE At(alg: T; line: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    ZeusCodeView.Event(alg, line)
  END At;

PROCEDURE Insert(alg: T;  pq: PriorityQueue; v: INTEGER) 
    RAISES {Thread.Alerted}=
  BEGIN
                IF pq.size < pq.maxSize THEN
                  ZeusCodeView.Event(alg, procedureName := "Insert");
At(alg, 101);     INC(pq.size); pq.heap[pq.size] := v; 
                  PQueueIE.Insert(alg, v);
                  IF pq.size > 1 THEN UpHeap(alg, pq, pq.size) END;
                  ZeusCodeView.Exit(alg);
                END;
  END Insert;

               PROCEDURE UpHeap(alg: T; pq: PriorityQueue; k:INTEGER) 
                   RAISES {Thread.Alerted}=
                 VAR v: INTEGER;
                 BEGIN
                 ZeusCodeView.Event(alg, procedureName := "UpHeap");
At(alg, 201);    v := pq.heap[k]; pq.heap[0] := LAST(INTEGER);
                 PQueueIE.HeapOpInit(alg, k);
At(alg, 202);    WHILE k > 1 DO
                   PQueueIE.Compare(alg, k DIV 2, 0);
                   IF pq.heap[k DIV 2] > v THEN EXIT END;
                   PQueueIE.HeapStep(alg, k, k DIV 2, FALSE);
                   pq.heap[k] := pq.heap[k DIV 2]; k := k DIV 2
                 END;
At(alg,204);     pq.heap[k] := v;
                 PQueueIE.PlaceElement(alg, k);
                 ZeusCodeView.Exit(alg);
  END UpHeap;

               PROCEDURE Remove(alg: T; pq: PriorityQueue): INTEGER 
                   RAISES {Thread.Alerted}=
               VAR outVal := pq.heap[1];
               BEGIN
                 ZeusCodeView.Event(alg, procedureName := "Remove");
                 IF pq.size > 0 THEN
At(alg, 301);      pq.heap[1] := pq.heap[pq.size]; DEC(pq.size);
                   PQueueIE.Remove(alg);
At(alg, 302);      IF pq.size > 1 THEN DownHeap(alg, pq, 1); END;
                   ZeusCodeView.Exit(alg);
At(alg, 303);      RETURN outVal;
                 ELSE
                   ZeusCodeView.Exit(alg);
                   RETURN 0 (* for now, so as not to crash *)
                 END;
               END Remove;

PROCEDURE DownHeap(alg: T; pq: PriorityQueue; k: INTEGER) 
    RAISES {Thread.Alerted}=
  VAR j, v: INTEGER;
               BEGIN
                 ZeusCodeView.Event(alg, procedureName := "DownHeap");
At(alg, 401);    v := pq.heap[k]; PQueueIE.HeapOpInit(alg,k);
                 WHILE k <= pq.size DIV 2 DO                 
At(alg,402);       j := k+k;
                   IF j+1 > pq.size THEN
                     PQueueIE.Compare(alg, j, 0);
                   ELSE
                     PQueueIE.Compare(alg, j, j+1);
                   END;
                  IF j < pq.size THEN
                                IF pq.heap[j] < pq.heap[j+1] THEN
                                   INC(j)
                    END;
                  END;
At(alg, 406);      IF v >= pq.heap[j] THEN
                     EXIT;
                   END;
At(alg, 408);      PQueueIE.HeapStep(alg, k,j, TRUE);
                       pq.heap[k] := pq.heap[j]; k := j;
                 END;
At(alg, 409);    pq.heap[k] := v; PQueueIE.PlaceElement(alg, k);
                 ZeusCodeView.Exit(alg);
               END DownHeap;

PROCEDURE Replace(alg: T; pq: PriorityQueue; v: INTEGER): INTEGER 
    RAISES {Thread.Alerted} =
  BEGIN
    pq.heap[0] := v; 
    IF pq.size > 1 THEN DownHeap(alg, pq, 0); END;
    RETURN pq.heap[0]
  END Replace;

PROCEDURE HeapSort(alg: T;  pq: PriorityQueue) 
    RAISES {Thread.Alerted} = 
  VAR t: INTEGER;
  BEGIN
                 ZeusCodeView.Event(alg, procedureName := "HeapSort");
At(alg, 501);    FOR k := pq.size DIV 2 TO 1 BY -1 DO        
At(alg, 502);      DownHeap(alg, pq, k);
                 END;
At(alg, 503);    PQueueIE.Pause(alg);
                 PQueueIE.Pause(alg);
                 WHILE pq.size > 1 DO
At(alg, 504);      PQueueIE.SortStep(alg, pq.size); 
                   t := pq.heap[1]; pq.heap[1] := pq.heap[pq.size];
                   pq.heap[pq.size] := t;
                   DEC(pq.size);
At(alg, 505);      IF pq.size > 1 THEN DownHeap(alg, pq, 1); END;
                 END;
                 PQueueIE.SortStep(alg, pq.size);
  END HeapSort;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    pq := NEW(PriorityQueue);
    doSort: BOOLEAN;
    rand := NEW (Random.Default).init ();
  BEGIN
    doSort := FormsVBT.GetBoolean(alg.data, "sort");
    IF doSort THEN
      pq.size := FormsVBT.GetInteger(alg.data, "N");
      pq.maxSize := pq.size
    ELSE
      pq.maxSize := FormsVBT.GetInteger(alg.data, "qSize");
      pq.size := 0;
    END;
    PQueueIE.Setup(alg, pq.maxSize, doSort);
    pq.heap := NEW(REF ARRAY OF INTEGER, pq.maxSize+1);
    IF doSort THEN
      FOR i := 1 TO pq.size DO
        pq.heap[i] := rand.integer (PQueue.MinElt, PQueue.MaxElt);
      END;
      PQueueIE.InitSort(alg, pq.heap);
      HeapSort(alg, pq)
    ELSE
      LOCK alg.wq DO alg.wq.q := NIL END;
      WHILE TRUE DO
        WITH operation = alg.wq.removeElement(),
             op = NARROW (operation.head, REF PQueue.QueueOp)^,
             p1 = NARROW (operation.tail.head, REF INTEGER)^ DO
          CASE op OF
          | PQueue.QueueOp.Insert => 
            Insert(alg, pq, p1);
          | PQueue.QueueOp.Replace => 
            EVAL Replace(alg, pq, p1);
          | PQueue.QueueOp.Remove => 
            EVAL Remove(alg, pq)
          END;
        END;
      END;
    END;
  END Run;


PROCEDURE New (): Algorithm.T =
  VAR
    cv := RefList.List1(RefList.List2("Modula-3 Code View", "pqueueAlgs.m3"));
    fv := ZeusPanel.NewForm("pqueueinput.fv");
    newWq := NEW(PQueue.WorkQueue, q := NIL, c := NEW(Thread.Condition));
    alg := NEW(T, data := fv, codeViews := cv, wq := newWq).init();
  BEGIN
    FormsVBT.AttachProc(fv, "insert", HandleInsert, alg);
    (*  FormsVBT.AttachProc(fv, "replace", HandleReplace, alg); *)
    FormsVBT.AttachProc(fv, "remove", HandleRemove, alg);
    RETURN alg;
  END New;

PROCEDURE HandleRemove(<* UNUSED *> form: FormsVBT.T; 
    <* UNUSED *> event: TEXT;
                 cl: REFANY; 
    <* UNUSED *> ts: VBT.TimeStamp) =
  VAR alg := NARROW(cl, T);
  BEGIN
    alg.wq.addElement(PQueue.QueueOp.Remove);
  END HandleRemove;

<* UNUSED *> PROCEDURE HandleReplace(<* UNUSED *> form: FormsVBT.T; 
    <* UNUSED *> event: TEXT;
                 cl: REFANY; 
    <* UNUSED *> ts: VBT.TimeStamp) =
  VAR alg := NARROW(cl, T);
  BEGIN
    alg.wq.addElement(PQueue.QueueOp.Replace, 
           FormsVBT.GetInteger(alg.data, "rplelt") );
  END HandleReplace;

PROCEDURE HandleInsert(<* UNUSED *> form: FormsVBT.T; 
    <* UNUSED *> event: TEXT;
                 cl: REFANY; 
    <* UNUSED *> ts: VBT.TimeStamp) =
  VAR alg := NARROW(cl, T);
  BEGIN
    alg.wq.addElement(PQueue.QueueOp.Insert, 
           FormsVBT.GetInteger(alg.data, "inselt") );
  END HandleInsert;

BEGIN
  ZeusPanel.RegisterAlg(New, "HeapSort", "PQueue");
END PQAlgs.
