(* Copyright 1992 Digital Equipment Corporation.            *)
(* Distributed only by permission.                          *)
(* Last modified on Tue May  3 09:51:23 PDT 1994 by najork  *)

MODULE PQueue;

IMPORT Fmt, RefList, Thread, MGPublic;

REVEAL
  WorkQueue = WorkQueuePublic BRANDED OBJECT
  OVERRIDES
    addElement := AddElement;
    removeElement := RemoveElement;
    isEmpty := IsEmpty;
  END;

PROCEDURE PrintHeap(heap: REF ARRAY OF INTEGER): TEXT =
  VAR result:= "";
  BEGIN
   FOR i := 1 TO LAST(heap^) DO
     result := result & Fmt.Int(heap[i])& " ";
   END;
   RETURN result;
  END PrintHeap;

PROCEDURE AddElement(wq: WorkQueue; op: QueueOp; p1: INTEGER :=0) =
  VAR refOp := NEW(REF QueueOp);
      refp1 := NEW(REF INTEGER);
  BEGIN
    refOp^ := op;  refp1^ := p1;
    LOCK wq DO
      wq.q := RefList.Append (wq.q, 
                              RefList.List1 (RefList.List2 (refOp, refp1)));
      Thread.Broadcast(wq.c);
    END;
  END AddElement;

PROCEDURE RemoveElement(wq: WorkQueue) : RefList.T RAISES {Thread.Alerted} =
  BEGIN
    LOCK wq DO
      WHILE RefList.Length(wq.q) = 0 DO
        Thread.AlertWait(wq, wq.c);
      END;
      WITH head = wq.q.head DO
        wq.q := wq.q.tail;
        RETURN head;
      END;
    END;
  END RemoveElement;

PROCEDURE IsEmpty(wq: WorkQueue) : BOOLEAN =
  BEGIN
    LOCK wq DO
      RETURN (RefList.Length(wq.q) = 0);
    END;
  END IsEmpty;

BEGIN
  StartColor := MGPublic.OpFromName("Orchid");
  SortedColor := MGPublic.OpFromName("RatherDarkOrchid");
  WorkColor := MGPublic.OpFromName("RatherPaleOrchid");
  HighlightColor := MGPublic.OpFromName("Turquoise");
  NotInHeapEdgeColor := MGPublic.OpFromName("PaleGray");
  Black := MGPublic.OpFromName("Black");
  White := MGPublic.OpFromName("White");
END PQueue.
