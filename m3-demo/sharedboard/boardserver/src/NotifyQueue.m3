(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE NotifyQueue;

IMPORT Thread,
       NotifyRec, NotifyRecList;

REVEAL Private = MUTEX BRANDED "NotifyQueueP" OBJECT END;
       T = Public BRANDED "NotifyQueue" OBJECT
         list: NotifyRecList.T := NIL;
         nonEmpty: Thread.Condition;
       OVERRIDES
         init := Init;
         enq := Enq;
         deq := Deq;
       END;    

PROCEDURE Init (nq: T): T =
  BEGIN
    nq.nonEmpty := NEW (Thread.Condition);
    RETURN nq;
  END Init;

PROCEDURE Enq (nq: T; nr: NotifyRec.T) =
  BEGIN
    LOCK nq DO
      nq.list := NotifyRecList.AppendD (nq.list, NotifyRecList.List1 (nr));
      (** This should be changed to allow merging for efficiency. **)
      Thread.Signal (nq.nonEmpty);
    END;
  END Enq;

PROCEDURE Deq (nq: T): NotifyRec.T =
  VAR nr: NotifyRec.T;
  BEGIN
    LOCK nq DO
      WHILE nq.list = NIL DO Thread.Wait (nq, nq.nonEmpty) END;
      nr := nq.list.head;
      nq.list := nq.list.tail;
      RETURN nr;
    END;
  END Deq;

BEGIN
END NotifyQueue.

