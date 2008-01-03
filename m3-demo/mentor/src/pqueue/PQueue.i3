(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

INTERFACE PQueue;

IMPORT RefList, PaintOp, Thread;

CONST
  MaxElt = 100;
  MinElt = 10;
  DefaultEdgeWidth = 0.03;
  ThickEdgeWidth = 0.1;
  HighlightWidth = 0.1;

VAR (* Read only after initialization *)
  StartColor, SortedColor, WorkColor, HighlightColor, 
    NotInHeapEdgeColor, Black, White: PaintOp.T;

TYPE
  PriorityQueue = OBJECT
    size: INTEGER;
    maxSize: INTEGER;
    heap: REF ARRAY OF INTEGER
  END;

  QueueOp = {Insert, Replace, Remove};

  WorkQueue <: WorkQueuePublic;

  WorkQueuePublic = Thread.Mutex OBJECT
    q: RefList.T (* OF (op, p1) *);
    c: Thread.Condition;
  METHODS
    addElement(op: QueueOp; p1: INTEGER := 0);
    removeElement() : RefList.T (* (op, p1) *) RAISES {Thread.Alerted};
    isEmpty() : BOOLEAN;
  END;
 
  Array = REF ARRAY OF INTEGER;

PROCEDURE PrintHeap(heap: REF ARRAY OF INTEGER): TEXT;

END PQueue.
