(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Sep 23 09:51:12 PDT 1994 by heydon                   *)

GENERIC MODULE PQueue(Priority, PQ, PQRep);
(* where "PQ = PQueue(Priority)" and "PQRep = PQueueRep(PQ)". *)

TYPE
  EltPub = OBJECT
    priority: Priority.T;
  END;

REVEAL
  Elt = EltPub BRANDED PQ.EltBrand OBJECT
    i: INTEGER := -1;              (* index in heap array; -1 => not in heap *)
  END;

REVEAL
  PQ.Default = PQRep.DefaultPub BRANDED PQ.DefaultBrand OBJECT
  OVERRIDES
    init := Init;
    fromArray := FromArray;
    insert := Insert;
    delete := Delete;
    size := Size;
    min := Min;
    deleteMin := DeleteMin;
    change := Change;
    pCompare := PCompare;
  END;

PROCEDURE UpHeap(pq: Default; start: Elt) =
  VAR curr := start.i; BEGIN
    WHILE
      curr > 1 AND
      pq.pCompare(start.priority, pq.heap[curr DIV 2].priority) = -1
    DO
      pq.heap[curr] := pq.heap[curr DIV 2];
      pq.heap[curr].i := curr;
      curr := curr DIV 2
    END;
    IF curr # start.i THEN
      pq.heap[curr] := start;
      start.i := curr
    END
  END UpHeap;

PROCEDURE DownHeap(pq: Default; start: Elt) =
  VAR
    curr := start.i;			 (* current array index *)
    startP := start.priority;		 (* priority of filtered element *)
    min: CARDINAL;			 (* index of smallest child *)
    lastInternal := pq.sz DIV 2;	 (* index of last node with children *)
    hp := pq.heap;
  BEGIN
    WHILE curr <= lastInternal DO
      (* set "min" to index of largest child *)
      min := curr * 2;
      IF min < pq.sz AND
         pq.pCompare(hp[min+1].priority, hp[min].priority) = -1
      THEN
        INC(min)
      END;
      (* exit if current node is smaller than smallest child *)
      IF pq.pCompare(startP, hp[min].priority) = -1 THEN EXIT END;
      (* bubble min element up *)
      hp[curr] := hp[min];
      hp[curr].i := curr;
      curr := min
    END;
    hp[curr] := start;
    start.i := curr
  END DownHeap;

PROCEDURE Init(pq: Default; sizeHint: CARDINAL := 10): Default =
  BEGIN
    IF pq.heap = NIL OR sizeHint > LAST(pq.heap^)
      THEN pq.heap := NEW(REF ARRAY OF Elt, sizeHint + 1)
      ELSE FOR i := 1 TO pq.sz DO pq.heap[i] := NIL END
    END;
    pq.sz := 0;
    RETURN pq
  END Init;

PROCEDURE FromArray(pq: Default; READONLY e: ARRAY OF Elt): Default =
(* Build heap bottom-up. This takes "O(n)" time, where "n" is "NUMBER(e)". *)
  VAR newSz := NUMBER(e); BEGIN
    IF pq.heap = NIL OR newSz > LAST(pq.heap^)
      THEN pq.heap := NEW(REF ARRAY OF Elt, newSz + 1)
      ELSE FOR i := newSz + 1 TO pq.sz DO pq.heap[i] := NIL END;
    END;
    pq.sz := newSz;
    SUBARRAY(pq.heap^, 1, newSz) := e;
    WITH heap = pq.heap DO
      FOR i := 1 TO newSz DO heap[i].i := i END;
      FOR i := (newSz DIV 2) TO 1 BY -1 DO DownHeap(pq, heap[i]) END
    END;
    RETURN pq
  END FromArray;

PROCEDURE Insert(pq: Default; READONLY elt: Elt) =
  BEGIN
    INC(pq.sz);
    IF pq.sz > LAST(pq.heap^) THEN
      VAR new := NEW(REF ARRAY OF Elt, NUMBER(pq.heap^) * 2); BEGIN
        SUBARRAY(new^, 0, NUMBER(pq.heap^)) := pq.heap^;
        pq.heap := new
      END
    END;
    pq.heap[pq.sz] := elt;
    elt.i := pq.sz;
    UpHeap(pq, elt)
  END Insert;

PROCEDURE Delete(pq: Default; elt: Elt) RAISES {NotInQueue} =
  BEGIN
    IF elt.i = -1 THEN RAISE NotInQueue END;
    VAR last := pq.heap[pq.sz]; BEGIN
      pq.heap[pq.sz] := NIL; (* to prevent storage leak *)
      DEC(pq.sz);
      IF last # elt THEN
      	pq.heap[elt.i] := last;
      	last.i := elt.i;
      	CASE pq.pCompare(elt.priority, last.priority) OF
      	 -1 => DownHeap(pq, last)
      	| 0 => (* SKIP *)
      	| 1 => UpHeap(pq, last)
      	END
      END
    END;
    elt.i := -1
  END Delete;

PROCEDURE Size(pq: Default): CARDINAL =
  BEGIN RETURN pq.sz END Size;

PROCEDURE Min(pq: Default): Elt RAISES {Empty} =
  BEGIN
    IF pq.sz = 0 THEN RAISE Empty END;
    RETURN pq.heap[1]
  END Min;

PROCEDURE DeleteMin(pq: Default): Elt RAISES {Empty} =
  VAR res: Elt; BEGIN
    IF pq.sz = 0 THEN RAISE Empty END;
    res := pq.heap[1];
    res.i := -1;
    VAR last := pq.heap[pq.sz]; BEGIN
      pq.heap[pq.sz] := NIL; (* to prevent storage leak *)
      DEC(pq.sz);
      IF pq.sz > 0 THEN
      	pq.heap[1] := last;
      	last.i := 1;
      	DownHeap(pq, last)
      END
    END;
    RETURN res
  END DeleteMin;

PROCEDURE Change(pq: Default; elt: Elt; newP: Priority.T) RAISES {NotInQueue} =
  BEGIN
    IF elt.i = -1 THEN RAISE NotInQueue END;
    VAR oldP := elt.priority; BEGIN
      elt.priority := newP;
      CASE pq.pCompare(oldP, newP) OF
       -1 => DownHeap(pq, elt)
      | 0 => (* SKIP *)
      | 1 => UpHeap(pq, elt)
      END
    END
  END Change;

PROCEDURE PCompare(<*UNUSED*> pq: Default; READONLY p1, p2: Priority.T)
  : [-1..1] =
  BEGIN RETURN Priority.Compare(p1, p2) END PCompare;

BEGIN
END PQueue.
