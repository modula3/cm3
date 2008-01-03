(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

TYPE PriorityQueue = RECORD
   size: INTEGER;
   heap: REF ARRAY OF INTEGER
  END;

@Insert
PROCEDURE Insert(pq: PriorityQueue; v: INTEGER) RAISES {}= @
  BEGIN
@101     INC(pq.size); pq.heap[size] := v; UpHeap(size) @
  END Insert;
@Insert

@UpHeap
PROCEDURE UpHeap(pq: PriorityQueue; k:INTEGER) RAISES {}=@
  VAR v: INTEGER;
  BEGIN
@201     v := pq.heap[k]; pq.heap[0] := LAST(INTEGER); @
@202     WHILE pq.heap[k DIV 2] <= v DO
      pq.heap[k] := pq.heap[k DIV 2]; k := k DIV 2 
    END;@
@204     pq.heap[k] := v;@
  END UpHeap;
@UpHeap

@Remove
PROCEDURE Remove(pq: PriorityQueue): INTEGER RAISES {}=@
  VAR outVal: INTEGER;
  BEGIN
@301     outVal := pq.heap[1];
    pq.heap[1] := pq.heap[pq.size]; DEC(pq.size); @
@302     DownHeap(pq, 1);@
@303    RETURN outVal;@
  END Remove;
@Remove

@DownHeap 
PROCEDURE DownHeap(pq: PriorityQueue; k: INTEGER) RAISES {} = @
  VAR i, j, v: INTEGER;
  BEGIN
@401     v := pq.heap[k];@
    WHILE k <= pq.size DIV 2 DO
@402       j := k+k;
      IF j < pq.size THEN
        IF pq.heap[j] < pq.heap[j+1] THEN
          INC(j)
        END;
      END;@
@406       IF v >= pq.heap[j] THEN
        EXIT;
      END;@
@408       pq.heap[k] := pq.heap[j]; k := j;@
    END;
@409     pq.heap[k] := v;@
  END DownHeap;
@DownHeap


@HeapSort
PROCEDURE HeapSort(pq: PriorityQueue) RAISES {} = @
  VAR t: INTEGER;
  BEGIN
@501     FOR k := pq.size DIV 2 TO 1 BY -1 DO @
@502       DownHeap(pq, k); @
    END; 
@503     (* array is now a heap *)@
    REPEAT 
@504       t := pq.heap[1]; pq.heap[1] := pq.heap[pq.size];
      pq.heap[pq.size] := t;
      DEC[pq.size];@
@505       DownHeap(pq, 1);@
    UNTIL pq.size <= 1;
  END HeapSort;
@HeapSort

