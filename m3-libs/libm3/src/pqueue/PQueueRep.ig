(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sun Sep 18 18:17:49 PDT 1994 by heydon                   *)

GENERIC INTERFACE PQueueRep(PQ);
(* where "PQ = PQueue(Priority)". *)

REVEAL PQ.Default <: DefaultPub;

TYPE
  EltsArray = REF ARRAY OF PQ.Elt;

  DefaultPub = PQ.DefaultPub OBJECT
    sz: CARDINAL := 0;                   (* number of elements in heap *)
    heap: EltsArray := NIL;      (* elements stored in heap[1..sz] *)
  END;

END PQueueRep.

(* A "PQueue.Default" is represented by a data structure called a {\it heap}.
   A heap is a complete binary tree in which each node has a priority at least
   that of its parent. Hence, the root of the tree has minimal priority.

   A priority queue "pq: PQueue.Default" is {\it valid} (written "Valid(pq)")
   iff "pq.heap # NIL". The methods "init(pq, sizeHint)" and "fromArray(pq,
   e)" establish "Valid(pq)", and all of the other methods beside "pCompare"
   require "Valid(pq)".

   A valid priority queue "pq: PQueue.Default" satisfies the following
   invariants:

   1. 0 <= pq.sz <= LAST(pq.heap^)

   2. (forall i: 1 < i <= sz: pq.pCompare(pq.heap[i DIV 2], pq.heap[i]) < 1)

   The heap is represented by an array "pq.heap", and a count "pq.size" of the
   number of elements in the heap. The "pq.size" elements are stored in the
   array entries "pq.heap[1]" through "pq.heap[pq.size]". The element
   "pq.heap[1]" is the root of the heap, and the parent of element "i" is the
   element "i DIV 2". The second invariant is the heap invariant: the priority
   of a non-root element is at least that of its parent.

   For a complete description of priority queues, see "Algorithms in
   Modula-3", Robert Sedgewick, Addison-Wesley Publishing Company, 1993,
   Chapter 11.
*)
