(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Thu Sep 22 20:19:26 PDT 1994 by heydon                   *)

(* "PQueue" is a generic interface defining priority queues. A priority queue
   is a multiset of elements. If "pq" is a priority queue, we denote its
   multiset by "mset(pq)". The basic priority queue operations are to insert a
   new element into the queue and to delete one of its elements with minimal
   priority value. *)

GENERIC INTERFACE PQueue(Priority);
(* Where "Priority.T" is a type that is not an open array type, and the
   "Priority" interface contains:

| CONST Brand = <text-constant>;
| PROCEDURE Compare(p1, p2: T): [-1..1]

   "Brand" must be a text constant. It will be used to construct a brand for
   the opaque types "PQueue.Elt" and "PQueue.Default", and any generic types
   instantiated with the "PQueue" interface. For a non-generic interface, we
   recommend choosing the name of the interface.

   "Compare" must be a total order. "Compare" may be declared with a parameter
   mode of either "VALUE" or "READONLY", but not "VAR". *)

CONST
  EltBrand = "(PQueueElt " & Priority.Brand & ")";
  (* The type "PQueue.Elt" is revealed to have the brand "EltBrand". *)

TYPE
  Elt <: OBJECT
    priority: Priority.T
  END;

(* A "PQueue.Elt" is an {\it element\/} of a priority queue. Every element
   "elt" has a priority "elt.priority". *)

EXCEPTION Empty; NotInQueue;

(* The "Empty" exception is raised when "mset(pq)" is empty. The "NotInQueue"
   exception is raised if the element to delete or change is not currently in
   the queue. *)

CONST
  Brand = "(PQueue " & Priority.Brand & ")";
  DefaultBrand = "(Default " & Brand & ")";
  (* The type "PQueue.Default" is revealed to have the brand "DefaultBrand". *)

TYPE
  T = OBJECT METHODS
    insert(READONLY elt: Elt);
    delete(elt: Elt) RAISES {NotInQueue};
    size(): CARDINAL;
    min(): Elt RAISES {Empty};
    deleteMin(): Elt RAISES {Empty};
    change(elt: Elt; newP: Priority.T) RAISES {NotInQueue};
  END;

(* Do not instantiate a "T"; instead, instantiate one of its subtypes, such as
   the "Default" implementation below. The methods have the following
   specifications:

   The call "pq.insert(elt)" adds "elt" to "mset(pq)".

   The call "pq.delete(elt)" removes element "elt" from the queue.

   The call "pq.size()" returns the number of elements in the queue.

   The call "pq.min()" returns an element in the queue with minimal priority
   value.

   The assignment "elt := pq.deleteMin()" is equivalent to the code

|    elt := pq.min(); pq.delete(elt)

   However, "deleteMin" can be implemented more efficiently than by simply
   composing the "min" and "delete" operations.

   The call "pq.change(elt, newP)" changes the priority of the element "elt"
   to "newP". It is equivalent to:

|    pq.delete(elt);
|    elt.priority := newP;
|    pq.insert(elt)

   However, "change" can be implemented more efficiently than by simply
   composing the "delete" and "insert" operations. *)

  Default <: DefaultPub;
  DefaultPub = T OBJECT METHODS
    init(sizeHint: CARDINAL := 10): Default;
    fromArray(READONLY e: ARRAY OF Elt): Default;
    pCompare(READONLY p1, p2: Priority.T): [-1..1]
  END;

(* The type "PQueue.Default" is an implementation of priority queues that
   uses a heap represented by an array. This implementation guarantees that
   the "insert", "delete", "deleteMin", and "change" operations will take
   "O(log n)" time (where "n" is the number of elements in the queue), and
   that the "size" and "min" operations will take "O(1)" time.

   The call "NEW(PQueue.Default).init(sizeHint)" creates a new, empty priority
   queue. The "init" method resets "mset(pq)" of an existing priority queue
   "pq" to the empty set.

   The call "NEW(PQueue.Default).fromArray(elts)" creates a new priority
   queue containing the elements "elts[FIRST(elts)], ..., elts[LAST(elts)]".
   Initializing a priority queue in this way takes linear time, whereas
   creating an empty queue and adding the elements to it one at a time takes
   "O(n log n)" time in the worst case.

   By default, the method call "pq.pCompare(p1, p2)" returns the result of the
   call "Priority.Compare(p1, p2)". Clients are free to override the
   "pCompare" method to change the total order on priorities.

   For efficiency, a "PQueue.Default" is unmonitored: it is up to clients to
   avoid illegal concurrent accesses on its methods. The readonly methods are
   "size" and "min". *)

END PQueue.
