(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Jan 26 13:37:39 PST 1996 by detlefs  *)
(*      modified on Thu Sep 22 19:43:14 PDT 1994 by heydon   *)
(*      modified on Thu Dec  9 11:45:28 PST 1993 by mcjones  *)
(*      modified on Thu Apr 29 15:51:05 PDT 1993 by gnelson  *)

(* "Sequence" is a generic interface defining extensible sequences.
   Elements can be added or removed at either end of a sequence; they
   can also be accessed or updated at specified indexes.  The expected
   cost of every method of a sequence is constant.
   \index{stack: {\tt Sequence} generic interface}
   \index{queue: {\tt Sequence} generic interface}
   \index{deque: {\tt Sequence} generic interface}
*)

GENERIC INTERFACE Sequence(Elem);
(* Where "Elem.T" is a type that is not an open array type and "Elem"
   contains

| CONST Brand = <text-constant>;

   "Brand" must be a text constant. It will be used to construct a brand for
   the opaque type "Sequence.T" and any generic types instantiated with the
   "Sequence" interface. For a non-generic interface, we recommend choosing
   the name of the interface.
*)

CONST
  Brand = "(Sequence " & Elem.Brand & ")";

TYPE 
  T <: Public;
  Public = OBJECT METHODS
    init(sizeHint: CARDINAL := 5): T;
    fromArray(READONLY a: ARRAY OF Elem.T): T;
    addhi(READONLY x: Elem.T);
    addlo(READONLY x: Elem.T);
    remhi(): Elem.T;
    remlo(): Elem.T;
    put(i: CARDINAL; READONLY x: Elem.T);
    size(): CARDINAL;
    gethi(): Elem.T;
    getlo(): Elem.T;
    get(i: CARDINAL): Elem.T
  END;

(* A "Sequence(Elem).T" (or just a {\it sequence}) represents an
   extensible sequence of "Elem.T"s.  
     
   The first group of methods have side effects on the sequence.  The
   call

| s.init(sizeHint)

   initializes "s" to be the empty sequence.  Furthermore "init"
   assumes that at least "sizeHint" elements will be added to the
   sequence; these operations may be executed more efficiently than if
   "sizeHint" was defaulted.  The call

| s.fromArray(a)

   initializes "s" to be the sequence with elements
   "a[0],~...,~a[LAST(a)]".  The call

| s.addhi(x)

   appends "x" to the end of "s".  Thus it does not change the index
   of any existing element.  The call

| s.addlo(x)

   appends "x" to the front of "s".  Thus it increases the index of
   all existing elements by one. The call
  
| s.remhi()

   removes and returns the last element of "s".  Thus it does not
   change the index of any of "s"'s other elements.  If "s" is empty,
   "s.remhi()" causes a checked runtime error.  The call

| s.remlo()

   removes and returns the first element of "s".  Thus it decreases
   the index of all other elements of "s" by one. If "s" is empty,
   "s.remlo()" causes a checked runtime error.  The call

| s.put(i, x)

   replaces element "i" of "s" with "x".  Element "0" is the first
   element. It is a checked runtime error unless "i" is less than
   "s.size()".

   The second group of methods have no side effect on the sequence.
   The call

| s.size()

   returns the number of elements in "s".  The call

| s.get(i)

   returns element "i" of "s". It is a checked runtime error unless
   "i" is less than "s.size()".  The call

| s.gethi()

   returns the last element of "s"; that is, it is equivalent to
   "s.get(s.size()-1)".  The call

| s.getlo()

   returns the first element of "s"; that is, it is equivalent to
   "s.get(0)". *)

PROCEDURE Cat(s, t: T): T;
(* Return a sequence whose elements are the concatenation of "s"
   and "t". *)

PROCEDURE Sub(s: T; start: CARDINAL;
    length: CARDINAL := LAST(CARDINAL)): T;
(* Return a sub-sequence of "s": empty if "start >= t.size()" or
   "length = 0"; otherwise the subsequence ranging from "start" to the
   minimum of "start+length-1" and "s.size()-1". *)

(* "Cat" and "Sub" create new sequences; they have no side-effects.

\smallskip
   Sequences are unmonitored: a client accessing a sequence from
   multiple threads must ensure that if two operations are active
   concurrently, then neither of them has side effects on the
   sequence.  *)

END Sequence.
