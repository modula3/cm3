(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 19:43:14 PDT 1994 by heydon   *)
(*      modified on Tue Aug  2 18:26:10 PDT 1994 by detlefs  *)
(*      modified on Thu Dec  9 11:45:28 PST 1993 by mcjones  *)
(*      modified on Thu Apr 29 15:51:05 PDT 1993 by gnelson  *)
<* PRAGMA SPEC*>

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

(* ESC Specifications. *)

<*SPEC VAR Valid: MAP T TO BOOLEAN *>
<*SPEC VAR Data: MAP T TO SEQ[Elem.T]*>

<*SPEC T.init(t, sizeHint)
       MODIFIES Valid[t], Data[t]
       ENSURES RES = t AND Valid'[t] AND NUMBER(Data'[t]) = 0 *>

<*SPEC T.fromArray(t, a)
       MODIFIES Valid[t], Data[t]
       ENSURES RES = t AND Valid'[t] AND NUMBER(Data'[t]) = NUMBER(a)
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(a)) IMPLIES
                                 Data'[t][i] = a[i]) *>

<*SPEC T.addhi(t, x)
       MODIFIES Data[t]
       REQUIRES Valid[t]
       ENSURES NUMBER(Data'[t]) = NUMBER(Data[t])+1
           AND (ALL [i: INTEGER] 0 <= i AND i < NUMBER(Data[t]) IMPLIES
                  Data'[t][i] = Data[t][i])
           AND Data'[t][NUMBER(Data[t])] = x *>

<*SPEC T.addlo(t, x)
       MODIFIES Data[t]
       REQUIRES Valid[t]
       ENSURES NUMBER(Data'[t]) = NUMBER(Data[t])+1
           AND Data'[t][0] = x
           AND (ALL [i: CARDINAL] (0 <= i AND i < NUMBER(Data[t])) IMPLIES
                  Data'[t][i+1] = Data[t][i]) *>

<*SPEC T.remhi(t)
       MODIFIES Data[t]
       REQUIRES Valid[t] AND NUMBER(Data[t]) > 0
       ENSURES NUMBER(Data'[t]) = NUMBER(Data[t])-1
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(Data'[t])) IMPLIES
                  Data'[t][i] = Data[t][i]) *>

<*SPEC T.remlo(t)
       MODIFIES Data[t]
       REQUIRES Valid[t] AND NUMBER(Data[t]) > 0
       ENSURES NUMBER(Data'[t]) = NUMBER(Data[t])-1
           AND (ALL [i: INTEGER] (0 <= i AND i < NUMBER(Data'[t])) IMPLIES
                  Data'[t][i] = Data[t][i+1]) *>

<*SPEC T.put(t, i, x)
       MODIFIES Data[t][i]
       REQUIRES Valid[t] AND i < NUMBER(Data[t])
       ENSURES Data'[t][i] = x
*>
(*
       ENSURES Data'[t][i] = x AND NUMBER(Data'[t]) = NUMBER(Data[t]) *>
*)

<*SPEC T.size(t)
       REQUIRES Valid[t]
       ENSURES RES = NUMBER(Data[t]) *>

<*SPEC T.gethi(t)
       REQUIRES Valid[t] AND NUMBER(Data[t]) > 0
       ENSURES RES = Data[t][NUMBER(Data[t])-1] *>

<*SPEC T.getlo(t)
       REQUIRES Valid[t] AND NUMBER(Data[t]) > 0
       ENSURES RES = Data[t][0] *>

<*SPEC T.get(t, i)
       REQUIRES Valid[t] AND i < NUMBER(Data[t])
       ENSURES RES = Data[t][i] *>

<*SPEC Cat(s, t)
       MODIFIES Data[RES], Valid[RES]
       REQUIRES Valid[s] AND Valid[t]
       ENSURES FRESH(RES) AND Valid'[RES]
           AND (ALL [i: INTEGER]
                 (0 <= i AND i < NUMBER(Data'[RES])) IMPLIES
                    Data'[RES][i] = CONCAT(Data[s], Data[t])[i])
*>

<*SPEC Sub(s, start, length)
       MODIFIES Data[RES], Valid[RES]
       REQUIRES Valid[s]
       ENSURES Valid'[RES] AND FRESH(RES) AND
               (ALL [i: INTEGER]
                 (0 <= i AND i < MIN(length, NUMBER(Data[s]) - start)) IMPLIES
                    Data'[RES][i] = SUBARRAY(Data[s],
                                             MAX(0, MIN(start,
                                                        NUMBER(Data[s])-1)),
                                             MIN(length, NUMBER(Data[s])
                                                         -start))[i])
*>

END Sequence.
