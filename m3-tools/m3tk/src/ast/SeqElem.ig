(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC INTERFACE SeqElem(Elem);

(* This interface provides a sequence abstraction, tailored to
   support the repetitive constructs in an Abstract Syntax Tree.
   It can also be used to support sets, provided the client enures
   no duplicates. There are no primitives to remove elements or
   to join sequences, since these notions are infrequent in
   ASTs, and can be programmed, admittedly inefficiently, in
   terms of the given primitives. Since this interface is 
   instantiated many times, it is important to minimise its size.

   Clients of this interface should provide their own locking in the 
   case of multiple threads. *)
   
TYPE T <: REFANY; 

CONST Null: T = NIL; 
(* To declare the null sequence do: "VAR s := SeqElem.Null;"
   Do not assume, however, that an empty sequence has the value NIL,
   use "Empty" instead. *)

PROCEDURE Empty(s: T): BOOLEAN RAISES {};
(* Returns TRUE iff "s" is the empty or null sequence, FALSE otherwise. *)

PROCEDURE Length(s: T): CARDINAL RAISES {};
(* Returns the length of sequence "s". The null sequence has
   length 0. *)

PROCEDURE AddFront(VAR (*inout*) s: T; elem: Elem.T) RAISES {};
(* Add the node "elem" to the front of sequence "s". *)

PROCEDURE AddRear(VAR (*inout*) s: T; elem: Elem.T) RAISES {};
(* Add the node "elem" to the rear of sequence "s". *)

PROCEDURE Insert(VAR (*inout*) s: T; elem: Elem.T; i: CARDINAL) RAISES {};
(* Insert the node "elem" before the node at index "i" in the
   sequence. The nodes are indexed from "0" to "Length(s)-1",
   and "0 <= i <= Length(s)". If "i = 0" the call is equivalent
   to "AddFront(s, elem)". If "i = Length(s)", the call is equivalent
   to "AddRear(s, elem)". *)

PROCEDURE First(s: T): Elem.T RAISES {};
(* Return the first node in sequence "s". It is a checked run-time
   error if "Empty(s)". *)

TYPE Iter <: REFANY;

PROCEDURE NewIter(s: T): Iter RAISES {};
(* Create an iterator on sequence "s". The behaviour of the iterator
  is undefined if updates to the sequence occur during an iteration. 
  Each call creates a distinct iterator. *)

PROCEDURE Next(
    VAR (*inout*) iter: Iter; 
    VAR (*out*) elem: Elem.T;
    ): BOOLEAN RAISES {};
(* Sequential calls of "Next" return the members of the sequence in turn
  starting with the first. If there are no more members, FALSE is returned
  and "elem" is unchanged, else "elem" is set to the member and TRUE is
  returned. *)

PROCEDURE Exhausted(iter: Iter): BOOLEAN RAISES {};
(* Return TRUE iff "iter" is exhausted, i.e. a call of "Next"
   would return FALSE. *)

PROCEDURE Update(VAR (*inout*) s: T; iter: Iter; new_elem: Elem.T) RAISES {};
(* If "Next(iter, elem)" would return TRUE, then this procedure replaces the
   member of "s" (whose old value was "elem) with "new_elem". No actual call
   of "Next" takes place, however, and "iter" is NOT advanced. It is an 
   unchecked run-time error if "s" is not the same value that was passed to 
   the call of "NewIter" that created "iter".  Typical usage is as follows:

  VAR 
    iter, iter_u := SeqElem.NewIter(s);
    elem, new_elem: Elem.T;
  BEGIN
    WHILE SeqElem.Next(iter, elem) DO
      IF some-condition THEN SeqElem.Update(s, iter_u, new_elem) END:
      EVAL SeqElem.Next(iter_u, elem);
    END;
  END
*)

PROCEDURE Ith(s: T; i: CARDINAL): Elem.T RAISES {};
(* Provides a view of the sequence as an array, indexed from "0"
   to "Length(s)-1". It is a checked runtime error if "i" is out of
   bounds. N.B. This is not an efficient way to iterate the sequence. *)

  
END SeqElem.

