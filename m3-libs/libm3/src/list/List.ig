(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Mon Apr 10 16:19:34 PDT 1995 by detlefs  *)
(*      modified on Thu Sep 22 19:31:11 PDT 1994 by heydon   *)
(*      modified on Thu Dec  9 11:51:43 PST 1993 by mcjones  *)
(*      modified on Wed Apr 28 18:35:21 PDT 1993 by meehan   *)
(*      modified on Wed Jan 27 20:23:00 PST 1993 by gnelson  *)

(* The generic interface "List" provides operations on linked lists of
   arbitrary element types. *)

GENERIC INTERFACE List(Elem);
(* Where "Elem.T" is not an open array type and "Elem" contains

| CONST Brand = <text-constant>;
| PROCEDURE Equal(k1, k2: Elem.T): BOOLEAN;

   "Brand" must be a text constant. It will be used to construct a brand for
   any generic types instantiated with the "List" interface. For a non-generic
   interface, we recommend choosing the name of the interface.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".

 *)

CONST Brand = "(List " & Elem.Brand & ")";

TYPE T = OBJECT head: Elem.T; tail: T END;

(* A "List.T" represents a linked list of items of type "Elem.T". *)

(* None of the operations of this interface modify the "head" field of
   an existing list element.  Operations that may modify the "tail"
   field of existing list elements are called {\it destructive}.  By
   convention, their names end in "D".
   \index{naming conventions!destructive list operations}
*)

PROCEDURE Cons(READONLY head: Elem.T; tail: T): T;
(* Equivalent to "NEW(T, head := head, tail := tail)". *)

PROCEDURE List1(READONLY e1: Elem.T): T;
(* Return a list containing the single element "e1". *)

PROCEDURE List2(READONLY e1, e2: Elem.T): T;
(* Return a list containing the element sequence "e1", "e2". *)

PROCEDURE List3(READONLY e1, e2, e3: Elem.T): T;
(* Return a list containing the element sequence "e1", "e2", "e3". *)

PROCEDURE FromArray(READONLY e: ARRAY OF Elem.T): T;
(* Return a list containing the elements of "e" in order. *)

PROCEDURE Length(l: T): CARDINAL;
(* Return the number of elements of "l". *)

PROCEDURE Nth(l: T; n: CARDINAL): Elem.T;
(* Return element "n" of list "l".  Element 0 is "l.head", element 1 
   is "l.tail.head", etc.  Cause a checked runtime error if 
   "n >= Length(l)". *)

PROCEDURE Member(l: T; READONLY e: Elem.T): BOOLEAN;
(* Return "TRUE" if some element of "l" is equal to "e", else return
   "FALSE".  The comparison is performed by "Elem.Equal".  *)

PROCEDURE Append(l1: T; l2: T): T;
PROCEDURE AppendD(l1: T; l2: T): T;
(* Append two lists together, returning the new list.  "Append"
   does this by making a copy of the cells of "l1"; "AppendD"
   modifies the "tail" field in the last cell of "l1". *)

PROCEDURE Reverse(l: T): T;
PROCEDURE ReverseD(l: T): T;
(* Return a list containing the elements of "l" in reverse order.
   "Reverse" copies the cells; "ReverseD" modifies the "tail" fields
   of the existing cells. *)

END List.

<*PRAGMA SPEC*>

<*SPEC Cons(head, tail)
       ENSURES RES # NIL AND FRESH(RES) AND RES.head = head AND RES.tail = tail
*>
<*SPEC List1(e1)
       ENSURES RES # NIL AND FRESH(RES) AND RES.head = e1
           AND RES.tail = NIL
*>
<*SPEC List2(e1, e2)
       ENSURES RES # NIL AND FRESH(RES) AND RES.head = e1
           AND RES.tail # NIL AND FRESH(RES.tail) AND RES.tail.head = e2
           AND RES.tail.tail = NIL
*>
<*SPEC List3(e1, e2, e3)
       ENSURES RES # NIL AND FRESH(RES) AND RES.head = e1
           AND RES.tail # NIL AND FRESH(RES.tail) AND RES.tail.head = e2
           AND RES.tail.tail # NIL AND FRESH(RES.tail.tail)
               AND RES.tail.tail.head = e3
           AND RES.tail.tail.tail = NIL
*>
