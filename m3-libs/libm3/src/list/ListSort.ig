(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Sep 22 19:35:29 PDT 1994 by heydon  *)
(*      modified on Mon Nov  8 14:52:01 PST 1993 by mcjones *)
(*      modified on Wed Jan 27 20:23:00 PST 1993 by gnelson *)

(* The generic interface "ListSort" extends the generic interface
   "List" with sorting operations.
   \index{sorting!lists}
 *)

GENERIC INTERFACE ListSort(Elem, ElemList);
(* Where "Elem.T" is not an open array type, "ElemList" equals "List(Elem)",
   and "Elem" contains

| CONST Brand = <text-constant>;
| PROCEDURE Compare(e1, e2: Elem.T): [-1..1];

   "Brand" must be a text constant. It will be used to construct a brand for
   any generic types instantiated with the "ListSort" interface. For a
   non-generic interface, we recommend choosing the name of the interface.

   "Compare" must be a total order. It may be declared with any parameter
   mode, but must have no visible side-effects.
*)

CONST Brand = "(ListSort " & Elem.Brand & ")";

TYPE T = ElemList.T;

PROCEDURE Sort(l: T; c := Elem.Compare): T;
PROCEDURE SortD(l: T; c := Elem.Compare): T;
(* Sort a list in ascending order using "c" to compare pairs of
   elements of "l". *)

(* The implementation is time- and cons-efficient but not guaranteed
   to be stable.  "Sort" copies the cells; "SortD" modifies the "tail"
   fields of the existing cells.  *)

END ListSort.

