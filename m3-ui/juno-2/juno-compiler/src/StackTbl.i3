(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Oct 16 15:51:09 PDT 1992 by heydon                   *)

INTERFACE StackTbl;

IMPORT Atom;

TYPE
  T <: Public;
  Public = OBJECT
    next_index: CARDINAL;
    next_formal: INTEGER
  METHODS
    init(): T;
  END;

(* A "StackTbl.T" is a pair of integers (the "next_index" and "next_formal")
   together with a list of marks and atom-integer pairs.

   A newly allocated "StackTbl.T" must be initialized before use, and can be
   initialized again for re-use. A newly intialized "StackTbl.T" has a
   "next_index" of 1, a "next_formal" of -1, and an empty list.
  *)

PROCEDURE Mark(t: T);
(* Add a mark to the front of "t"'s list. *)

PROCEDURE PopToMark(t: T);
(* Remove all entries from "t"'s list up to and including the first mark. This
   call should ``balance'' with a previous call to "Mark". More precisely, it
   is a checked run-time error to call this procedure if "t"'s stack does not
   contain any marks. *)

PROCEDURE Push(t: T; nm: Atom.T);
(* Add the pair ("nm", "t.next_index") to the front of "t"'s list, and
   increment "t.next_index". *)

PROCEDURE PushFormal(t: T; nm: Atom.T);
(* Add the pair ("nm", "t.next_formal") to the front of "t"'s list, and
   decrement "t.next_formal". *)

PROCEDURE Lookup(t: T; nm: Atom.T): INTEGER;
(* Return the integer paired with the first occurrence of "nm" in "t"'s list,
   or return 0 if there are no such occurrences. *)

END StackTbl.
