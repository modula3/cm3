(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Sun May 14 19:55:17 PDT 1995 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "Set" is a generic interface defining sets of "Elem.T"'s. *)

GENERIC INTERFACE Set(Elem);
(* Where "Elem.T" is a type that is not an open array type and "Elem" contains

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".
*)

CONST Brand = "(Set " & Elem.Brand & ")";

TYPE 
  Public = OBJECT METHODS
    fromArray(READONLY a: ARRAY OF Elem.T): T;
    copy(): T;
    member(e: Elem.T): BOOLEAN;
    insert(e: Elem.T): BOOLEAN;
    delete(e: Elem.T): BOOLEAN;
    size(): CARDINAL;
    isEmpty(): BOOLEAN;
    subset(s2: T): BOOLEAN;
    equal(s2: T): BOOLEAN;
    intersect(s2: T): BOOLEAN;
    union(s2: T): T;
    intersection(s2: T): T;
    diff(s2: T): T;
    unionD(s2: T): T;
    intersectionD(s2: T): T;
    diffD(s2: T): T;
    iterate(): Iterator;
  END;
  T <: Public;
  Iterator = OBJECT METHODS
    next(VAR e: Elem.T): BOOLEAN
  END;

(* A "Set(Elem)" is a set of "Elem.T"'s.  "Elem.T"'s that are equivalent
   under "Elem.Equal" are treated as equivalent by "Set";
   for example, if you are creating a set with an "Elem.T" of "TEXT",
   you are likely to want "Text.Equal" as the equivalence relation.
   The equivalence relation must be time-invariant.  For example,
   it can't depend on the values of particular references since some
   garbage collectors may move "REF" values.

   Formally, a set "s" has the component

| set(s) `a set of equivalence classes of "Elem.T"'s`.

   We will use "equiv(e)" to denote the equivelance class containing
   an "Elem.T" "e".

   For efficiency, a set is not monitored: it is up to the clients
   to avoid illegal concurrent accesses on the methods of a set.  A
   set's "insert" and "delete" methods have side-effects on the set,
   so can't be performed concurrently with any other method of that
   set or of an iterator on that set.  An iterator's "next" method
   has side-effects on the iterator, and is also considered to be a
   side-effect free operation on the parent set.

   The methods of an object "s" of type "Set.T" have the following
   specifications:

   The call "s.fromArray(a)" causes "set(s)" to contain exactly
   the equivalence classes containing all the elements of the array "a".

   The call "s.copy()" returns a set "s2" whose abstract state "set(s2)" 
   is the same as "set(s)".

   The call "s.member(e)" returns "TRUE" iff "e" is in an equivalence
   class in "set(s)".

   The call "s.insert(e)" returns "TRUE" and does not modify "s" if
   "equiv(e)" is in "set(s)"; otherwise it adds "equiv(e)" to "set(s)"
   and returns "FALSE".

   The call "s.delete(e)" ensures that "set(s)" does not contain
   "equiv(e)", returning "TRUE" iff "set(s)" contained "equiv(e)"
   before the call.

   The call "s.isEmpty()" returns "TRUE" iff "set(s)" is the empty set. 

   The call "s.size()" returns the cardinality of "set(s)".

   The call "s.subset(s2)" returns "TRUE" iff "set(s)" is a subset of
   "set(s2)".

   The call "s.equal(s2)" returns "TRUE" iff "set(s)" and "set(s2)" are the
   same set.

   The call "s.union(s2)" returns a new set "s3" such that "set(s3)" is
   the union of "set(s)" and "set(s2)".

   The call "s.intersection(s2)" returns a new set "s3" such that
   "set(s3)" is the intersection of "set(s)" and "set(s2)".

   The call "s.diff(s2)" returns a set "s3" such that "set(s3)"
   contains all equivalence classes in "set(s)" but not in "set(s2)".

   The call "s.unionD(s2)" modifies "s" so that "set(s)" contains the
   union of "set(s`)" and "set(s2)", where "s`" is the state of "s"
   immediately before the call, and returns the modified set.

   The call "s.intersectionD(s2)" modifies "s" so that "set(s)"
   contains the intersection of "set(s`)" and "set(s2)", where "s`" is
   the state of "s" immediately before the call, and returns the
   modified set.

   The call "s.diffD(s2)" modifies "s" so that "set(s)" contains no
   equivalence classes that are in "set(s2)", and returns the modified
   set.

   The call "s.iterate()" returns an iterator, which is an object
   that can be used to iterate over the elements in "s".  See below
   for the specification of the "Iterator" type.

   If "it" is the result of the call "s.iterate()", then the call
   "it.next(e)" selects an element from "s" that has not already been
   returned by "it", sets "e" to that element, and returns
   "TRUE".  If no entries remain, the call returns "FALSE" without
   setting "e".  It is a checked runtime error to call "next"
   after it has returned "FALSE".
*)

PROCEDURE Equal(s1, s2: T): BOOLEAN;
(* Equivalent to "s1.equal(s2)".  Exported so that "Set"'s may be used as
   arguments to generic interfaces. *)

END Set.


