(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last modified on Mon Jul 25 11:53:25 PDT 1994 by detlefs                  *)
(*      modified on Tue Feb 11 20:48:45 PST 1992 by muller                   *)

(* "SetList" is a generic interface defining sets of "Elem.T"'s,
   implemented as linked lists.  This implementations is appropriate
   only for small sets.
*)

GENERIC INTERFACE SetList(ElemSet);
(* WHERE "Elem.T" is a REF type and contains 

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

   "ElemSet = Set(Elem)".

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".
*)

TYPE 
  Public = ElemSet.T OBJECT METHODS
    init(): T;
  END;
  T <: Public;
  Iterator <: ElemSet.Iterator;

(* If "s" is an object of type "SetDefault.T", the expression

| NEW(SetDef.T).init()

   creates a new, empty set.

   The call "s.toRefList()" returns a "RefList.T" whose elements
   include an element from each of the equivalence classes in
   "set(s)", and no other elements. 
*)

END SetList.


