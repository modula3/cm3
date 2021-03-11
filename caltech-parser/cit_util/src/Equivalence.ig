(*                                                                           *)
(*  Equivalence.ig                                                           *)
(*                                                                           *)
(*  Generic equivalence classes                                              *)
(*                                                                           *)
(*  Copyright (c) 2000 California Institute of Technology                    *)
(*  All rights reserved.                                                     *)
(*  Department of Computer Science                                           *)
(*  Pasadena, CA 91125.                                                      *)
(*                                                                           *)
(*  Author: Karl Papadantonakis <kp@caltech.edu>                             *)
(*                                                                           *)
(*  Permission to use, copy, modify, and distribute this software            *)
(*  and its documentation for any purpose and without fee is hereby          *)
(*  granted, provided that the above copyright notice appear in all          *)
(*  copies. The California Institute of Technology makes no representations  *)
(*  about the suitability of this software for any purpose. It is            *)
(*  provided "as is" without express or implied warranty. Export of this     *)
(*  software outside of the United States of America may require an          *)
(*  export license.                                                          *)
(* $Id$ *)

GENERIC INTERFACE Equivalence(Elem, ElemList, ElemElemTbl, ElemElemListTbl);

(*
A "T" represents an equivalence relation on the set of all "Elem.T"s.
A newly created "Default" has each "Elem.T" in its own equivalence
class.

Interface "Elem" is expected to have the following declaration:

| PROCEDURE Equal(e1, e2: Elem.T): BOOLEAN;

which defines the a priori equality of two elements.

The "Default" implementation is union-find with path compression using
hash tables.
*)

TYPE
  T = OBJECT METHODS
    equal(e1, e2: Elem.T): BOOLEAN;
(* are "e1" and "e2" members of the same equivalence class? *)

    identify(e1, e2: Elem.T): BOOLEAN;
(* join the two equivalence classes represented by "e1" and "e2".
   return "TRUE" iff they are already equal. *)

    canon(e: Elem.T): Elem.T;
(* return the canonical representative of the class containing "e".
    canon may change the data structure, so unmonitored concurrent calls
    are not allowed. *)

    iterate(): Iterator;
(* For each element which is not its own canonical representative,
   obtain that element as "alias", and
   its canonical representative as "canon". *)

    getClass(e: Elem.T): ElemList.T;
(* return a list of all elements in the class containing "e". *)

    toTable(): ElemElemTbl.T;
(* return a table mapping aliases to canonical elements. *)

    toRevTable(): ElemElemListTbl.T;
(* return a table mapping canonical elements to lists of aliases. *)

    initCopy(toCopy : T) : T;
(* initialize as a copy of toCopy; for a Default, call standard init first,
   e.g., write new := NEW(Default).init().initCopy(old) *)
  END;
  Iterator = OBJECT METHODS
    next(VAR alias, canon: Elem.T): BOOLEAN;
  END;

  Default <: T OBJECT METHODS
    init(sizeHint: CARDINAL := 0;
         leaderPreference: Preference := NIL): Default;
  END;

  Preference = OBJECT METHODS
    is(thisBetter, thanThis: Elem.T): BOOLEAN;
  END;

END Equivalence.
