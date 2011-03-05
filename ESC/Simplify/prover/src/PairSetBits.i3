(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb 23 13:45:40 PST 1996 by detlefs                  *)

INTERFACE PairSetBits;

IMPORT IdSet;

TYPE T <: REFANY;

(* Represents (an approximaxtion of) a set of pairs of enodes. *)

PROCEDURE Empty(): T;
(* Return the empty set of pairs. *)

PROCEDURE SingletonSet(id: INTEGER): IdSet.T;
(* Returns the singleton set containing only "id". *)

PROCEDURE IsEmpty(ps: T): BOOLEAN;
(* Returns "TRUE" iff "ps" is empty. *)

PROCEDURE MakeEmpty(VAR ps: T);
(* Ensures that "ps" represents the empty set. *)

PROCEDURE Copy(ps: T): T;
(* Returns a copy of "ps". *)

PROCEDURE Size(ps: T): INTEGER;
(* Returns the number of pairs in the set. *)

PROCEDURE AddSetCrossSetD(ps: T; s1, s2: IdSet.T);
(* Destructively union the cross product of "s1" and "s2" into "ps". *)

PROCEDURE AddSetCrossElemD(ps: T; s: IdSet.T; id: INTEGER);
(* Destructively union the cross product of "s" and the singleton set
   containing "id" into "ps". *)

PROCEDURE AddElemCrossSetD(ps: T; id: INTEGER; s: IdSet.T);
(* Destructively union the cross product of the singleton set
   containing "id" and "s" into "ps". *)

PROCEDURE UnionD(VAR ps1: T; ps2: T);
(* ps1 := union of ps1 and ps2. *)

PROCEDURE Member(ps: T; id1, id2: INTEGER): BOOLEAN;
(* Returns "TRUE" iff the pair "(id1, id2)" is a member of "ps". *)

END PairSetBits.
