(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
 
(* Last modified on Fri Jan 18  9:09:22 PST 1991 by mjordan    *)

INTERFACE PropertyV;

IMPORT Property;

(* This interface provides a procedural interface to a Property.Set.
   It specifies VAR (*inout*) parameters to denote the set, so that
   NIL can denote the uninitialised (empty) set. The Put procedure
   creates the empty Set object if the input set is NIL. The
   specification is otherwise as per Property. *)

CONST
  NullSet: Set = NIL; (* usage: VAR s := Property.NullSet; *)

TYPE
  Set = Property.Set;

PROCEDURE Put(VAR (*inout*) ps: Set; r: REFANY);
PROCEDURE Remove(VAR (*inout*) ps: Set; tc: CARDINAL);
PROCEDURE Get(ps: Set; tc: CARDINAL): REFANY;
PROCEDURE RemoveSub(VAR (*inout*) ps: Set; tc: CARDINAL);
PROCEDURE GetSub(ps: Set; tc: CARDINAL): REFANY;

END PropertyV.

