(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxSet.i3                                              *)
(* Last Modified On Mon Mar 14 16:40:18 PST 1994 By kalsow     *)

INTERFACE MxSet;

IMPORT Mx;

TYPE T   <: REFANY;  (* SET OF Elt *)
TYPE Elt = Mx.Unit;

PROCEDURE New (): T;
(* return a new, empty set *)

PROCEDURE Insert (t: T;  e: Elt);
(* add 'e' to 't' *)

PROCEDURE IsMember (t: T;  e: Elt): BOOLEAN;
(* return TRUE iff 'e' is a member of 't' *)

PROCEDURE ToList (t: T): Mx.UnitList;
(* return a list of the elts of 't' *)

END MxSet.

