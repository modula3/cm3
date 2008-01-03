(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListExtras.ig,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

GENERIC INTERFACE ListExtras(Elem, ElemList);
CONST Brand = "(ListExtras " & Elem.Brand & ")";
TYPE T = ElemList.T;

PROCEDURE MemberDelD(VAR l: T; e: Elem.T): BOOLEAN;
PROCEDURE LastDelD(VAR l: T): Elem.T;

PROCEDURE DeleteD(l: T; READONLY e: Elem.T): T;

END ListExtras.
