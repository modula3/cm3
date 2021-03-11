(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

GENERIC INTERFACE ListF(Elem, ElemList);
IMPORT Word;
CONST
  Brand = "(ListF " & Elem.Brand & ")";
TYPE
  T = ElemList.T;
PROCEDURE Format(l: T): TEXT; (* assumes Elem.Format exists *)
PROCEDURE Equal(l1,l2: T): BOOLEAN;
PROCEDURE Hash(l: T): Word.T;  (* assumes Elem.Hash *)

(* the following used to be in ListExtras.ig *)
PROCEDURE MemberDelD(VAR l: T; e: Elem.T): BOOLEAN;
PROCEDURE LastDelD(VAR l: T): Elem.T;

PROCEDURE DeleteD(l: T; READONLY e: Elem.T): T;
(* destructive delete.
   NB: part of "l" after "e" is untouched. *)
END ListF.
