(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: ListF.ig,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC INTERFACE ListF(Elem, ElemList);
IMPORT Word;
CONST
  Brand = "(ListF " & Elem.Brand & ")";
TYPE
  T = ElemList.T;
PROCEDURE Format(l: T): TEXT; (* assumes Elem.Format exists *)
PROCEDURE Equal(l1,l2: T): BOOLEAN;
PROCEDURE Hash(l: T): Word.T;  (* assumes Elem.Hash *)
END ListF.
