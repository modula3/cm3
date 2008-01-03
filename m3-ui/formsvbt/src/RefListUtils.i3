(* Copyright © 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  3 12:12:03 PDT 1993 by meehan                   *)

INTERFACE RefListUtils;

IMPORT RefList;

PROCEDURE Push (VAR list: RefList.T; item: REFANY);
PROCEDURE Pop (VAR list: RefList.T): REFANY;

PROCEDURE Equal (x, y: REFANY): BOOLEAN;
(* Compare "x" and "y". If they are both "RefList.T"'s, do a recursive
   comparison. *)

PROCEDURE Assoc (alist: RefList.T; item: REFANY): RefList.T;
(* An association list is a list of sublists. Return the first sublist
   "s" for which "Equal(item, s.head)" is "TRUE", or "NIL" otherwise. *)

PROCEDURE AssocQ (alist: RefList.T; item: REFANY): RefList.T;
(* Return the first sublist "s" for which "item = s.head" is "TRUE",
   or "NIL" otherwise. *)

PROCEDURE SetNth (list: RefList.T; n: CARDINAL; item: REFANY);
(* Store "item" as the "n"th element of "list". If "n >= RefList.Length(list)",
   this is a no-op. *)

PROCEDURE NthTail (list: RefList.T; n: CARDINAL): RefList.T;
(* Return the "n"th tail of "list". If "n >= RefList.Length(list)",
   this is a no-op. *)

PROCEDURE Delete (VAR list: RefList.T; item: REFANY);
(* Destructively remove all elements "e" of "list" for which
   "Equal (item, e)" is "TRUE", and set "list" to the result. *)

PROCEDURE DeleteQ (VAR list: RefList.T; item: REFANY);
(* Destructively remove all elements "e" of "list" for which
   "item = e" is "TRUE", and set "list" to the result. *)

END RefListUtils.
