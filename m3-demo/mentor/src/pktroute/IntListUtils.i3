(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Fri Jul 16 15:11:42 PDT 1993 by heydon                   *)

INTERFACE IntListUtils;

IMPORT IntList;

PROCEDURE ToText(l: IntList.T): TEXT;
(* Return the list "l" represented as a text of the form "[l1, l2, ..., ln]",
   where the "l_i" are the elements of "l". *)

END IntListUtils.
