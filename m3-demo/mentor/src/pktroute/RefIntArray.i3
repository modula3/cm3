(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sat Jul 17 15:49:37 PDT 1993 by heydon                   *)

INTERFACE RefIntArray;

TYPE
  IntArray = ARRAY OF INTEGER;
  T = REF IntArray;

PROCEDURE ToText(ia: REF IntArray): TEXT;
(* Return the elements of the array "ia" as a text of the form "[ia[0], ia[1],
   ..., ia[LAST(ia)]". *)

END RefIntArray.
