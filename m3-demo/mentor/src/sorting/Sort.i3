(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon May 11 08:47:06 1992 by mhb   *)
(*      modified on Thu Apr 30 18:55:52 PDT 1992 by johnh *)

INTERFACE Sort;

CONST
    MaxN = 1000;
   
TYPE
    Key  = INTEGER;
    Keys = ARRAY [ 0 .. MaxN+1 ] OF Key;

END Sort.
