(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Jun 23 14:03:43 PDT 1993 by steveg *)

INTERFACE IntRef;

TYPE
  T = RECORD key: INTEGER; value: REFANY END;

PROCEDURE Compare(a, b: T): [-1..1];

END IntRef.
