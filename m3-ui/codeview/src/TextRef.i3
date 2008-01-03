(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Jun 23 13:56:20 PDT 1993 by steveg *)

INTERFACE TextRef;

TYPE
  T = RECORD key: TEXT; value: REFANY END;

PROCEDURE Compare(a, b: T): [-1..1];

END TextRef.
