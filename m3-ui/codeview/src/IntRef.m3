(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Jun 23 14:09:35 PDT 1993 by steveg *)

MODULE IntRef;

IMPORT Integer;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    RETURN Integer.Compare(a.key, b.key);
  END Compare;

BEGIN
END IntRef.
