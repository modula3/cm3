(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Jun 23 14:01:07 PDT 1993 by steveg *)

MODULE TextRef;

IMPORT Text;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    RETURN Text.Compare(a.key, b.key);
  END Compare;

BEGIN
END TextRef.
