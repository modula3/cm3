(* Copyright (C) 1994 Digital Equipment Corporation.        *)
(* Distributed only by permission.                          *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri Sep 23 09:32:59 PDT 1994 by heydon  *)
(*      modified on Thu Mar 10 15:46:33 PST 1994 by kalsow  *)
(*      modified on Tue Dec  7 17:39:14 PST 1993 by mcjones *)

(* An "Atom.T" is a unique representative for a set of equal texts
   (like a Lisp atomic symbol)
   \index{atomic symbol}
   \index{symbolic expression!atom}
*)

INTERFACE Atom;

TYPE T <: REFANY;

CONST Brand = "Atom-1.0";

PROCEDURE FromText(t: TEXT): T;
(* Return the unique atom "a" such that for any text "u", if
   "Text.Equal(u, t)", then "FromText(u) = a".  *)

PROCEDURE ToText(a: T): TEXT;
(* Return a text "t" such that "FromText(t) = a". *)

PROCEDURE Equal(a1, a2: T): BOOLEAN;
(* Return "a1 = a2". *)

PROCEDURE Hash(a: T): INTEGER;
(* Return a hash code for "a" by taking the image of "ToText(a)"
   under some fixed hash function.  *)

PROCEDURE Compare(a1, a2: T): [-1..1];
(* Cause a checked runtime error. *)

END Atom.

(* "Compare" causes a checked runtime error because there is no
   default order on atoms. *)
