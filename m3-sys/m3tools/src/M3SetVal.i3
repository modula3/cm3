(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3SetVal;

TYPE
  T <: REFANY;

PROCEDURE NewEmpty      (n_elts: INTEGER): T;
PROCEDURE Compare       (a, b: T): INTEGER;
PROCEDURE Union         (a, b: T): T;
PROCEDURE Intersection  (a, b: T): T;
PROCEDURE Difference    (a, b: T): T;
PROCEDURE SymDifference (a, b: T): T;
PROCEDURE Include       (set: T;  elt: INTEGER): T;
PROCEDURE Exclude       (set: T;  elt: INTEGER): T;
PROCEDURE IsMember      (set: T;  elt: INTEGER): BOOLEAN;
   
END M3SetVal.
(*
   "Compare(a, b)" returns
      "-1" if "a < b",
      "0"  if "a = b",
      "+1" if "a > b", and
      "-99" otherwise.

  If the two sets passed to any of these functions differ in
  size, "NIL" is returned.

  All sets are zero-based.  That is, they are sets of "[0..n]".
*)
