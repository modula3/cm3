(* $Id$ *)

INTERFACE LRVector;

TYPE S = ARRAY OF LONGREAL;
TYPE T = REF S;

CONST Brand = "LRVector";

PROCEDURE Norm(v : T) : LONGREAL;

PROCEDURE Copy(a : T) : T;

END LRVector.
