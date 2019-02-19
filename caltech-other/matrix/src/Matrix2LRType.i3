(* $Id$ *)

INTERFACE Matrix2LRType;
IMPORT Random;

TYPE T = LONGREAL;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2LRType";

END Matrix2LRType.
