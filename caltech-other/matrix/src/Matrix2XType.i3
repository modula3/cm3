(* $Id$ *)

INTERFACE Matrix2XType;
IMPORT Random;

TYPE T = EXTENDED;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2XType";

END Matrix2XType.
