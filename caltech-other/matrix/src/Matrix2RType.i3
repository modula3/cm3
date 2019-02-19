(* $Id$ *)

INTERFACE Matrix2RType;
IMPORT Random;

TYPE T = REAL;

PROCEDURE Format(t : T) : TEXT;
PROCEDURE Rand(r : Random.T) : T;

CONST Brand = "Matrix2RType";

END Matrix2RType.
