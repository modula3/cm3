(* $Id$ *)

MODULE Matrix2RType;
IMPORT Fmt;
IMPORT Random;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.Real(t) END Format;

PROCEDURE Rand(r : Random.T) : T = 
  BEGIN RETURN r.real() END Rand;

BEGIN END Matrix2RType.
