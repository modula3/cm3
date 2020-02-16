(* $Id$ *)
INTERFACE BDDPair;
IMPORT BDD, Word;

TYPE
  T = ARRAY [0..1] OF BDD.T;

CONST
  Brand = "Pair of " & BDD.Brand;

PROCEDURE Equal( a, b : T ) : BOOLEAN;

PROCEDURE Hash( x : T ) : Word.T;

END BDDPair.
