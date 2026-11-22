(* $Id: IntTriple.i3,v 1.1 2007/05/31 12:00:54 mika Exp $ *)

INTERFACE IntTriple;
IMPORT Word;

TYPE T = RECORD k1, k2, k3 : INTEGER END;

PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

CONST Brand = "IntTriple";

END IntTriple.
