(* $Id$ *)

INTERFACE Cardinal;
IMPORT Word;

CONST Brand = "Cardinal";
TYPE T = CARDINAL;
TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE Hash(a : T) : Word.T;

END Cardinal.
