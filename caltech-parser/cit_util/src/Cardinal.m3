(* $Id$ *)

MODULE Cardinal;
IMPORT Integer;
IMPORT Word;

PROCEDURE Compare(a, b : T) : CompRet = 
  BEGIN RETURN Integer.Compare(a,b) END Compare; 

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN RETURN Integer.Equal(a,b) END Equal;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN Integer.Hash(a) END Hash;

BEGIN END Cardinal.
