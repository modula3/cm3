(* $Id$ *)

MODULE Stuff;
IMPORT Word;


PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

PROCEDURE Compare(a, b : T) : [-1..1] = BEGIN RETURN a.compare(b) END Compare;

BEGIN END Stuff.
