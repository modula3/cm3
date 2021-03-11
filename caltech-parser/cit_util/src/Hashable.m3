(* $Id$ *)

MODULE Hashable;
IMPORT Word;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hash() END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

BEGIN END Hashable.
