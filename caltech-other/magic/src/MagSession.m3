(* $Id$ *)

MODULE MagSession;
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.id() END Hash;

BEGIN END MagSession.
