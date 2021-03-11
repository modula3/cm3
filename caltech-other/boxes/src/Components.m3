(* $Id$ *)

MODULE Components;
IMPORT Word, RouteID;

PROCEDURE Hash(t : T) : Word.T = BEGIN RETURN RouteID.Hash(t.id()) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;


BEGIN END Components.
