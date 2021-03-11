(* $Id$ *)

INTERFACE RouteID;
IMPORT Word;

TYPE T = Word.T;

PROCEDURE Equal (READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash (READONLY a : T) : Word.T;
PROCEDURE Format(READONLY a : T) : TEXT;

CONST Brand = "RouteID";

(* the special value Nil is used for routes that can't be ripped up *)
CONST Nil : T = 0-1;

(* get a new one that is guaranteed unique and non-Nil *)
PROCEDURE New() : T;

END RouteID.
