(* $Id$ *)

INTERFACE RouteTag;
IMPORT Word;

(* just an interface, inherit and extend *)
TYPE 
  T = OBJECT METHODS
    hash() : Word.T;
    equal(to : T) : BOOLEAN;
    format() : TEXT;
  END;

CONST Brand = "RouteTag";

PROCEDURE Hash(a : T) : Word.T;        (* a.hash() *)
PROCEDURE Equal(a, b : T) : BOOLEAN;   (* a.equal(b) *)

END RouteTag.
