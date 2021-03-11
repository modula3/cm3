(* $Id$ *)

INTERFACE Stuff;
IMPORT Word;

(* this is an interface you can use when you want to use OO rather than
   generics.  Simply subtype a Stuff.T and use it for your own purposes... *)


TYPE
  T = OBJECT METHODS
    equal(b : T) : BOOLEAN;
    hash() : Word.T;
    compare(b : T) : [-1..1];
  END;

PROCEDURE Hash(a : T) : Word.T;        (* a.hash() *)
PROCEDURE Equal(a, b : T) : BOOLEAN;   (* a.equal(b) *)
PROCEDURE Compare(a, b : T) : [-1..1]; (* a.compare(b) *)

CONST Brand = "Stuff";

END Stuff.
