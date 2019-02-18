INTERFACE KeyObject;
IMPORT Word;

(* this interface allows a standard table to be constructed
   using an object that has not been declared in an interface
   (e.g. because it's in a generic implementation, or customized). *)

TYPE
  T = OBJECT METHODS
    hash(): Word.T;
    equal(other: T): BOOLEAN;
  END;

PROCEDURE Hash(key: T): Word.T;
PROCEDURE Equal(k1, k2: T): BOOLEAN;

CONST
  Brand = "KeyObject";

END KeyObject.
