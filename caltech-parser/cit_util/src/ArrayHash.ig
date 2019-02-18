GENERIC INTERFACE ArrayHash(Elem);
IMPORT Word;
TYPE
  T = REF ARRAY OF Elem.T;
CONST
  Brand = Elem.Brand & "Array";

PROCEDURE Hash(a: T): Word.T;
PROCEDURE Equal(a,b: T): BOOLEAN;
END ArrayHash.
