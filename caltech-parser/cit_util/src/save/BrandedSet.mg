GENERIC MODULE BrandedSet(Elem);
IMPORT Word;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    iter := a.iterate();
    e : Elem.T;
    hash := 0;
  BEGIN 
    WHILE iter.next(e) DO hash := Word.Plus(hash, Elem.Hash(e)) END;
    RETURN hash
  END Hash;

BEGIN END BrandedSet.

