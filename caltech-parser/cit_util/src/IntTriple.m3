MODULE IntTriple;
IMPORT Word;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(a.k1, Word.Plus(a.k2, a.k3))
  END Hash;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

BEGIN END IntTriple.
