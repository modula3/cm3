MODULE KeyObject;
IMPORT Word;

PROCEDURE Hash(key: T) : Word.T =
  BEGIN
    RETURN key.hash();
  END Hash;

PROCEDURE Equal(k1, k2: T) : BOOLEAN =
  BEGIN
    RETURN k1.equal(k2);
  END Equal;

BEGIN
END KeyObject.
