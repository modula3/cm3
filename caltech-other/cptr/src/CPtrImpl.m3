UNSAFE MODULE CPtrImpl;
IMPORT Word;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN
    RETURN LOOPHOLE(a,Word.T);
  END Hash;

PROCEDURE Equal(a,b : T) : BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

BEGIN
END CPtrImpl.
