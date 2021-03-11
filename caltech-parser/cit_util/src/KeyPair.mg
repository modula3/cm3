(* $Id$ *)

GENERIC MODULE KeyPair(Key1, Key2);
IMPORT Word;

PROCEDURE Equal(READONLY a , b : T) : BOOLEAN =
  BEGIN
    RETURN Key1.Equal(a.k1, b.k1) AND Key2.Equal(a.k2, b.k2) 
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    x := Word.Times(Key1.Hash(a.k1),169066571);
    y := Key2.Hash(a.k2);
  BEGIN 
    RETURN Word.Plus(x,y)
  END Hash;

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  BEGIN
    WITH c1 = Key1.Compare(a.k1, b.k1) DO
      IF c1 # 0 THEN 
        RETURN c1
      ELSE 
        RETURN Key2.Compare(a.k2, b.k2)
      END
    END
  END Compare;

BEGIN END KeyPair.
