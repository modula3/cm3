GENERIC MODULE SettableRef(Elem);

IMPORT Word;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN Elem.Hash(a^) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN Elem.Equal(a^, b^) END Equal;

PROCEDURE New(READONLY a : Elem.T) : T =
  VAR res := NEW(T); BEGIN res^ := a; RETURN res END New;

PROCEDURE Get(a : T) : Elem.T = BEGIN RETURN a^ END Get;

BEGIN END SettableRef.
