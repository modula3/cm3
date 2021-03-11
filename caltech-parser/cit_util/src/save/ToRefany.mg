GENERIC MODULE ToRefany(Elem);
IMPORT ToRefanyClass AS Private;
IMPORT ToRefany;
IMPORT Word;

PROCEDURE Hash(a : REFANY) : Word.T = BEGIN RETURN Elem.Hash(a) END Hash;

PROCEDURE Equal(a , b : REFANY) : BOOLEAN =
  BEGIN RETURN Elem.Equal(a,b) END Equal;

BEGIN 
  ToRefany.AddType( Private.T { TYPECODE(Elem.T), Hash, Equal } )
END ToRefany.
