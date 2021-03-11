GENERIC INTERFACE SettableRef(Elem);
IMPORT Word;

TYPE
  T = REF Elem.T;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE New(READONLY a : Elem.T) : T;
PROCEDURE Get(a : T) : Elem.T;

CONST Brand = "SettableRef(" & Elem.Brand & ")";

END SettableRef.  
