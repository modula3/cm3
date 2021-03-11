GENERIC INTERFACE Ref(Elem);
IMPORT Word;

TYPE T <: REFANY;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE New(READONLY a : Elem.T) : T;
PROCEDURE Get(a : T) : Elem.T;

CONST Brand = "Ref(" & Elem.Brand & ")";

END Ref.  
