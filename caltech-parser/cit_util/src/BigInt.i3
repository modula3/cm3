(* $Id$
   For complete documentation, see IntForRat.i3.
   There is practically no size limitation on "BigInt.T"s, however.
*)
INTERFACE BigInt;
IMPORT Word;

CONST Brand = "BigInt";

TYPE T        <: Public;
TYPE Natural  = T;

TYPE Public = ROOT;

TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE New(x : INTEGER) : T;
PROCEDURE Div(a, b : T) : T;
PROCEDURE Mul(a, b : T) : T;
PROCEDURE Add(a, b : T) : T;
PROCEDURE Abs(a : T) : T;
PROCEDURE Mod(a, b : T) : T;
PROCEDURE Sign(a : T) : CompRet;

PROCEDURE Format(a : T; base : CARDINAL := 10) : TEXT;

VAR (* CONST *) Zero, One : T;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE ToLongReal(a : T) : LONGREAL;
PROCEDURE Max(a, b : T) : T;
PROCEDURE Min(a, b : T) : T;
PROCEDURE Divide(a, b : T; VAR q, r : T);
PROCEDURE ToInt(a : T) : INTEGER RAISES { OutOfRange };

EXCEPTION OutOfRange;

END BigInt.
