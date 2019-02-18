(* $Id$ *)

MODULE IntForRat;
IMPORT Integer;
IMPORT Fmt;
IMPORT Word;

PROCEDURE Compare(a, b : T) : CompRet = 
  BEGIN RETURN Integer.Compare(a,b) END Compare;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE New(x : INTEGER) : T = BEGIN RETURN x END New;

PROCEDURE Div(a, b : T) : T = BEGIN RETURN a DIV b END Div;

PROCEDURE Mul(a, b : T) : T = BEGIN RETURN a * b END Mul;

PROCEDURE Add(a, b : T) : T = BEGIN RETURN a + b END Add;

PROCEDURE Abs(a : T) : T = BEGIN RETURN ABS(a) END Abs;

PROCEDURE Mod(a, b : T) : T = BEGIN RETURN a MOD b END Mod;

PROCEDURE Sign(a : T) : CompRet = BEGIN RETURN Compare(a,0) END Sign;

PROCEDURE Format(a : T; base : CARDINAL) : TEXT =
  BEGIN RETURN Fmt.Int(a, base) END Format;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Integer.Hash(a) END Hash;

PROCEDURE Max(a, b : T) : T = BEGIN RETURN MAX(a,b) END Max;

PROCEDURE Min(a, b : T) : T = BEGIN RETURN MIN(a,b) END Min;

PROCEDURE ToLongReal(a : T) : LONGREAL = 
  BEGIN RETURN FLOAT(a,LONGREAL) END ToLongReal;

PROCEDURE Divide(a, b : T; VAR q, r : T) =
  BEGIN q:=a DIV b; r:=a MOD b; END Divide;
BEGIN END IntForRat.

