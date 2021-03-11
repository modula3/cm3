(* $Id$ *)
INTERFACE IntForRat;
IMPORT Word;

(* Integer interface for use in generics.

   This implementation truncates its results MOD NUMBER(INTEGER)
   so that they are between FIRST(INTEGER) and LAST(INTEGER) inclusive.
   Other implementations may have different truncation or none at all.
*)

CONST Brand = "IntForRat";

TYPE T        = INTEGER;
TYPE Cardinal = CARDINAL;
TYPE Natural  = [1..LAST(CARDINAL)];

TYPE CompRet = [-1..1];

PROCEDURE Compare(a, b : T) : CompRet;
PROCEDURE Equal(a, b : T) : BOOLEAN;
PROCEDURE New(x : INTEGER) : T;
PROCEDURE Mul(a, b : T) : T;
PROCEDURE Add(a, b : T) : T;
PROCEDURE Abs(a : T) : T;

PROCEDURE Sign(a : T) : CompRet;
(* Sign(0) = 0 *)

PROCEDURE Mod(a, b : T) : T;
(* Mod(a,b) returns the unique r in [0,b) such that there is a
solution to a-r=b*x *)

PROCEDURE Div(a, b : T) : T;
(* a-Mod(a,b)=b*Div(a,b) *)

PROCEDURE Format(a : T; base : CARDINAL := 10) : TEXT;

CONST
  Zero = 0;
  One = 1;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE ToLongReal(a : T) : LONGREAL;
PROCEDURE Max(a, b : T) : T;
PROCEDURE Min(a, b : T) : T;
PROCEDURE Divide(a, b : T; VAR q, r : T);
(* equivalent to "q:=Div(a,b); r:=Mod(a,b);" but possibly more efficient *)

END IntForRat.
