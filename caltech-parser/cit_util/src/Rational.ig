(* $Id$ *)
GENERIC INTERFACE Rational(BaseInt);
IMPORT Word;

(* A Rational.T represents a rational number whose numerator is of
   type BaseInt.T and whose denominator is of type BaseInt.Natural.  
   Abstractly, a BaseInt.T represents any integer and a BaseInt.Natural
   represents any natural number.

   BaseInts must be read-only.  
   The BaseInt interface must contain the following:

   A text constant Brand.

   Procedures

     Compare(a,b) : [-1..1]
     Equal(a,b) : BOOLEAN
     New(x : INTEGER) : BaseInt.T

     Div(a,b) : BaseInt.T
     Mul(a,b) : BaseInt.T
     Add(a,b) : BaseInt.T

     Abs(a) : BaseInt.T
     Mod(a,b) : BaseInt.T
     Sign(a) : [-1..1]

     Format(a, base : CARDINAL) : TEXT

   Sign(0) may return -1, 0, or 1.

   The parameter mode of each of the procedures can be VALUE or READONLY,
   but not VAR.
 *)

(* You should probably treat a T as being opaque.
   The routines in this interface will return Ts in lowest terms *)

TYPE 
  T = RECORD 
    (* CONST *) n : BaseInt.T; 
    (* CONST *) d : BaseInt.Natural
  END;

CONST Brand = "Rational of " & BaseInt.Brand;

TYPE CompRet = [-1..1];

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Compare(READONLY a, b : T) : CompRet;

PROCEDURE Mul(READONLY a, b : T) : T;
PROCEDURE Add(READONLY a, b : T) : T;
PROCEDURE Sub(READONLY a, b : T) : T;
PROCEDURE Div(READONLY a, b : T) : T;
PROCEDURE Reciprocal(READONLY a : T) : T;

(* make a new T representing the integer a *)
PROCEDURE NewInt(READONLY a : BaseInt.T) : T;

(* make a new T representing the fraction n/d *)
PROCEDURE New(READONLY n : BaseInt.T; READONLY d : BaseInt.Natural) : T;

(* make a new T representing the fraction n/d, where n and d are INTEGERs *)
PROCEDURE NewSimple(n : INTEGER; d : [1..LAST(CARDINAL)]) : T;

(* some bonus things... *)
PROCEDURE BaseGCD(a, b : BaseInt.T) : BaseInt.T;
PROCEDURE BaseLCM(a, b : BaseInt.T) : BaseInt.T;

PROCEDURE Format(a : T; base : CARDINAL := 10) : TEXT;

PROCEDURE Hash(a : T) : Word.T;

PROCEDURE ToLongReal(a : T) : LONGREAL;

END Rational.
