GENERIC INTERFACE FractionBasic(R);
(*Copyright (c) 1996, m3na project

Abstract: Generic fraction type
          Instantiate with integers, polynomials

*)

FROM xUtils IMPORT Error;

(*==========================*)

TYPE
  T = RECORD n,d: R.T; END;  (* Numerator, Denominator *)

CONST
  Zero    =  T{n:=R.Zero,     d:=R.One};
  One     =  T{n:=R.One,      d:=R.One};
  MinusOne=  T{n:=R.MinusOne, d:=R.One};

<*INLINE*> PROCEDURE Add(READONLY x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(READONLY x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(READONLY x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(READONLY x:T):T;   (*return complex conjugate of x*)
           PROCEDURE Compare(READONLY x,y:T) : [-1..1];
           PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

<*INLINE*> PROCEDURE Mul(READONLY x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(READONLY x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)
<*INLINE*> PROCEDURE Mod(READONLY x,y:T):T RAISES {Error};  (*return x mod y*)
<*INLINE*> PROCEDURE DivMod(READONLY x,y:T;VAR r:T):T RAISES {Error};  (*return x/y and write the remainder (0) in r*)
<*INLINE*> PROCEDURE IntMod(READONLY x,y:T):T RAISES {Error};  (*return x mod y*)

<*INLINE*> PROCEDURE Square(READONLY x:T):T;         (*return x*x*)
<*INLINE*> PROCEDURE Scale (READONLY x:T; y:R.T):T;  (*return x*y*)

(*==========================*)
END FractionBasic.
