GENERIC INTERFACE PolynomialBasic(R,V);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

FROM NADefinitions IMPORT Error;
(*==========================*)

CONST
  Brand = R.Brand & "Polynomial";

TYPE
  (*interpretation is: a[0] + a[1]*xi + a[2]* xi^2...a[n]*xi^n *)
  (*text form is: T4{a0,a1,a2,a3} *)

  (*this is not only a reuse because of laziness,
    more than this, a polynomial can be treated as vector
    and behaves like a vector of arbitrary size*)
  TBody = V.TBody;
  T     = V.T;
  QuotRem = RECORD quot, rem: T END;

(* It's not possible to obtain a pointer to a constant array.
   We can not turn T from a reference type to an array type,
   because some routines have to return a result via a VAR parameter.
CONST
  Zero    =  TBody{R.Zero};
  One     =  TBody{R.One};
*)

VAR
  (*CONST*) Zero : T;
  (*CONST*) One  : T;

PROCEDURE New(degree:CARDINAL):T;  (*make a poly for a0..an*)
CONST
  FromArray = V.FromArray;
  Copy      = V.Copy;

<*INLINE*>
PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN;  (*return x=y*)
PROCEDURE Compare(x,y:T) : [-1..1];  (*a dummy to let Fraction module work*)

PROCEDURE Add(x,y:T):T;  (*return x+y*)
PROCEDURE Sub(x,y:T):T;  (*return x-y*)
CONST Neg = V.Neg;

CONST Scale = V.Scale;

PROCEDURE Mul(x,y:T):T;  (*return x*y*)
PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y if possible, will fail for floating point numbers often*)
PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return x mod y*)
PROCEDURE DivMod(x,y:T):QuotRem RAISES {Error};  
             (*compute quotient x/y and remainder*)

PROCEDURE Eval(x:T;           (*eval this polynomial*)
               xi:R.T          (*at this point*)
               ):R.T;

PROCEDURE Derive(x:T;           (*differentiate polynomial*)
                 ):T;
PROCEDURE EvalDerivative(x:T;          (*Eval this polynomial*)
                xi:R.T;               (*for this argument*)
           VAR pd:ARRAY OF R.T;      (*returning x(xi), x'(xi)...*)
                );

PROCEDURE Compose(x,y:T;           (*y(x) - apply y on the values of x*)
                 ):T;

(*==========================*)
END PolynomialBasic.
