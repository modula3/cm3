GENERIC INTERFACE PolynomialFast(V,R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

FROM NADefinitions IMPORT Error;
(*==========================*)

CONST
  Brand = "Polynomial";

TYPE
  (*interpretation is: a[0] + a[1]*xi + a[2]* xi^2...a[n]*xi^n *)
  (*text form is: T4{a0,a1,a2,a3} *)

  (*this is not only a reuse because of laziness,
    more than this, a polynomial can be treated as vector
    and behaves like a vector of arbitrary size*)
  TBody = V.TBody;
  T     = V.T;

(*
CONST
  Zero    =  TBody{R.Zero};
  One     =  TBody{R.One};
*)

VAR
  Zero    : T;
  One     : T;

PROCEDURE New(n:CARDINAL):T;    (*make a poly for a0..an*)
PROCEDURE Copy(x:T):T;       (*copy x to a New poly*)
(*
PROCEDURE Zero(x:T);          (*set x to zeros*)
PROCEDURE One (x:T);          (*set x to 1*)
*)

PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN;  (*return x=y*)

PROCEDURE Add(x,y:T):T;  (*return x+y*)
PROCEDURE Sub(x,y:T):T;  (*return x-y*)
CONST Neg = V.Neg;

CONST Scale = V.Scale;

PROCEDURE Mul(x,y:T):T;  (*return x*y*)
PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y if possible*)
(*PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return x mod y*)*)
PROCEDURE DivMod(x,y:T;        (*compute x/y *)
               VAR r:T):T;     (*giving quotient with remainder r*)

PROCEDURE Eval(x:T;           (*eval this polynomial*)
              xi:R.T          (*at this point*)
               ):R.T;

PROCEDURE Derive
              (x:T;           (*differentiate polynomial*)
               ):T;
PROCEDURE EvalDerivative
              (x:T;          (*Eval this polynomial*)
              xi:R.T;               (*for this argument*)
          VAR pd:ARRAY OF R.T;      (*returning x(xi), x'(xi)...*)
               );
(*==========================*)
END PolynomialFast.
