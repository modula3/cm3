GENERIC INTERFACE RootBasic(P,R);
(*Copyright (c) 1996, m3na project

Abstract: Arithmetic on the roots of polynomials
          E.g. from given polynomials x and y
          with the roots X={x1,...,xn} and
          Y={y1,...,ym} respectively,
          we can compute the polynomials
          that have the roots
          {xi+eta : xi in X and eta in Y}
          {xi*eta : xi in X and eta in Y}
          {xi^n   : xi in X}
          etc.
*)

FROM xUtils IMPORT Error;
(*==========================*)

CONST
  Brand = "Root";

TYPE
  (*interpretation is: a[0] + a[1]*xi + a[2]* xi^2...a[n]*xi^n *)

  TBody = P.TBody;
  T = REF TBody;

(* It's not possible to obtain a pointer to a constant array.
   We can not turn T from a reference type to an array type,
   because some routines have to return a result via a VAR parameter.
CONST
  Zero    =  TBody{R.Zero,R.One};
  One     =  TBody{R.MinusOne,R.One};
*)

VAR
  (*CONST*) Zero : T;
  (*CONST*) One  : T;

CONST New  = P.New;
CONST Copy = P.Copy;

<*INLINE*>
PROCEDURE IsZero(x:T):BOOLEAN;
CONST Equal=P.Equal;

PROCEDURE Add(x,y:T):T;  (*return x+y*)
PROCEDURE Sub(x,y:T):T;  (*return x-y*)
PROCEDURE Neg(x:T):T;    (*return -x *)

PROCEDURE Mul(x,y:T):T;  (*return x*y*)
PROCEDURE Div(x,y:T):T RAISES {Error};  (*return x/y if possible, will fail for floating point numbers often*)
PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)
PROCEDURE Mod(x,y:T):T RAISES {Error};  (*return x mod y*)
PROCEDURE DivMod(x,y:T;                 (*compute x/y *)
              VAR r:T):T RAISES {Error};   (*giving quotient with remainder r (always zero)*)

PROCEDURE ElimMultRoots(x:T):T;
(*Eliminate multiple roots
  Can be used for simplifying polynomials after some basic operations.*)

PROCEDURE PowN(READONLY x:T;
                        y:CARDINAL):T;  (*return x^y*)
(*The number of roots doesn't change,
  thus PowN(x,2) is very different from Mul(x,x)
  because in the latter case every root is multiplied with _each_ other.*)

CONST RootOf = P.Compose;
(**)

TYPE
  PowerSumSeq = ARRAY OF R.T;

PROCEDURE ToPowerSumSeq(x:T):REF PowerSumSeq;
(*For a given set of roots x1,...,xn (represented by a polynomial)
  calculate the sum x1^j+...+xn^j for j from 1 to n
  Since for any of xi holds 0=a[0]+a[1]*xi+a[2]*xi^2+...+a[n]*xi^n,
  which means a[n]*xi^n=-(a[0]+a[1]*xi+a[2]*xi^2+...+a[n-1]*xi^(n-1)),
  one can easily continue the sequence of these power sums
  by a linear recurrence.
  *)

PROCEDURE FromPowerSumSeq(READONLY x:PowerSumSeq):T RAISES{Error};
(*Inverse to ToPowerSumSeq
  Err.indivisible is raised if x is not a sequence of power sums*)


(*==========================*)
END RootBasic.
