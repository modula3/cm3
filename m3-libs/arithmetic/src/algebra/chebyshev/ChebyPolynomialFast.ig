GENERIC INTERFACE ChebyPolynomialFast(P,R);
(*
Abstract:  Chebyshev polynomials and
           approximation of functions by sums of chebyshev polynomials

12/13/95   Harry George      Initial version
1/22/96    Harry George      Change to m3na project
*)

FROM xUtils IMPORT Error;

(*=================================*)
(* Chebyshev Approximations        *)
(*=================================*)

TYPE
  TBody = P.TBody;
  T     = P.T;
  Ftn = PROCEDURE (x:R.T):R.T;

CONST
  New    = P.New;
  Copy   = P.Copy;

  IsZero = P.IsZero;
  Equal  = P.Equal;

  Add    = P.Add;
  Sub    = P.Sub;
  Neg    = P.Neg;

  Scale  = P.Scale;

PROCEDURE Expand
           (func:Ftn;         (*differentiate polynomial*)
               m:CARDINAL;    (*order*)
               ):T;

(*find the minimum upper exponent that still guarants a good approximation*)
(*if you abort the sum before the determined exponent
  the approximation error will get at most 'prec' higher*)
PROCEDURE FindUpperExp
              (x:T;
            prec:R.T:=FLOAT(0.00001D0,R.T);
               ):CARDINAL;

(*the result of FindUpperExp can be passed to 'm'*)
PROCEDURE Abort(x:T;          (*abort the expansion*)
                m:CARDINAL;   (*before the m-th term*)
                ):T;

PROCEDURE Eval(x:T;           (*eval this polynomial*)
              xi:R.T;         (*at this point*)
               ):R.T
                RAISES {Error};

PROCEDURE Derive
              (x:T;           (*differentiate polynomial*)
               ):T;

PROCEDURE Integrate
              (x:T;           (*differentiate polynomial*)
               ):T;

(*========================*)
END ChebyPolynomialFast.
