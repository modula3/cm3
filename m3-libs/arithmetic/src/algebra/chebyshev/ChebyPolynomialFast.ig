GENERIC INTERFACE ChebyPolynomialFast(R, P);
(*Arithmetic for Modula-3, see doc for details *)

FROM Arithmetic IMPORT Error;

CONST Brand = "Cheby" & P.Brand;

TYPE
  TBody = P.TBody;
  T = P.T;
  Ftn = PROCEDURE (x: R.T): R.T;

PROCEDURE Expand (func: Ftn;       (*differentiate polynomial*)
                  m   : CARDINAL;  (*order*)
  ): T;

(*find the minimum upper exponent that still guarants a good
   approximation*)
(*if you abort the sum before the determined exponent the approximation
   error will get at most 'prec' higher*)
PROCEDURE FindUpperExp (x: T; prec: R.T := FLOAT(0.00001D0, R.T); ):
  CARDINAL;

(*the result of FindUpperExp can be passed to 'm'*)
PROCEDURE Abort (x: T;           (*abort the expansion*)
                 m: CARDINAL;    (*before the m-th term*)
  ): T;

PROCEDURE Eval (x : T;           (*evaluate this polynomial*)
                xi: R.T;         (*at this point*)
  ): R.T RAISES {Error};

PROCEDURE Derive (x: T;          (*differentiate polynomial*)
  ): T;

PROCEDURE Integrate (x: T;       (*integrate polynomial*)
  ): T;

(*========================*)
END ChebyPolynomialFast.
