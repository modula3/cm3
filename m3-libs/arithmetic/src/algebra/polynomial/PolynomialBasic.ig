GENERIC INTERFACE PolynomialBasic(R, V);
(*Arithmetic for Modula-3, see doc for details *)

FROM Arithmetic IMPORT Error;
(*==========================*)

TYPE
  (*this is not only a reuse because of laziness, more than this, a
     polynomial can be treated as vector and behaves like a vector of
     arbitrary size*)
  TBody = V.TBody;
  T = V.T;
  QuotRem = RECORD quot, rem: T END;

<*INLINE*>
PROCEDURE IsZero (x: T): BOOLEAN;
PROCEDURE Equal (x, y: T): BOOLEAN; (*return x=y*)
PROCEDURE Compare (x, y: T):
  [-1 .. 1];                     (*a dummy to let Fraction module work*)

PROCEDURE Add (x, y: T): T;      (*return x+y*)
PROCEDURE Sub (x, y: T): T;      (*return x-y*)
CONST Neg = V.Neg;

CONST Scale = V.Scale;

PROCEDURE Mul (x, y: T): T;      (*return x*y*)
PROCEDURE Div (x, y: T): T
  RAISES {Error};                (*return x/y if possible, will fail for
                                    floating point numbers often*)
PROCEDURE Mod (x, y: T): T RAISES {Error}; (*return x mod y*)
PROCEDURE DivMod (x, y: T): QuotRem RAISES {Error};
(*compute quotient x/y and remainder*)

PROCEDURE Eval (x: T;            (*eval this polynomial*)
                xi: R.T          (*at this point*)
  ): R.T;

PROCEDURE Derive (x: T;          (*differentiate polynomial*)
  ): T;
PROCEDURE EvalDerivative (x : T;    (*Eval this polynomial*)
                          xi: R.T;  (*for this argument*)
                          n: CARDINAL;  (*with this number of derivatives*)
  ): REF ARRAY OF R.T;           (*returning x(xi), x'(xi)...*)

PROCEDURE Compose (x, y: T;      (*y(x) - apply y on the values of x*)
  ): T;

(*==========================*)
END PolynomialBasic.
