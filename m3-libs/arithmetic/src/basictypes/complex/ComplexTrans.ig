GENERIC INTERFACE ComplexTrans(R, C);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Transcendental functions of complex numbers. *)

FROM NADefinitions IMPORT Error;

TYPE T = C.T;

CONST
  Zero     = C.Zero;
  One      = C.One;
  I        = C.I;
  MinusOne = C.MinusOne;
  Half     = C.Half;

PROCEDURE Arg (READONLY x: T): R.T; (*return polar angle*)
PROCEDURE Abs (READONLY x: T): R.T; (*return magnitude*)
PROCEDURE AbsSqr (READONLY x: T): R.T; (*return square of the magnitude*)

PROCEDURE Norm1 (READONLY x: T): R.T;
PROCEDURE NormInf (READONLY x: T): R.T;
CONST Norm2 = Abs;

PROCEDURE SqRt (READONLY x: T): T; (*return square root of x with x.re>=0*)
PROCEDURE PowR (READONLY x: T; y: R.T): T; (*return x^y*)
(*NOTE: Also for roots, e.g., cube root: y=1/3*)
PROCEDURE Pow (x, y: T): T;      (*return x^y*)

(*---transcendentals---*)
PROCEDURE Exp (READONLY x: T): T; (*return e^x *)
PROCEDURE Ln (READONLY x: T): T; (*return ln(x) *)
PROCEDURE ExpI (x: R.T): T;      (*return e^(i*x) *)

(*---for trig and hyperbolics, must have |x|<=18---*)
PROCEDURE Cos (READONLY x: T): T RAISES {Error}; (*return cos(x) *)
PROCEDURE Sin (READONLY x: T): T RAISES {Error}; (*return sin(x) *)
PROCEDURE Tan (READONLY x: T): T RAISES {Error}; (*return tan(x) *)
PROCEDURE CosH (READONLY x: T): T RAISES {Error}; (*return cosh(x) *)
PROCEDURE SinH (READONLY x: T): T RAISES {Error}; (*return sinh(x) *)
PROCEDURE TanH (READONLY x: T): T RAISES {Error}; (*return tanh(x) *)

(*---for inverse trigonometrics---*)
PROCEDURE ArcCos (READONLY x: T): T RAISES {Error}; (*return arccos(x) *)
PROCEDURE ArcSin (READONLY x: T): T RAISES {Error}; (*return arcsin(x) *)
PROCEDURE ArcTan (READONLY x: T): T RAISES {Error}; (*return arctan(x) *)

(*==========================*)
END ComplexTrans.
