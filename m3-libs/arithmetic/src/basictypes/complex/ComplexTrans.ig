GENERIC INTERFACE ComplexTrans(R, C);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Transcendental functions of complex numbers. *)

FROM Arithmetic IMPORT Error;

TYPE T = C.T;

CONST
  Zero     = C.Zero;
  One      = C.One;
  I        = C.I;
  MinusOne = C.MinusOne;
  Half     = C.Half;

PROCEDURE Arg (READONLY x: T; ): R.T; (* polar angle*)
PROCEDURE Abs (READONLY x: T; ): R.T; (* magnitude*)
PROCEDURE AbsSqr (READONLY x: T; ): R.T; (* square of the magnitude*)

PROCEDURE Norm1 (READONLY x: T; ): R.T;
PROCEDURE NormInf (READONLY x: T; ): R.T;
CONST Norm2 = Abs;

PROCEDURE SqRt (READONLY x: T; ): T; (* square root of x with x.re>=0*)
PROCEDURE PowR (READONLY x: T; y: R.T; ): T; (* x^y*)
(* NOTE: Also for roots, e.g., cube root: y=1/3 *)
PROCEDURE Pow (x, y: T; ): T;    (* x^y*)

(* transcendentals *)
PROCEDURE Exp (READONLY x: T; ): T; (* e^x *)
PROCEDURE Ln (READONLY x: T; ): T; (* ln(x) *)
PROCEDURE ExpI (x: R.T; ): T;    (* e^(i*x) *)

(* for trig and hyperbolics, must have |x|<=18 *)
PROCEDURE Cos (READONLY x: T; ): T RAISES {Error}; (* cos(x) *)
PROCEDURE Sin (READONLY x: T; ): T RAISES {Error}; (* sin(x) *)
PROCEDURE Tan (READONLY x: T; ): T RAISES {Error}; (* tan(x) *)
PROCEDURE CosH (READONLY x: T; ): T RAISES {Error}; (* cosh(x) *)
PROCEDURE SinH (READONLY x: T; ): T RAISES {Error}; (* sinh(x) *)
PROCEDURE TanH (READONLY x: T; ): T RAISES {Error}; (* tanh(x) *)

(* for inverse trigonometrics *)
PROCEDURE ArcCos (READONLY x: T; ): T RAISES {Error}; (* arccos(x) *)
PROCEDURE ArcSin (READONLY x: T; ): T RAISES {Error}; (* arcsin(x) *)
PROCEDURE ArcTan (READONLY x: T; ): T RAISES {Error}; (* arctan(x) *)

END ComplexTrans.
