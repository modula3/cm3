GENERIC INTERFACE ComplexTrans(C,R);
(*Copyright (x) 1996, m3na project

Abstract: Transcendental functions of complex numbers.

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;

PROCEDURE Arg   (READONLY x:C.T):R.T;       (*return polar angle*)    
PROCEDURE Abs   (READONLY x:C.T):R.T;       (*return magnitude*)
PROCEDURE AbsSqr(READONLY x:C.T):R.T;       (*return square of the magnitude*)

PROCEDURE Norm1  (READONLY x:C.T):R.T;
PROCEDURE NormInf(READONLY x:C.T):R.T;
CONST Norm2 = Abs;

PROCEDURE SqRt(READONLY x:C.T):C.T;   (*return square root of x with x.re>=0*)
PROCEDURE PowR(READONLY x:C.T;
                        y:R.T):C.T;      (*return x^y*)
         (*NOTE: Also for roots, e.g., cube root: y=1/3*)
PROCEDURE Pow(x,y:C.T):C.T;  (*return x^y*)

(*---transcendentals---*)
PROCEDURE Exp(READONLY x:C.T):C.T;      (*return e^x *)
PROCEDURE Ln (READONLY x:C.T):C.T;      (*return ln(x) *)

(*---for trig and hyperbolics, must have |x|<=18---*)
PROCEDURE Cos(READONLY x:C.T):C.T RAISES {Error}; (*return cos(x) *)
PROCEDURE Sin(READONLY x:C.T):C.T RAISES {Error}; (*return sin(x) *)
PROCEDURE Tan(READONLY x:C.T):C.T RAISES {Error}; (*return tan(x) *)
PROCEDURE CosH(READONLY x:C.T):C.T RAISES {Error};(*return cosh(x) *)
PROCEDURE SinH(READONLY x:C.T):C.T RAISES {Error};(*return sinh(x) *)
PROCEDURE TanH(READONLY x:C.T):C.T RAISES {Error};(*return tanh(x) *)

(*---for inverse trigonometrics---*)
PROCEDURE ArcCos(READONLY x:C.T):C.T RAISES {Error}; (*return arccos(x) *)
PROCEDURE ArcSin(READONLY x:C.T):C.T RAISES {Error}; (*return arcsin(x) *)
PROCEDURE ArcTan(READONLY x:C.T):C.T RAISES {Error}; (*return arctan(x) *)

(*---- Floating point representations ----*)

<*INLINE*> PROCEDURE FrExp (READONLY x: C.T; VAR exp: INTEGER): C.T;
(* returns a value y and sets exp such that x = y * 2^exp,
    where ABS(y.re*y.im) is "close to" 1.
   this can be used to improve numerical condition *)

<*INLINE*> PROCEDURE LdExp (READONLY x: C.T; exp: INTEGER): C.T;
(* returns x * 2^exp. *)

<*INLINE*> PROCEDURE ModF (READONLY x: C.T; VAR(*OUT*) i: C.T): C.T;
(* splits the argument "x" into an integer part "i" and a fractional part "f"
   such that "f + i = x" and such that "f" and "i" both have the same sign as
   "x", and returns "f". Although "i" is a LONGREAL, it is set to an integral
   value. *)

(*==========================*)
END ComplexTrans.
