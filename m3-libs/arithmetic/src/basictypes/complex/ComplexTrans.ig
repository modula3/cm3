GENERIC INTERFACE ComplexTrans(C,R);
(*Copyright (c) 1996, m3na project

Abstract: Transcendental functions of complex numbers.

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;

PROCEDURE Arg   (READONLY c:C.T):R.T;       (*return polar angle*)    
PROCEDURE Abs   (READONLY c:C.T):R.T;       (*return magnitude*)
PROCEDURE AbsSqr(READONLY c:C.T):R.T;       (*return square of the magnitude*)

PROCEDURE Norm1  (READONLY c:C.T):R.T;
PROCEDURE NormInf(READONLY c:C.T):R.T;
CONST Norm2 = Abs;

PROCEDURE SqRt(READONLY c:C.T):C.T;   (*return square root of c with c.re>=0*)
PROCEDURE PowR(READONLY c:C.T;
                        y:R.T):C.T;      (*return c^y*)
         (*NOTE: Also for roots, e.g., cube root: y=1/3*)
PROCEDURE Pow(x,y:C.T):C.T;  (*return x^y*)

(*---transcendentals---*)
PROCEDURE Exp(READONLY c:C.T):C.T;      (*return e^c *)
PROCEDURE Ln (READONLY c:C.T):C.T;      (*return ln(c) *)

(*---for trig and hyperbolics, must have |c|<=18---*)
PROCEDURE Cos(READONLY c:C.T):C.T RAISES {Error}; (*return cos(c) *)
PROCEDURE Sin(READONLY c:C.T):C.T RAISES {Error}; (*return sin(c) *)
PROCEDURE Tan(READONLY c:C.T):C.T RAISES {Error}; (*return tan(c) *)
PROCEDURE CosH(READONLY c:C.T):C.T RAISES {Error};(*return cosh(c) *)
PROCEDURE SinH(READONLY c:C.T):C.T RAISES {Error};(*return sinh(c) *)
PROCEDURE TanH(READONLY c:C.T):C.T RAISES {Error};(*return tanh(c) *)

(*---for inverse trigonometrics---*)
PROCEDURE ArcCos(READONLY c:C.T):C.T RAISES {Error}; (*return arccos(c) *)
PROCEDURE ArcSin(READONLY c:C.T):C.T RAISES {Error}; (*return arcsin(c) *)
PROCEDURE ArcTan(READONLY c:C.T):C.T RAISES {Error}; (*return arctan(c) *)

(*==========================*)
END ComplexTrans.
