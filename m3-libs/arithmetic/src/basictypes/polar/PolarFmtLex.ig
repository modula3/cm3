GENERIC INTERFACE PolarFmtLex();
(*Copyright (c) 1996, m3na project
  
Abstract: Direct interfaces to complex functions

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;
IMPORT Fmt,xReal64 AS R;
FROM xReal64 IMPORT REAL64;

TYPE
   COMPLEX= RECORD re,im:REAL64; END;
   T      = COMPLEX;

   (*polar angles are in radians*)
   POLAR  = RECORD radius,angle:REAL64; END;

CONST
   Zero    =  T{re:=R.Zero,  im:=R.Zero};
   One     =  T{re:=R.One,   im:=R.Zero};
   i       =  T{re:=R.Zero,  im:=R.One};
   MinusOne=  T{re:=-R.One,  im:=R.Zero};
(*============================*)
(* Handy collectors           *)
(*============================*)
TYPE Array = REF ARRAY OF COMPLEX;

(*============================*)
(* Functions                  *)
(*============================*)
(*new, copy are via primitives*)

PROCEDURE lex(str:TEXT):COMPLEX RAISES {Error};
        (*reads after the "COMPLEX{" in COMPLEX{re:=<r>; im:=<r>},
        thru the "}"*)
PROCEDURE fmt(x:COMPLEX; 
        style:Fmt.Style:=Fmt.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*outputs as "COMPLEX{re:=<r>; im:=<r>}"
        Uses simple Fmt.Real if x.im=0.0.*)

PROCEDURE arg(VALUE c:COMPLEX):REAL64;       (*return polar angle*)    
PROCEDURE conj(VALUE c:COMPLEX):COMPLEX;     (*return conjugate of c*)

PROCEDURE add(c1,c2:COMPLEX):COMPLEX;  (*return c1+c2*)
PROCEDURE sub(c1,c2:COMPLEX):COMPLEX;  (*return c1-c2*)
PROCEDURE mul(c1,c2:COMPLEX):COMPLEX;  (*return c1*c2*)

PROCEDURE scale(VALUE c:COMPLEX;
          factor:REAL64):COMPLEX;(*return c*factor*)                 

PROCEDURE powN(VALUE c:COMPLEX;
         n:REAL64):COMPLEX;      (*return c^n*)
         (*NOTE: Also for roots, e.g., cube root: n=1/3*)
PROCEDURE powXY(x,y:COMPLEX):COMPLEX;  (*return x^y*)

(*---transcendentals---*)
PROCEDURE exp(VALUE c:COMPLEX):COMPLEX;      (*return e^c *)
PROCEDURE ln (VALUE c:COMPLEX):COMPLEX;      (*return ln(c) *)

(*---for trig and hyperbolics, must have |c|<=18---*)
PROCEDURE cos(VALUE c:COMPLEX):COMPLEX RAISES {Error}; (*return cos(c) *)
PROCEDURE sin(VALUE c:COMPLEX):COMPLEX RAISES {Error}; (*return sin(c) *)
PROCEDURE tan(VALUE c:COMPLEX):COMPLEX RAISES {Error}; (*return tan(c) *)
PROCEDURE cosh(VALUE c:COMPLEX):COMPLEX RAISES {Error};(*return cosh(c) *)
PROCEDURE sinh(VALUE c:COMPLEX):COMPLEX RAISES {Error};(*return sinh(c) *)
PROCEDURE tanh(VALUE c:COMPLEX):COMPLEX RAISES {Error};(*return tanh(c) *)
        
(*---polar form---*)
PROCEDURE toPolar(VALUE c:COMPLEX):POLAR;
PROCEDURE fromPolar(VALUE c:POLAR):COMPLEX;
PROCEDURE fmtPolar(VALUE c:POLAR;
        style:Fmt.Style:=Fmt.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*as "POLAR{radius:=<r>; angle:=<r>}"*)
        
PROCEDURE pmul(p1,p2:POLAR):POLAR;     (*return p1*p2*)
PROCEDURE pdiv(p1,p2:POLAR):POLAR;     (*return p1/p2*)

(*===============*)
(* WDS Functions *)
(*===============*)
PROCEDURE Magnitude(VALUE c : T) : LONGREAL;

PROCEDURE FromRadiusAndTheta(r, theta : LONGREAL) : T;

PROCEDURE Negative(VALUE c : T) : T;

PROCEDURE Divide(VALUE a,b : T) : T;

PROCEDURE L1Norm(VALUE c : T) : LONGREAL;

PROCEDURE LinfinityNorm(VALUE c : T) : LONGREAL;

PROCEDURE Square(VALUE a : T) : T;

PROCEDURE Sqrt(VALUE a : T) : T;

(*===============================*)
(* Alternative naming conventions*)
(*===============================*)
CONST
  (*---HGG style using WDS implementations---*)
  abs = Magnitude;
  div = Divide;
  sqrt= Sqrt;

  
  (*---WDS style using HGG implementations---*)
  Arg = arg;
  Conjugate = conj;
  Scale = scale;

  Plus = add;
  Minus = sub;
  Times = mul;

  log = ln;


(*==========================*)
END PolarFmtLex.
