GENERIC INTERFACE ComplexFast(R);
(*Copyright (c) 1996, m3na project

Abstract: Generic complex number type
          Fast implementation using infix operators.
		  Will become unnecessary if the compiler allows for INLINE procedures some day.
          Instantiate with Real, LongReal, Extended

Originally was xComplex

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations

*)

FROM xUtils IMPORT Error;

(*==========================*)

TYPE
  T = RECORD re,im: R.T; END;

CONST
  Zero     = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(0.0,R.T)};
  One      = T{re:=FLOAT( 1.0,R.T), im:=FLOAT(0.0,R.T)};
  I        = T{re:=FLOAT( 0.0,R.T), im:=FLOAT(1.0,R.T)};
  MinusOne = T{re:=FLOAT(-1.0,R.T), im:=FLOAT(0.0,R.T)};

<*INLINE*> PROCEDURE Add(READONLY x,y:T):T;  (*return x+y*)
<*INLINE*> PROCEDURE Sub(READONLY x,y:T):T;  (*return x-y*)
<*INLINE*> PROCEDURE Neg(READONLY x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(READONLY x:T):T;   (*return complex conjugated of x*)
<*INLINE*> PROCEDURE IsZero(READONLY x:T):BOOLEAN;
<*INLINE*> PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

<*INLINE*> PROCEDURE Mul(READONLY x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(READONLY x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)

<*INLINE*> PROCEDURE Square(READONLY x:T):T;         (*return x*x*)
<*INLINE*> PROCEDURE Scale (READONLY x:T; y:R.T):T;  (*return x*y*)
<*OBSOLETE*> PROCEDURE ScaleInt(x:T;y:INTEGER):T;  (*return x*y*)

(*---- Floating point representations ----*)

<*INLINE*> PROCEDURE FrExp (READONLY x: T; VAR exp: INTEGER): T;
(* returns a value y and sets exp such that x = y * 2^exp,
    where ABS(y.re*y.im) is "close to" 1.
   this can be used to improve numerical condition *)

<*INLINE*> PROCEDURE LdExp (READONLY x: T; exp: INTEGER): T;
(* returns x * 2^exp. *)

<*INLINE*> PROCEDURE ModF (READONLY x: T; VAR(*OUT*) i: T): T;
(* splits the argument "x" into an integer part "i" and a fractional part "f"
   such that "f + i = x" and such that "f" and "i" both have the same sign as
   "x", and returns "f". Although "i" is a LONGREAL, it is set to an integral
   value. *)

(*==========================*)
END ComplexFast.
