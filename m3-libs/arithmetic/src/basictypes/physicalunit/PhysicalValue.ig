GENERIC INTERFACE PhysicalValue(R);
(*Arithmetic for Modula-3, see doc for details

Abstract: Combines a numerical type with physical units.
          This allows for dynamically safe operations.

*)

IMPORT PhysicalUnit AS U;

FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE
  T = RECORD val:R.T; unit:U.T END;
  QuotRem = RECORD quot, rem: T END;

VAR
  Zero     : T;
  One      : T;

<*INLINE*> PROCEDURE Add(READONLY x,y:T):T RAISES {Error};  (*return x+y*)
<*INLINE*> PROCEDURE Sub(READONLY x,y:T):T RAISES {Error};  (*return x-y*)
<*INLINE*> PROCEDURE Neg(READONLY x:T):T;    (*return -x *)
<*INLINE*> PROCEDURE Conj(READONLY x:T):T;   (*return complex conjugate of x*)
<*INLINE*> PROCEDURE IsZero  (READONLY x:T):BOOLEAN;
<*INLINE*> PROCEDURE IsScalar(READONLY x:T):BOOLEAN;
<*INLINE*> PROCEDURE Equal(READONLY x,y:T):BOOLEAN;  (*return x=y*)

<*INLINE*> PROCEDURE Mul(READONLY x,y:T):T;  (*return x*y*)
<*INLINE*> PROCEDURE Div(READONLY x,y:T):T RAISES {Error};  (*return x/y*)
<*INLINE*> PROCEDURE Rec(READONLY x:T):T RAISES {Error};    (*return 1/x*)
<*INLINE*> PROCEDURE Mod(READONLY x,y:T):T RAISES {Error};  (*return x mod y*)
<*INLINE*> PROCEDURE DivMod(READONLY x,y:T): QuotRem RAISES {Error};  (*return x/y and write the remainder in r*)

<*INLINE*> PROCEDURE Square(READONLY x:T):T;         (*return x*x*)
<*INLINE*> PROCEDURE Scale (READONLY x:T; y:R.T):T;  (*return x*y*)

(*==========================*)
END PhysicalValue.
