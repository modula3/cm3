GENERIC INTERFACE ComplexBasic(R);
(*Copyright (c) 1996, m3na project

   Abstract: Generic complex number type

   Instantiate with RealBasic, LongRealBasic, ExtendedBasic,
   IntegerBasic *)

FROM NADefinitions IMPORT Error;

(*==========================*)

CONST Brand = R.Brand & "Complex";

TYPE
  T = RECORD re, im: R.T;  END;
  QuotRem = RECORD quot, rem: T;  END;

(*Zero and One non-constant in some modules CONST Zero = T{re:=R.Zero,
   im:=R.Zero}; One = T{re:=R.One, im:=R.Zero}; I = T{re:=R.Zero,
   im:=R.One}; MinusOne = T{re:=R.MinusOne, im:=R.Zero}; *)

VAR
  Zero    : T;
  One     : T;
  I       : T;
  MinusOne: T;

<*INLINE*>
PROCEDURE Add (READONLY x, y: T): T; (*return x+y*)
<*INLINE*>
PROCEDURE Sub (READONLY x, y: T): T; (*return x-y*)
<*INLINE*>
PROCEDURE Neg (READONLY x: T): T; (*return -x *)
<*INLINE*>
PROCEDURE Conj (READONLY x: T): T; (*return complex conjugate of x*)
<*INLINE*>
PROCEDURE IsZero (READONLY x: T): BOOLEAN;
<*INLINE*>
PROCEDURE Equal (READONLY x, y: T): BOOLEAN; (*return x=y*)

<*INLINE*>
PROCEDURE Mul (READONLY x, y: T): T; (*return x*y*)
<*INLINE*>
PROCEDURE Div (READONLY x, y: T): T RAISES {Error}; (*return x/y*)
<*INLINE*>
PROCEDURE Rec (READONLY x: T): T RAISES {Error}; (*return 1/x*)
<*INLINE*>
PROCEDURE Mod (READONLY x, y: T): T RAISES {Error}; (*return x mod y*)
<*INLINE*>
PROCEDURE DivMod (READONLY x, y: T): QuotRem
  RAISES {Error};                (*return x/y and write the remainder in
                                    r*)

<*INLINE*>
PROCEDURE Square (READONLY x: T): T; (*return x*x*)
<*INLINE*>
PROCEDURE Scale (READONLY x: T; y: R.T): T; (*return x*y*)

(*==========================*)
END ComplexBasic.
