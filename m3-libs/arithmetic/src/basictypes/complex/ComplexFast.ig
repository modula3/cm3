GENERIC INTERFACE ComplexFast(R);
(*Arithmetic for Modula-3, see doc for details

   Abstract: Generic complex number type

   Fast implementation using infix operators.  Will become unnecessary if
   the compiler allows for INLINE procedures some day.  Instantiate with
   Real, LongReal, Extended *)

IMPORT FloatMode;
FROM NADefinitions IMPORT Error;

(*==========================*)

CONST Brand = R.Brand & "Complex";

TYPE
  T = RECORD re, im: R.T;  END;
  QuotRem = RECORD quot, rem: T;  END;

CONST
  Zero     = T{re := R.Zero, im := R.Zero};
  One      = T{re := R.One, im := R.Zero};
  I        = T{re := R.Zero, im := R.One};
  MinusOne = T{re := R.MinusOne, im := R.Zero};
  Half     = T{re := R.Half, im := R.Zero};

<*INLINE*>
PROCEDURE FromInteger (x: INTEGER): T;

<*INLINE*>
PROCEDURE Add (READONLY x, y: T): T; (*return x+y*)
<*INLINE*>
PROCEDURE Sub (READONLY x, y: T): T; (*return x-y*)
<*INLINE*>
PROCEDURE Neg (READONLY x: T): T; (*return -x *)
<*INLINE*>
PROCEDURE Conj (READONLY x: T): T; (*return complex conjugated of x*)
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
PROCEDURE Mod (READONLY x, y: T): T
  RAISES {Error};                (*return x mod y, which is always 0 for
                                    rational numbers*)
<*INLINE*>
PROCEDURE DivMod (READONLY x, y: T): QuotRem
  RAISES {Error};                (*return x/y and write the remainder in r,
                                    which is in turn 0*)

<*INLINE*>
PROCEDURE Square (READONLY x: T): T; (*return x*x*)
<*INLINE*>
PROCEDURE Scale (READONLY x: T; y: R.T): T; (*return x*y*)

(*---- Floating point representations ----*)

TYPE
  TExp = RECORD
           val: T;
           exp: INTEGER;
         END;

<*INLINE*>
PROCEDURE Normalize (READONLY x: T): TExp;

<*INLINE*>
PROCEDURE Scalb (READONLY x: T; n: INTEGER): T RAISES {FloatMode.Trap};

<*INLINE*>
PROCEDURE ILogb (READONLY x: T): INTEGER;
(*This is not really a logarithm of x or its absolute value, but something
   between the magnitudes of x.re and x.im, usable for normalization.*)

(*==========================*)
END ComplexFast.
