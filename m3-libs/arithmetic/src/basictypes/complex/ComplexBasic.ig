GENERIC INTERFACE ComplexBasic(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Generic complex number type

   Instantiate with RealBasic, LongRealBasic, ExtendedBasic,
   IntegerBasic *)

FROM Arithmetic IMPORT Error;


CONST Brand = R.Brand & "Complex";

TYPE
  T = RECORD re, im: R.T;  END;
  QuotRem = RECORD quot, rem: T;  END;

(* Zero and One non-constant in some modules CONST Zero = T{re:=R.Zero,
   im:=R.Zero}; One = T{re:=R.One, im:=R.Zero}; I = T{re:=R.Zero,
   im:=R.One}; MinusOne = T{re:=R.MinusOne, im:=R.Zero}; *)

VAR
  Zero    : T;
  One     : T;
  I       : T;
  MinusOne: T;

<* INLINE *>
PROCEDURE Add (READONLY x, y: T; ): T; (* x+y *)
<* INLINE *>
PROCEDURE Sub (READONLY x, y: T; ): T; (* x-y *)
<* INLINE *>
PROCEDURE Neg (READONLY x: T; ): T; (* -x *)
<* INLINE *>
PROCEDURE Conj (READONLY x: T; ): T; (* complex conjugate of x *)
<* INLINE *>
PROCEDURE IsZero (READONLY x: T; ): BOOLEAN;
<* INLINE *>
PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN; (* x=y *)

<* INLINE *>
PROCEDURE Mul (READONLY x, y: T; ): T; (* x*y *)
<* INLINE *>
PROCEDURE Div (READONLY x, y: T; ): T RAISES {Error}; (* x/y *)
<* INLINE *>
PROCEDURE Rec (READONLY x: T; ): T RAISES {Error}; (* 1/x *)
<* INLINE *>
PROCEDURE Mod (READONLY x, y: T; ): T RAISES {Error}; (* x mod y *)
<* INLINE *>
PROCEDURE DivMod (READONLY x, y: T; ): QuotRem
  RAISES {Error};                (* return x/y and x mod y *)

<* INLINE *>
PROCEDURE Square (READONLY x: T; ): T; (* x*x *)
<* INLINE *>
PROCEDURE Scale (READONLY x: T; y: R.T; ): T; (* x*y *)


END ComplexBasic.
