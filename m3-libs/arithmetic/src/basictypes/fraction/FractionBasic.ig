GENERIC INTERFACE FractionBasic(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Generic fraction type

   Instantiate with integers, polynomials *)

FROM Arithmetic IMPORT Error;


CONST Brand = R.Brand & "Fraction";

TYPE
  T = RECORD n, d: R.T;  END;    (* Numerator, Denominator *)
  QuotRem = RECORD quot, rem: T END;

(**
CONST
  Zero     = T{n:=R.Zero,     d:=R.One};
  One      = T{n:=R.One,      d:=R.One};
  MinusOne = T{n:=R.MinusOne, d:=R.One};
*)
(*Zero and One aren't constants in some modules*)
VAR
  Zero: T;
  One : T;
(* MinusOne : T; *)

<* INLINE *>
PROCEDURE Add (READONLY x, y: T; ): T; (* x+y*)
<* INLINE *>
PROCEDURE Sub (READONLY x, y: T; ): T; (* x-y*)
<* INLINE *>
PROCEDURE Neg (READONLY x: T; ): T; (* -x *)
<* INLINE *>
PROCEDURE Conj (READONLY x: T; ): T; (* complex conjugate of x*)
<* INLINE *>
PROCEDURE IsZero (READONLY x: T; ): BOOLEAN;
PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN; (* x=y*)
PROCEDURE Compare (READONLY x, y: T; ): [-1 .. 1];

<* INLINE *>
PROCEDURE Mul (READONLY x, y: T; ): T; (* x*y*)
<* INLINE *>
PROCEDURE Div (READONLY x, y: T; ): T RAISES {Error}; (* x/y*)
<* INLINE *>
PROCEDURE Rec (READONLY x: T; ): T RAISES {Error}; (* 1/x*)
<* INLINE *>
PROCEDURE Mod (READONLY x, y: T; ): T RAISES {Error}; (* x mod y*)
<* INLINE *>
PROCEDURE DivMod (x, y: T; ): QuotRem
  RAISES {Error};                (* x/y and write the remainder (0) in r*)
<* INLINE *>
PROCEDURE IntMod (READONLY x, y: T; ): T RAISES {Error}; (* x mod y*)

<* INLINE *>
PROCEDURE Square (READONLY x: T; ): T; (* x*x*)
<* INLINE *>
PROCEDURE Scale (READONLY x: T; y: R.T; ): T; (* x*y*)

END FractionBasic.
