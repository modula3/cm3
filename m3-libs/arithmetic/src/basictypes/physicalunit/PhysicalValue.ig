GENERIC INTERFACE PhysicalValue(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Combines a numerical type with physical units.  This allows
   for dynamically safe operations. *)

IMPORT PhysicalUnit AS U;

FROM Arithmetic IMPORT Error;


TYPE
  T = RECORD
        val : R.T;
        unit: U.T
      END;
  QuotRem = RECORD quot, rem: T END;

VAR
  Zero: T;
  One : T;

<* INLINE *>
PROCEDURE Add (READONLY x, y: T; ): T RAISES {Error}; (* x+y *)
<* INLINE *>
PROCEDURE Sub (READONLY x, y: T; ): T RAISES {Error}; (* x-y *)
<* INLINE *>
PROCEDURE Neg (READONLY x: T; ): T; (* -x *)
<* INLINE *>
PROCEDURE Conj (READONLY x: T; ): T; (* complex conjugate of x *)
<* INLINE *>
PROCEDURE IsZero (READONLY x: T; ): BOOLEAN;
<* INLINE *>
PROCEDURE IsScalar (READONLY x: T; ): BOOLEAN;
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
  RAISES {Error};                (* x/y and x mod y *)

<* INLINE *>
PROCEDURE Square (READONLY x: T; ): T; (* x*x *)
<* INLINE *>
PROCEDURE Scale (READONLY x: T; y: R.T; ): T; (* x*y *)

END PhysicalValue.
