INTERFACE BigInteger;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Integers of arbitrary size

   DragonInt seems to be less flexible and lacks some essential operations
   like multiplication and division.

   Daniel Beer *)

(*==========================*)

FROM Arithmetic IMPORT Error;

CONST Brand = "BigInteger";

(*
BigIntegers are immutable, do not change them manually,
because some routines use references to existing BigIntegers
for performance reasons.
*)
TYPE
  Value <: REFANY;
  T = RECORD
        data: Value;
        size: INTEGER;           (*not all entries in 'data' may be used*)
        sign: BOOLEAN;
      END;
  QuotRem = RECORD quot, rem: T END;

PROCEDURE FromInteger (x: INTEGER): T;
PROCEDURE Copy (READONLY x: T): T;

VAR
  (*CONST*)
  Zero,
  (*CONST*) One,
  (*CONST*) Two,
  (*CONST*) MinusOne: T;

PROCEDURE Add (READONLY x, y: T): T;
PROCEDURE Sub (READONLY x, y: T): T;
<*INLINE*>
PROCEDURE Neg (READONLY x: T): T;
<*INLINE*>
PROCEDURE Conj (READONLY x: T): T;

<*INLINE*>
PROCEDURE IsZero (READONLY x: T): BOOLEAN;
PROCEDURE Equal (READONLY x, y: T): BOOLEAN;
PROCEDURE Compare (READONLY x, y: T): [-1 .. 1];

PROCEDURE Mul (READONLY x, y: T): T;
PROCEDURE Div (READONLY x, y: T): T RAISES {Error};
PROCEDURE Rec (READONLY x: T): T RAISES {Error};
PROCEDURE Mod (READONLY x, y: T): T RAISES {Error};
PROCEDURE DivMod (READONLY x, y: T): QuotRem RAISES {Error};

(*==========================*)
END BigInteger.
