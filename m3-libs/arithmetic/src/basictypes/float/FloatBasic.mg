GENERIC MODULE FloatBasic();
(*Arithmetic for Modula-3, see doc for details

   Abstract: Generic wrapper routines for basic operations of float
   types *)

IMPORT Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "FloatBasic.";
(*==========================*)

PROCEDURE FromInteger (x: INTEGER): T =
  BEGIN
    RETURN FLOAT(x, T)
  END FromInteger;


PROCEDURE Add (x, y: T): T =
  BEGIN
    RETURN x + y
  END Add;

PROCEDURE Sub (x, y: T): T =
  BEGIN
    RETURN x - y
  END Sub;

PROCEDURE Neg (x: T): T =
  BEGIN
    RETURN -x
  END Neg;

PROCEDURE Conj (x: T): T =
  BEGIN
    RETURN x
  END Conj;

PROCEDURE IsZero (x: T): BOOLEAN =
  BEGIN
    RETURN x = Zero;
  END IsZero;

<* INLINE *>
PROCEDURE CheckDivisor (x: T) RAISES {Arith.Error} =
  BEGIN
    IF x = Zero THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    END;
  END CheckDivisor;


PROCEDURE Mul (x, y: T): T =
  BEGIN
    RETURN x * y
  END Mul;

PROCEDURE Div (x, y: T): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN x / y
  END Div;

PROCEDURE Mod (<* UNUSED *> x: T; y: T): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN Zero
  END Mod;

PROCEDURE DivMod (x, y: T): QuotRem RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN QuotRem{x / y, Zero}
  END DivMod;

PROCEDURE IntMod (x, y: T): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN x MOD y
  END IntMod;

PROCEDURE Rec (x: T): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(x);
    RETURN One / x
  END Rec;

PROCEDURE ScaleInt (x: T; y: INTEGER): T =
  BEGIN
    RETURN x * FLOAT(y, T)
  END ScaleInt;

(*==========================*)
BEGIN
END FloatBasic.
