GENERIC MODULE IntegerBasic();
(* Arithmetic for Modula-3, see doc for details*)

IMPORT Word AS W, Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "IntegerBasic.";

PROCEDURE Add (x, y: T; ): T =
  BEGIN
    RETURN x + y
  END Add;

PROCEDURE Sub (x, y: T; ): T =
  BEGIN
    RETURN x - y
  END Sub;

PROCEDURE Neg (x: T; ): T =
  BEGIN
    RETURN -x
  END Neg;

PROCEDURE Conj (x: T; ): T =
  BEGIN
    RETURN x
  END Conj;

PROCEDURE IsZero (x: T; ): BOOLEAN =
  BEGIN
    RETURN x = Zero;
  END IsZero;

PROCEDURE Mul (x, y: T; ): T =
  BEGIN
    RETURN x * y
  END Mul;

PROCEDURE CheckDivisor (x: T; ) RAISES {Arith.Error} =
  BEGIN
    IF x = 0 THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init())
    END;
  END CheckDivisor;

PROCEDURE Div (x, y: T; ): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    IF x MOD y # 0 THEN
      RAISE Arith.Error(NEW(Arith.ErrorIndivisible).init())
    END;
    RETURN x DIV y;
  END Div;

PROCEDURE Rec (x: T; ): T RAISES {Arith.Error} =
  BEGIN
    CASE x OF
    | 0 => RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    | 1 => RETURN 1;
    ELSE
      RAISE Arith.Error(NEW(Arith.ErrorIndivisible).init());
    END;
  END Rec;

PROCEDURE Mod (x, y: T; ): T RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN x MOD y;
  END Mod;

PROCEDURE DivMod (x, y: T; ): QuotRem RAISES {Arith.Error} =
  BEGIN
    CheckDivisor(y);
    RETURN QuotRem{x MOD y, x DIV y};
  END DivMod;

(* Kapil Hari Paranjape: Some lectures on number theory, elliptic curves
   and cryptology, chapter 2: Greatest common divisor,
   http://www.imsc.ernet.in/~kapil/crypto/notes/node8.html*)
PROCEDURE GCD (x, y: T; ): T =
  VAR
    xt, yt: [0 .. BITSIZE(T)] := 0;
    z     : T                 := One;
  BEGIN
    IF x = 0 THEN RETURN y; END;
    IF y = 0 THEN RETURN x; END;
    (* This will be optimized to bit shift operations I hope*)
    (* count the factor 2*)
    WHILE x MOD 2 = 0 DO x := x DIV 2; INC(xt); END;
    WHILE y MOD 2 = 0 DO y := y DIV 2; INC(yt); END;
    WHILE x # y DO
      IF x <= y THEN z := y - x; ELSE z := x - y; x := y; END;
      (* both x and y are odd, thus there difference is even*)
      WHILE z MOD 2 = 0 DO z := z DIV 2; END;
      y := z;
    END;
    RETURN W.LeftShift(x, MIN(xt, yt));
  END GCD;

BEGIN
END IntegerBasic.
