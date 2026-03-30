(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeInt;
IMPORT SchemeObject, Mpz, SchemeLongReal;

CONST
  CacheLo = -256;
  CacheHi =  256;

VAR
  cache: ARRAY [CacheLo .. CacheHi] OF SchemeObject.T;

PROCEDURE FromI(x: INTEGER): SchemeObject.T =
  BEGIN
    IF x >= CacheLo AND x <= CacheHi THEN
      RETURN cache[x]
    ELSE
      WITH ri = NEW(T) DO ri^ := x; RETURN ri END
    END
  END FromI;

PROCEDURE IsInt(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN x # NIL AND ISTYPE(x, T)
  END IsInt;

PROCEDURE IsExactInt(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN x # NIL AND (ISTYPE(x, T) OR ISTYPE(x, Mpz.T))
  END IsExactInt;

PROCEDURE IsNumber(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN x # NIL AND (ISTYPE(x, T) OR ISTYPE(x, Mpz.T) OR
                         ISTYPE(x, SchemeLongReal.T))
  END IsNumber;

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER =
  BEGIN
    TYPECASE x OF
      T(ri) => RETURN ri^
    | Mpz.T(m) => RETURN Mpz.ToInteger(m)
    ELSE
      <* ASSERT FALSE *>
    END
  END ToInteger;

PROCEDURE Add(a, b: INTEGER): SchemeObject.T =
  BEGIN
    IF (a > 0 AND b > 0 AND a > LAST(INTEGER) - b) OR
       (a < 0 AND b < 0 AND a < FIRST(INTEGER) - b) THEN
      VAR ma := Mpz.NewInt(a); mb := Mpz.NewInt(b); mr := Mpz.New();
      BEGIN
        Mpz.add(mr, ma, mb); RETURN MpzToScheme(mr)
      END
    ELSE
      RETURN FromI(a + b)
    END
  END Add;

PROCEDURE Sub(a, b: INTEGER): SchemeObject.T =
  BEGIN
    IF (b > 0 AND a < FIRST(INTEGER) + b) OR
       (b < 0 AND a > LAST(INTEGER) + b) THEN
      VAR ma := Mpz.NewInt(a); mb := Mpz.NewInt(b); mr := Mpz.New();
      BEGIN
        Mpz.sub(mr, ma, mb); RETURN MpzToScheme(mr)
      END
    ELSE
      RETURN FromI(a - b)
    END
  END Sub;

CONST
  (* If both operands fit in 32-bit signed, the product fits in 64-bit signed *)
  Mul32Lo = -2147483648;
  Mul32Hi =  2147483647;

PROCEDURE Mul(a, b: INTEGER): SchemeObject.T =
  BEGIN
    IF a >= Mul32Lo AND a <= Mul32Hi AND
       b >= Mul32Lo AND b <= Mul32Hi THEN
      RETURN FromI(a * b)
    ELSE
      VAR ma := Mpz.NewInt(a); mb := Mpz.NewInt(b); mr := Mpz.New();
      BEGIN
        Mpz.mul(mr, ma, mb); RETURN MpzToScheme(mr)
      END
    END
  END Mul;

PROCEDURE ToMpz(x: SchemeObject.T): Mpz.T =
  BEGIN
    TYPECASE x OF
      T(ri) => RETURN Mpz.NewInt(ri^)
    | Mpz.T(m) => RETURN m
    ELSE
      <* ASSERT FALSE *>
    END
  END ToMpz;

PROCEDURE MpzToScheme(m: Mpz.T): SchemeObject.T =
  BEGIN
    IF Mpz.fits_slong_p(m) # 0 THEN
      RETURN FromI(Mpz.get_si(m))
    ELSE
      RETURN m
    END
  END MpzToScheme;

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      T(ri) => RETURN ri^ = 0
    | Mpz.T(m) => RETURN Mpz.cmp(m, MpzZero) = 0
    ELSE
      RETURN FALSE
    END
  END IsZero;

PROCEDURE IsPositive(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      T(ri) => RETURN ri^ > 0
    | Mpz.T(m) => RETURN Mpz.cmp(m, MpzZero) > 0
    ELSE
      RETURN FALSE
    END
  END IsPositive;

PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      T(ri) => RETURN ri^ < 0
    | Mpz.T(m) => RETURN Mpz.cmp(m, MpzZero) < 0
    ELSE
      RETURN FALSE
    END
  END IsNegative;

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER =
  VAR
    ma, mb: Mpz.T;
    result: INTEGER;
  BEGIN
    TYPECASE a OF
      T(ra) =>
      TYPECASE b OF
        T(rb) =>
        IF ra^ < rb^ THEN RETURN -1
        ELSIF ra^ > rb^ THEN RETURN 1
        ELSE RETURN 0
        END
      | Mpz.T(mb2) =>
        ma := Mpz.NewInt(ra^);
        result := Mpz.cmp(ma, mb2);
        IF result < 0 THEN RETURN -1
        ELSIF result > 0 THEN RETURN 1
        ELSE RETURN 0
        END
      ELSE
        <* ASSERT FALSE *>
      END
    | Mpz.T(ma2) =>
      TYPECASE b OF
        T(rb) =>
        mb := Mpz.NewInt(rb^);
        result := Mpz.cmp(ma2, mb);
        IF result < 0 THEN RETURN -1
        ELSIF result > 0 THEN RETURN 1
        ELSE RETURN 0
        END
      | Mpz.T(mb2) =>
        result := Mpz.cmp(ma2, mb2);
        IF result < 0 THEN RETURN -1
        ELSIF result > 0 THEN RETURN 1
        ELSE RETURN 0
        END
      ELSE
        <* ASSERT FALSE *>
      END
    ELSE
      <* ASSERT FALSE *>
    END
  END Compare;

VAR MpzZero: Mpz.T;

BEGIN
  FOR i := CacheLo TO CacheHi DO
    WITH ri = NEW(T) DO
      ri^ := i;
      cache[i] := ri
    END
  END;

  Zero   := cache[0];
  One    := cache[1];
  NegOne := cache[-1];
  Two    := cache[2];

  MpzZero := Mpz.NewInt(0);
END SchemeInt.
