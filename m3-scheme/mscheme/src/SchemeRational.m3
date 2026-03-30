(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeRational;
IMPORT SchemeObject, SchemeInt, Mpz;

VAR MpzZero, MpzOne: Mpz.T;

PROCEDURE New(num, den: Mpz.T): SchemeObject.T =
  VAR g := Mpz.New();
  BEGIN
    <* ASSERT Mpz.cmp(den, MpzZero) # 0 *>

    (* Ensure denominator is positive *)
    IF Mpz.cmp(den, MpzZero) < 0 THEN
      Mpz.neg(num, num);
      Mpz.neg(den, den);
    END;

    (* Reduce to lowest terms *)
    Mpz.gcd(g, num, den);
    IF Mpz.cmp(g, MpzOne) # 0 THEN
      Mpz.tdiv_q(num, num, g);
      Mpz.tdiv_q(den, den, g);
    END;

    (* Demote to integer if den = 1 *)
    IF Mpz.cmp(den, MpzOne) = 0 THEN
      RETURN SchemeInt.MpzToScheme(num)
    END;

    RETURN NEW(T, num := num, den := den)
  END New;

PROCEDURE Add(a, b: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
      t   := Mpz.New();
  BEGIN
    (* (a.num * b.den + b.num * a.den) / (a.den * b.den) *)
    Mpz.mul(num, a.num, b.den);
    Mpz.mul(t,   b.num, a.den);
    Mpz.add(num, num, t);
    Mpz.mul(den, a.den, b.den);
    RETURN New(num, den)
  END Add;

PROCEDURE Sub(a, b: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
      t   := Mpz.New();
  BEGIN
    (* (a.num * b.den - b.num * a.den) / (a.den * b.den) *)
    Mpz.mul(num, a.num, b.den);
    Mpz.mul(t,   b.num, a.den);
    Mpz.sub(num, num, t);
    Mpz.mul(den, a.den, b.den);
    RETURN New(num, den)
  END Sub;

PROCEDURE Mul(a, b: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
  BEGIN
    (* (a.num * b.num) / (a.den * b.den) *)
    Mpz.mul(num, a.num, b.num);
    Mpz.mul(den, a.den, b.den);
    RETURN New(num, den)
  END Mul;

PROCEDURE Div(a, b: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
  BEGIN
    <* ASSERT NOT IsZero(b) *>
    (* (a.num * b.den) / (a.den * b.num) *)
    Mpz.mul(num, a.num, b.den);
    Mpz.mul(den, a.den, b.num);
    RETURN New(num, den)
  END Div;

PROCEDURE Neg(a: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
  BEGIN
    Mpz.neg(num, a.num);
    Mpz.set(den, a.den);
    RETURN NEW(T, num := num, den := den)
    (* Already normalized; den unchanged, num negated *)
  END Neg;

PROCEDURE Abs(a: T): SchemeObject.T =
  VAR num := Mpz.New();
      den := Mpz.New();
  BEGIN
    Mpz.abs(num, a.num);
    Mpz.set(den, a.den);
    RETURN NEW(T, num := num, den := den)
    (* Already normalized; den unchanged, |num| *)
  END Abs;

PROCEDURE Compare(a, b: T): INTEGER =
  VAR lhs := Mpz.New();
      rhs := Mpz.New();
      result: INTEGER;
  BEGIN
    (* Compare a.num/a.den vs b.num/b.den by cross-multiplying.
       Both denominators are positive, so sign is preserved. *)
    Mpz.mul(lhs, a.num, b.den);
    Mpz.mul(rhs, b.num, a.den);
    result := Mpz.cmp(lhs, rhs);
    IF result < 0 THEN RETURN -1
    ELSIF result > 0 THEN RETURN 1
    ELSE RETURN 0
    END
  END Compare;

PROCEDURE IsZero(a: T): BOOLEAN =
  BEGIN
    RETURN Mpz.cmp(a.num, MpzZero) = 0
  END IsZero;

PROCEDURE IsPositive(a: T): BOOLEAN =
  BEGIN
    (* den is always positive, so sign(rational) = sign(num) *)
    RETURN Mpz.cmp(a.num, MpzZero) > 0
  END IsPositive;

PROCEDURE IsNegative(a: T): BOOLEAN =
  BEGIN
    RETURN Mpz.cmp(a.num, MpzZero) < 0
  END IsNegative;

PROCEDURE Numerator(x: T): SchemeObject.T =
  BEGIN
    RETURN SchemeInt.MpzToScheme(x.num)
  END Numerator;

PROCEDURE Denominator(x: T): SchemeObject.T =
  BEGIN
    RETURN SchemeInt.MpzToScheme(x.den)
  END Denominator;

PROCEDURE ToLongReal(x: T): LONGREAL =
  BEGIN
    RETURN Mpz.get_d(x.num) / Mpz.get_d(x.den)
  END ToLongReal;

PROCEDURE Format(x: T): TEXT =
  BEGIN
    RETURN Mpz.FormatDecimal(x.num) & "/" & Mpz.FormatDecimal(x.den)
  END Format;

PROCEDURE FromInt(x: SchemeObject.T): T =
  VAR num: Mpz.T;
      den := Mpz.New();
  BEGIN
    TYPECASE x OF
      SchemeInt.T(ri) => num := Mpz.NewInt(ri^);
    | Mpz.T(m)        => num := Mpz.New(); Mpz.set(num, m);
    ELSE
      <* ASSERT FALSE *>
    END;
    Mpz.set(den, MpzOne);
    RETURN NEW(T, num := num, den := den)
  END FromInt;

BEGIN
  MpzZero := Mpz.NewInt(0);
  MpzOne  := Mpz.NewInt(1);
END SchemeRational.
