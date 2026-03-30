(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeExact;
IMPORT SchemeObject, SchemeInt, SchemeRational, Mpz, Fmt;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsExactInt(x) OR
           (x # NIL AND ISTYPE(x, SchemeRational.T))
  END Is;

PROCEDURE IsInteger(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsExactInt(x)
  END IsInteger;

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN SchemeRational.IsZero(r)
    ELSE
      RETURN SchemeInt.IsZero(x)
    END
  END IsZero;

PROCEDURE IsPositive(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN SchemeRational.IsPositive(r)
    ELSE
      RETURN SchemeInt.IsPositive(x)
    END
  END IsPositive;

PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN SchemeRational.IsNegative(r)
    ELSE
      RETURN SchemeInt.IsNegative(x)
    END
  END IsNegative;

(* Helper: ensure operand is rational.  If it's an integer,
   promote to rational with denominator 1. *)
PROCEDURE ToRat(x: SchemeObject.T): SchemeRational.T =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN r
    ELSE
      RETURN SchemeRational.FromInt(x)
    END
  END ToRat;

(* Integer-only arithmetic (fixnum x fixnum, fixnum x bignum, etc.) *)
PROCEDURE IntAdd(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Add(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END IntAdd;

PROCEDURE IntSub(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Sub(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END IntSub;

PROCEDURE IntMul(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Mul(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END IntMul;

(* Dispatch: if either operand is rational, promote both and
   use rational arithmetic.  Otherwise use integer arithmetic. *)

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeRational.T) OR
       ISTYPE(b, SchemeRational.T) THEN
      RETURN SchemeRational.Add(ToRat(a), ToRat(b))
    ELSE
      RETURN IntAdd(a, b)
    END
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeRational.T) OR
       ISTYPE(b, SchemeRational.T) THEN
      RETURN SchemeRational.Sub(ToRat(a), ToRat(b))
    ELSE
      RETURN IntSub(a, b)
    END
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeRational.T) OR
       ISTYPE(b, SchemeRational.T) THEN
      RETURN SchemeRational.Mul(ToRat(a), ToRat(b))
    ELSE
      RETURN IntMul(a, b)
    END
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T =
  (* Phase 2: exact division now returns exact rational.
     a/b is constructed as rational and normalized;
     if the result is an integer, New demotes it. *)
  VAR
    num, den: Mpz.T;
  BEGIN
    IF ISTYPE(a, SchemeRational.T) OR
       ISTYPE(b, SchemeRational.T) THEN
      RETURN SchemeRational.Div(ToRat(a), ToRat(b))
    ELSE
      (* Both are integers: construct num/den rational *)
      num := Mpz.New(); Mpz.set(num, SchemeInt.ToMpz(a));
      den := Mpz.New(); Mpz.set(den, SchemeInt.ToMpz(b));
      <* ASSERT Mpz.cmp(den, MpzZero) # 0 *>
      RETURN SchemeRational.New(num, den)
    END
  END Div;

PROCEDURE Rem(a, b: SchemeObject.T): SchemeObject.T =
  VAR
    ma := SchemeInt.ToMpz(a);
    mb := SchemeInt.ToMpz(b);
    mr := Mpz.New();
  BEGIN
    <* ASSERT Mpz.cmp(mb, MpzZero) # 0 *>
    Mpz.tdiv_r(mr, ma, mb);
    RETURN SchemeInt.MpzToScheme(mr)
  END Rem;

PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeRational.T(r) => RETURN SchemeRational.Neg(r)
    ELSE
      RETURN IntSub(SchemeInt.Zero, a)
    END
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeRational.T(r) => RETURN SchemeRational.Abs(r)
    | SchemeInt.T(ri) =>
      IF ri^ >= 0 THEN RETURN a
      ELSIF ri^ = FIRST(INTEGER) THEN
        (* -FIRST(INTEGER) overflows INTEGER; promote *)
        VAR mr := Mpz.NewInt(ri^); BEGIN
          Mpz.neg(mr, mr); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE
        RETURN SchemeInt.FromI(-ri^)
      END
    | Mpz.T(m) =>
      VAR mr := Mpz.New(); BEGIN
        Mpz.abs(mr, m); RETURN SchemeInt.MpzToScheme(mr)
      END
    ELSE <* ASSERT FALSE *>
    END
  END Abs;

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER =
  BEGIN
    IF ISTYPE(a, SchemeRational.T) OR
       ISTYPE(b, SchemeRational.T) THEN
      RETURN SchemeRational.Compare(ToRat(a), ToRat(b))
    ELSE
      RETURN SchemeInt.Compare(a, b)
    END
  END Compare;

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER =
  BEGIN
    RETURN SchemeInt.ToInteger(x)
  END ToInteger;

PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN SchemeRational.ToLongReal(r)
    | SchemeInt.T(ri) => RETURN FLOAT(ri^, LONGREAL)
    | Mpz.T(m) => RETURN Mpz.get_d(m)
    ELSE
      <* ASSERT FALSE *>
    END
  END ToLongReal;

PROCEDURE Format(x: SchemeObject.T): TEXT =
  BEGIN
    TYPECASE x OF
      SchemeRational.T(r) => RETURN SchemeRational.Format(r)
    | SchemeInt.T(ri) =>
      RETURN Fmt.Int(ri^)
    | Mpz.T(m) =>
      RETURN Mpz.FormatDecimal(m)
    ELSE
      <* ASSERT FALSE *>
    END
  END Format;

PROCEDURE FromI(x: INTEGER): SchemeObject.T =
  BEGIN
    RETURN SchemeInt.FromI(x)
  END FromI;

VAR MpzZero: Mpz.T;

BEGIN
  Zero   := SchemeInt.Zero;
  One    := SchemeInt.One;
  NegOne := SchemeInt.NegOne;
  Two    := SchemeInt.Two;

  MpzZero := Mpz.NewInt(0);
END SchemeExact.
