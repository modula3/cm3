(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeExact;
IMPORT SchemeObject, SchemeInt, Mpz, Fmt;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsExactInt(x)
  END Is;

PROCEDURE IsInteger(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsExactInt(x)
  END IsInteger;

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsZero(x)
  END IsZero;

PROCEDURE IsPositive(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsPositive(x)
  END IsPositive;

PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInt.IsNegative(x)
  END IsNegative;

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T =
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
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T =
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
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T =
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
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T =
  VAR
    ma := SchemeInt.ToMpz(a);
    mb := SchemeInt.ToMpz(b);
    mq := Mpz.New();
  BEGIN
    <* ASSERT Mpz.cmp(mb, MpzZero) # 0 *>
    Mpz.tdiv_q(mq, ma, mb);
    RETURN SchemeInt.MpzToScheme(mq)
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
    RETURN Sub(SchemeInt.Zero, a)
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ri) =>
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
    RETURN SchemeInt.Compare(a, b)
  END Compare;

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER =
  BEGIN
    RETURN SchemeInt.ToInteger(x)
  END ToInteger;

PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL =
  BEGIN
    TYPECASE x OF
      SchemeInt.T(ri) => RETURN FLOAT(ri^, LONGREAL)
    | Mpz.T(m) => RETURN Mpz.get_d(m)
    ELSE
      <* ASSERT FALSE *>
    END
  END ToLongReal;

PROCEDURE Format(x: SchemeObject.T): TEXT =
  BEGIN
    TYPECASE x OF
      SchemeInt.T(ri) =>
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
