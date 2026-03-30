(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeMpfr;
IMPORT SchemeObject, SchemeInt, SchemeRational, Mpfr, Mpz;

PROCEDURE New(prec: CARDINAL): T =
  BEGIN
    RETURN NEW(T, val := Mpfr.New(prec), prec := prec)
  END New;

PROCEDURE FromLR(v: LONGREAL; prec: CARDINAL): T =
  VAR r := New(prec);
  BEGIN
    EVAL Mpfr.SetLR(r.val, v);
    RETURN r
  END FromLR;

PROCEDURE FromExact(v: SchemeObject.T; prec: CARDINAL): T =
  VAR r := New(prec);
  BEGIN
    TYPECASE v OF
      SchemeInt.T(ri) =>
      EVAL Mpfr.SetInt(r.val, ri^);
    | Mpz.T(m) =>
      (* Convert bignum via double — loses precision for very large values,
         but MPFR doesn't have a direct set_mpz in our bindings *)
      EVAL Mpfr.SetLR(r.val, Mpz.get_d(m));
    | SchemeRational.T(q) =>
      (* Compute num/den in MPFR *)
      VAR rn := Mpfr.New(prec);
          rd := Mpfr.New(prec);
      BEGIN
        EVAL Mpfr.SetLR(rn, Mpz.get_d(q.num));
        EVAL Mpfr.SetLR(rd, Mpz.get_d(q.den));
        EVAL Mpfr.Div(r.val, rn, rd);
      END
    ELSE
      <* ASSERT FALSE *>
    END;
    RETURN r
  END FromExact;

PROCEDURE FromMpfr(v: Mpfr.T): T =
  (* Wrap an existing Mpfr.T.  We don't have a public GetPrec in our
     Mpfr bindings, so use 53 as default (LONGREAL equivalent). *)
  BEGIN
    RETURN NEW(T, val := v, prec := 53)
  END FromMpfr;

PROCEDURE ToLR(v: T): LONGREAL =
  BEGIN
    RETURN Mpfr.GetLR(v.val)
  END ToLR;

PROCEDURE GetPrec(v: T): CARDINAL =
  BEGIN
    RETURN v.prec
  END GetPrec;

PROCEDURE MaxPrec(a, b: T): CARDINAL =
  BEGIN
    RETURN MAX(a.prec, b.prec)
  END MaxPrec;

PROCEDURE Add(a, b: T): T =
  VAR r := New(MaxPrec(a, b));
  BEGIN
    EVAL Mpfr.Add(r.val, a.val, b.val);
    RETURN r
  END Add;

PROCEDURE Sub(a, b: T): T =
  VAR r := New(MaxPrec(a, b));
  BEGIN
    EVAL Mpfr.Sub(r.val, a.val, b.val);
    RETURN r
  END Sub;

PROCEDURE Mul(a, b: T): T =
  VAR r := New(MaxPrec(a, b));
  BEGIN
    EVAL Mpfr.Mul(r.val, a.val, b.val);
    RETURN r
  END Mul;

PROCEDURE Div(a, b: T): T =
  VAR r := New(MaxPrec(a, b));
  BEGIN
    EVAL Mpfr.Div(r.val, a.val, b.val);
    RETURN r
  END Div;

PROCEDURE Neg(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Neg(r.val, a.val);
    RETURN r
  END Neg;

PROCEDURE Abs(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Abs(r.val, a.val);
    RETURN r
  END Abs;

PROCEDURE Sqrt(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Sqrt(r.val, a.val);
    RETURN r
  END Sqrt;

PROCEDURE Sin(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Sin(r.val, a.val);
    RETURN r
  END Sin;

PROCEDURE Cos(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Cos(r.val, a.val);
    RETURN r
  END Cos;

PROCEDURE Tan(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Tan(r.val, a.val);
    RETURN r
  END Tan;

PROCEDURE Exp(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Exp(r.val, a.val);
    RETURN r
  END Exp;

PROCEDURE Log(a: T): T =
  VAR r := New(a.prec);
  BEGIN
    EVAL Mpfr.Log(r.val, a.val);
    RETURN r
  END Log;

PROCEDURE Compare(a, b: T): INTEGER =
  VAR result := Mpfr.Compare(a.val, b.val);
  BEGIN
    IF result < 0 THEN RETURN -1
    ELSIF result > 0 THEN RETURN 1
    ELSE RETURN 0
    END
  END Compare;

PROCEDURE Format(v: T): TEXT =
  BEGIN
    RETURN Mpfr.Format(v.val)
  END Format;

BEGIN
END SchemeMpfr.
