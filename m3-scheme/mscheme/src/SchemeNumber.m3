(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeNumber;
IMPORT SchemeObject, SchemeExact, SchemeInexact, SchemeComplex,
       SchemeDual, SchemeInt, SchemeLongReal;
FROM Scheme IMPORT E;
FROM SchemeUtils IMPORT StringifyT;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeExact.Is(x) OR SchemeInexact.Is(x) OR
           (x # NIL AND (ISTYPE(x, SchemeComplex.T) OR
                         ISTYPE(x, SchemeDual.T)))
  END Is;

PROCEDURE IsExact(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      NULL => RETURN FALSE
    | SchemeDual.T(d) =>
      RETURN IsExact(d.re) AND IsExact(d.eps)
    | SchemeComplex.T(c) =>
      RETURN SchemeExact.Is(c.re) AND SchemeExact.Is(c.im)
    ELSE
      RETURN SchemeExact.Is(x)
    END
  END IsExact;

PROCEDURE IsInexact(x: SchemeObject.T): BOOLEAN =
  BEGIN
    TYPECASE x OF
      NULL => RETURN FALSE
    | SchemeDual.T(d) =>
      RETURN IsInexact(d.re) OR IsInexact(d.eps)
    | SchemeComplex.T(c) =>
      RETURN SchemeInexact.Is(c.re) OR SchemeInexact.Is(c.im)
    ELSE
      RETURN SchemeInexact.Is(x)
    END
  END IsInexact;

(* Promote an exact number to inexact LONGREAL for mixed arithmetic *)
PROCEDURE ExactToInexact(x: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeInexact.FromLongReal(SchemeExact.ToLongReal(x))
  END ExactToInexact;

PROCEDURE CheckNumber(x: SchemeObject.T) RAISES {E} =
  BEGIN
    IF NOT Is(x) THEN
      RAISE E("expected a number, got: " & StringifyT(x))
    END
  END CheckNumber;

(* Promote a real number to complex with im=0 *)
PROCEDURE ToComplex(x: SchemeObject.T): SchemeComplex.T =
  BEGIN
    TYPECASE x OF
      NULL => RETURN NEW(SchemeComplex.T, re := SchemeInt.Zero, im := SchemeInt.Zero)
    | SchemeComplex.T(c) => RETURN c
    ELSE
      RETURN NEW(SchemeComplex.T, re := x, im := SchemeInt.Zero)
    END
  END ToComplex;

(* Real-only dispatch (no complex) *)
PROCEDURE RealAdd(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeExact.Add(a, b)
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Add(ExactToInexact(a), b)
      END
    ELSIF SchemeInexact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeInexact.Add(a, ExactToInexact(b))
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Add(a, b)
      END
    END;
    IF NOT Is(a) THEN CheckNumber(a) END;
    CheckNumber(b);
    RETURN NIL (* notreached *)
  END RealAdd;

PROCEDURE RealSub(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeExact.Sub(a, b)
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Sub(ExactToInexact(a), b)
      END
    ELSIF SchemeInexact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeInexact.Sub(a, ExactToInexact(b))
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Sub(a, b)
      END
    END;
    IF NOT Is(a) THEN CheckNumber(a) END;
    CheckNumber(b);
    RETURN NIL
  END RealSub;

PROCEDURE RealMul(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeExact.Mul(a, b)
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Mul(ExactToInexact(a), b)
      END
    ELSIF SchemeInexact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeInexact.Mul(a, ExactToInexact(b))
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Mul(a, b)
      END
    END;
    IF NOT Is(a) THEN CheckNumber(a) END;
    CheckNumber(b);
    RETURN NIL
  END RealMul;

PROCEDURE RealDiv(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        IF SchemeExact.IsZero(b) THEN
          RAISE E("/: division by zero")
        END;
        RETURN SchemeExact.Div(a, b)
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Div(ExactToInexact(a), b)
      END
    ELSIF SchemeInexact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeInexact.Div(a, ExactToInexact(b))
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Div(a, b)
      END
    END;
    IF NOT Is(a) THEN CheckNumber(a) END;
    CheckNumber(b);
    RETURN NIL
  END RealDiv;

(* Promote a non-dual to dual with eps=0 *)
PROCEDURE ToDual(x: SchemeObject.T): SchemeDual.T =
  BEGIN
    TYPECASE x OF
      SchemeDual.T(d) => RETURN d
    ELSE
      RETURN NEW(SchemeDual.T, re := x, eps := SchemeInt.Zero)
    END
  END ToDual;

PROCEDURE IsDual(x: SchemeObject.T): BOOLEAN =
  BEGIN RETURN x # NIL AND ISTYPE(x, SchemeDual.T) END IsDual;

(* --- Full-tower Arithmetic (with dual and complex) --- *)

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      RETURN SchemeDual.Add(ToDual(a), ToDual(b))
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      RETURN SchemeComplex.Add(ToComplex(a), ToComplex(b))
    END;
    RETURN RealAdd(a, b)
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      RETURN SchemeDual.Sub(ToDual(a), ToDual(b))
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      RETURN SchemeComplex.Sub(ToComplex(a), ToComplex(b))
    END;
    RETURN RealSub(a, b)
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      RETURN SchemeDual.Mul(ToDual(a), ToDual(b))
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      RETURN SchemeComplex.Mul(ToComplex(a), ToComplex(b))
    END;
    RETURN RealMul(a, b)
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      RETURN SchemeDual.Div(ToDual(a), ToDual(b))
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      RETURN SchemeComplex.Div(ToComplex(a), ToComplex(b))
    END;
    RETURN RealDiv(a, b)
  END Div;

PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    TYPECASE a OF
      NULL => CheckNumber(a); RETURN NIL
    | SchemeDual.T(d) => RETURN SchemeDual.Neg(d)
    | SchemeComplex.T(c) => RETURN SchemeComplex.Neg(c)
    ELSE
      IF SchemeExact.Is(a) THEN
        RETURN SchemeExact.Neg(a)
      ELSIF SchemeInexact.Is(a) THEN
        RETURN SchemeInexact.Neg(a)
      END;
      CheckNumber(a);
      RETURN NIL
    END
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    TYPECASE a OF
      NULL => CheckNumber(a); RETURN NIL
    | SchemeComplex.T(c) =>
      RETURN SchemeComplex.Magnitude(c)
    ELSE
      IF SchemeExact.Is(a) THEN
        RETURN SchemeExact.Abs(a)
      ELSIF SchemeInexact.Is(a) THEN
        RETURN SchemeInexact.Abs(a)
      END;
      CheckNumber(a);
      RETURN NIL
    END
  END Abs;

(* --- Comparison --- *)

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      RAISE E("ordering not defined for dual numbers")
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      RAISE E("ordering not defined for complex numbers")
    END;
    IF SchemeExact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeExact.Compare(a, b)
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Compare(ExactToInexact(a), b)
      END
    ELSIF SchemeInexact.Is(a) THEN
      IF SchemeExact.Is(b) THEN
        RETURN SchemeInexact.Compare(a, ExactToInexact(b))
      ELSIF SchemeInexact.Is(b) THEN
        RETURN SchemeInexact.Compare(a, b)
      END
    END;
    IF NOT Is(a) THEN CheckNumber(a) END;
    CheckNumber(b);
    RETURN 0
  END Compare;

PROCEDURE Equal(a, b: SchemeObject.T): BOOLEAN RAISES {E} =
  BEGIN
    IF IsDual(a) OR IsDual(b) THEN
      IF IsDual(a) AND IsDual(b) THEN
        RETURN SchemeDual.Equal(NARROW(a, SchemeDual.T),
                                NARROW(b, SchemeDual.T))
      ELSE
        (* One dual, one non-dual: equal iff eps=0 and re matches.
           But dual with eps=0 would have been demoted,
           so this is always FALSE. *)
        RETURN FALSE
      END
    END;
    IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) OR
       (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
      IF (a # NIL AND ISTYPE(a, SchemeComplex.T)) AND
         (b # NIL AND ISTYPE(b, SchemeComplex.T)) THEN
        RETURN SchemeComplex.Equal(
                 NARROW(a, SchemeComplex.T),
                 NARROW(b, SchemeComplex.T))
      ELSE
        RETURN FALSE
      END
    END;
    RETURN Compare(a, b) = 0
  END Equal;

(* --- Conversion --- *)

PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL RAISES {E} =
  BEGIN
    TYPECASE x OF
      SchemeDual.T =>
      RAISE E("cannot convert dual to real")
    | SchemeComplex.T =>
      RAISE E("cannot convert complex to real")
    ELSE
      IF SchemeExact.Is(x) THEN
        RETURN SchemeExact.ToLongReal(x)
      ELSIF SchemeInexact.Is(x) THEN
        RETURN SchemeInexact.ToLongReal(x)
      END;
      CheckNumber(x);
      RETURN 0.0d0
    END
  END ToLongReal;

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER RAISES {E} =
  BEGIN
    TYPECASE x OF
      SchemeDual.T =>
      RAISE E("cannot convert dual to integer")
    | SchemeComplex.T =>
      RAISE E("cannot convert complex to integer")
    ELSE
      IF SchemeExact.Is(x) THEN
        RETURN SchemeExact.ToInteger(x)
      ELSIF SchemeInexact.Is(x) THEN
        RETURN SchemeLongReal.Int(x)
      END;
      CheckNumber(x);
      RETURN 0
    END
  END ToInteger;

PROCEDURE CheckReal(x: SchemeObject.T) RAISES {E} =
  BEGIN
    TYPECASE x OF
      NULL => RAISE E("not a real number: ()")
    | SchemeDual.T(d) =>
      RAISE E("not a real number: " & SchemeDual.Format(d))
    | SchemeComplex.T =>
      RAISE E("not a real number: " & SchemeComplex.Format(NARROW(x, SchemeComplex.T)))
    ELSE (* ok *)
    END
  END CheckReal;

PROCEDURE IsZero(x: SchemeObject.T): BOOLEAN RAISES {E} =
  BEGIN
    TYPECASE x OF
      NULL => RAISE E("expected a number, got: ()")
    | SchemeDual.T(d) =>
      RETURN IsZero(d.re) AND IsZero(d.eps)
    | SchemeComplex.T(c) =>
      RETURN Equal(c.re, SchemeInt.Zero) AND Equal(c.im, SchemeInt.Zero)
    ELSE
      IF SchemeExact.Is(x) THEN RETURN SchemeExact.IsZero(x)
      ELSIF SchemeInexact.Is(x) THEN
        RETURN SchemeInexact.ToLongReal(x) = 0.0d0
      END;
      CheckNumber(x);
      RETURN FALSE
    END
  END IsZero;


PROCEDURE Format(x: SchemeObject.T): TEXT =
  BEGIN
    TYPECASE x OF
      NULL => RETURN "()"
    | SchemeDual.T(d) => RETURN SchemeDual.Format(d)
    | SchemeComplex.T(c) => RETURN SchemeComplex.Format(c)
    ELSE
      IF SchemeExact.Is(x) THEN
        RETURN SchemeExact.Format(x)
      ELSIF SchemeInexact.Is(x) THEN
        RETURN SchemeInexact.Format(x)
      ELSE
        RETURN "<not-a-number>"
      END
    END
  END Format;

BEGIN
END SchemeNumber.
