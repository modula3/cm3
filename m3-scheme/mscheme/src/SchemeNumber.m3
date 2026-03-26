(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeNumber;
IMPORT SchemeObject, SchemeExact, SchemeInexact, SchemeLongReal;
FROM Scheme IMPORT E;
FROM SchemeUtils IMPORT StringifyT;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeExact.Is(x) OR SchemeInexact.Is(x)
  END Is;

PROCEDURE IsExact(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeExact.Is(x)
  END IsExact;

PROCEDURE IsInexact(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeInexact.Is(x)
  END IsInexact;

(* Promote an exact integer to inexact LONGREAL for mixed arithmetic *)
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

(* --- Arithmetic --- *)

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
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
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
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
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
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
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T RAISES {E} =
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
  END Div;

PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      RETURN SchemeExact.Neg(a)
    ELSIF SchemeInexact.Is(a) THEN
      RETURN SchemeInexact.Neg(a)
    END;
    CheckNumber(a);
    RETURN NIL
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T RAISES {E} =
  BEGIN
    IF SchemeExact.Is(a) THEN
      RETURN SchemeExact.Abs(a)
    ELSIF SchemeInexact.Is(a) THEN
      RETURN SchemeInexact.Abs(a)
    END;
    CheckNumber(a);
    RETURN NIL
  END Abs;

(* --- Comparison --- *)

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER RAISES {E} =
  BEGIN
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
    RETURN Compare(a, b) = 0
  END Equal;

(* --- Conversion --- *)

PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL RAISES {E} =
  BEGIN
    IF SchemeExact.Is(x) THEN
      RETURN SchemeExact.ToLongReal(x)
    ELSIF SchemeInexact.Is(x) THEN
      RETURN SchemeInexact.ToLongReal(x)
    END;
    CheckNumber(x);
    RETURN 0.0d0
  END ToLongReal;

PROCEDURE ToInteger(x: SchemeObject.T): INTEGER RAISES {E} =
  BEGIN
    IF SchemeExact.Is(x) THEN
      RETURN SchemeExact.ToInteger(x)
    ELSIF SchemeInexact.Is(x) THEN
      RETURN SchemeLongReal.Int(x)
    END;
    CheckNumber(x);
    RETURN 0
  END ToInteger;

PROCEDURE Format(x: SchemeObject.T): TEXT =
  BEGIN
    IF SchemeExact.Is(x) THEN
      RETURN SchemeExact.Format(x)
    ELSIF SchemeInexact.Is(x) THEN
      RETURN SchemeInexact.Format(x)
    ELSE
      RETURN "<not-a-number>"
    END
  END Format;

BEGIN
END SchemeNumber.
