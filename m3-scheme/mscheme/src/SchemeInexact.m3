(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeInexact;
IMPORT SchemeObject, SchemeLongReal, SchemeMpfr;
IMPORT Fmt, Text;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN x # NIL AND (ISTYPE(x, SchemeLongReal.T) OR
                         ISTYPE(x, SchemeMpfr.T))
  END Is;

(* Internal: promote a SchemeLongReal to SchemeMpfr at given precision *)
PROCEDURE LRToMpfr(x: SchemeLongReal.T; prec: CARDINAL): SchemeMpfr.T =
  BEGIN
    RETURN SchemeMpfr.FromLR(x^, prec)
  END LRToMpfr;

(* Internal: ensure operand is SchemeMpfr.T.
   If it's a SchemeLongReal, promote at the given precision. *)
PROCEDURE ToMpfr(x: SchemeObject.T; prec: CARDINAL): SchemeMpfr.T =
  BEGIN
    TYPECASE x OF
      SchemeMpfr.T(m) => RETURN m
    | SchemeLongReal.T(lr) => RETURN LRToMpfr(lr, prec)
    ELSE
      <* ASSERT FALSE *>
    END
  END ToMpfr;

(* Get the Mpfr precision if applicable, else 0 *)
PROCEDURE MpfrPrec(x: SchemeObject.T): CARDINAL =
  BEGIN
    TYPECASE x OF
      SchemeMpfr.T(m) => RETURN m.prec
    ELSE
      RETURN 0
    END
  END MpfrPrec;

PROCEDURE ToLR(x: SchemeObject.T): LONGREAL =
  BEGIN
    TYPECASE x OF
      SchemeLongReal.T(lr) => RETURN lr^
    | SchemeMpfr.T(m) => RETURN SchemeMpfr.ToLR(m)
    ELSE
      <* ASSERT FALSE *>
    END
  END ToLR;

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeMpfr.T) OR ISTYPE(b, SchemeMpfr.T) THEN
      VAR p := MAX(MpfrPrec(a), MpfrPrec(b)); BEGIN
        IF p = 0 THEN p := 53 END;
        RETURN SchemeMpfr.Add(ToMpfr(a, p), ToMpfr(b, p))
      END
    ELSE
      RETURN SchemeLongReal.FromLR(ToLR(a) + ToLR(b))
    END
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeMpfr.T) OR ISTYPE(b, SchemeMpfr.T) THEN
      VAR p := MAX(MpfrPrec(a), MpfrPrec(b)); BEGIN
        IF p = 0 THEN p := 53 END;
        RETURN SchemeMpfr.Sub(ToMpfr(a, p), ToMpfr(b, p))
      END
    ELSE
      RETURN SchemeLongReal.FromLR(ToLR(a) - ToLR(b))
    END
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeMpfr.T) OR ISTYPE(b, SchemeMpfr.T) THEN
      VAR p := MAX(MpfrPrec(a), MpfrPrec(b)); BEGIN
        IF p = 0 THEN p := 53 END;
        RETURN SchemeMpfr.Mul(ToMpfr(a, p), ToMpfr(b, p))
      END
    ELSE
      RETURN SchemeLongReal.FromLR(ToLR(a) * ToLR(b))
    END
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    IF ISTYPE(a, SchemeMpfr.T) OR ISTYPE(b, SchemeMpfr.T) THEN
      VAR p := MAX(MpfrPrec(a), MpfrPrec(b)); BEGIN
        IF p = 0 THEN p := 53 END;
        RETURN SchemeMpfr.Div(ToMpfr(a, p), ToMpfr(b, p))
      END
    ELSE
      RETURN SchemeLongReal.FromLR(ToLR(a) / ToLR(b))
    END
  END Div;

PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeMpfr.T(m) => RETURN SchemeMpfr.Neg(m)
    ELSE
      RETURN SchemeLongReal.FromLR(-ToLR(a))
    END
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    TYPECASE a OF
      SchemeMpfr.T(m) => RETURN SchemeMpfr.Abs(m)
    ELSE
      RETURN SchemeLongReal.FromLR(ABS(ToLR(a)))
    END
  END Abs;

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER =
  BEGIN
    IF ISTYPE(a, SchemeMpfr.T) OR ISTYPE(b, SchemeMpfr.T) THEN
      VAR p := MAX(MpfrPrec(a), MpfrPrec(b)); BEGIN
        IF p = 0 THEN p := 53 END;
        RETURN SchemeMpfr.Compare(ToMpfr(a, p), ToMpfr(b, p))
      END
    ELSE
      VAR la := ToLR(a); lb := ToLR(b); BEGIN
        IF la < lb THEN RETURN -1
        ELSIF la > lb THEN RETURN 1
        ELSE RETURN 0
        END
      END
    END
  END Compare;

PROCEDURE ToLongReal(x: SchemeObject.T): LONGREAL =
  BEGIN
    RETURN ToLR(x)
  END ToLongReal;

PROCEDURE FromLongReal(x: LONGREAL): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(x)
  END FromLongReal;

PROCEDURE FromInteger(x: INTEGER): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromI(x)
  END FromInteger;

PROCEDURE Format(x: SchemeObject.T): TEXT =
  BEGIN
    TYPECASE x OF
      SchemeMpfr.T(m) => RETURN SchemeMpfr.Format(m)
    ELSE
      WITH txt = Fmt.LongReal(ToLR(x)) DO
        IF Text.FindChar(txt, '.') < 0 AND
           Text.FindChar(txt, 'e') < 0 AND
           Text.FindChar(txt, 'E') < 0 AND
           Text.FindChar(txt, 'N') < 0 AND
           Text.FindChar(txt, 'I') < 0 THEN
          RETURN txt & ".0"
        ELSE
          RETURN txt
        END
      END
    END
  END Format;

BEGIN
END SchemeInexact.
