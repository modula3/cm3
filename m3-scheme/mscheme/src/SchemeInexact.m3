(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeInexact;
IMPORT SchemeObject, SchemeLongReal;
IMPORT Fmt, Text;

PROCEDURE Is(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN x # NIL AND ISTYPE(x, SchemeLongReal.T)
  END Is;

PROCEDURE ToLR(x: SchemeObject.T): LONGREAL =
  (* Internal: extract LONGREAL from an inexact value.
     Checked runtime error if not inexact. *)
  BEGIN
    TYPECASE x OF
      SchemeLongReal.T(lr) => RETURN lr^
    ELSE
      <* ASSERT FALSE *>
    END
  END ToLR;

PROCEDURE Add(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(ToLR(a) + ToLR(b))
  END Add;

PROCEDURE Sub(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(ToLR(a) - ToLR(b))
  END Sub;

PROCEDURE Mul(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(ToLR(a) * ToLR(b))
  END Mul;

PROCEDURE Div(a, b: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(ToLR(a) / ToLR(b))
  END Div;

PROCEDURE Neg(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(-ToLR(a))
  END Neg;

PROCEDURE Abs(a: SchemeObject.T): SchemeObject.T =
  BEGIN
    RETURN SchemeLongReal.FromLR(ABS(ToLR(a)))
  END Abs;

PROCEDURE Compare(a, b: SchemeObject.T): INTEGER =
  VAR la := ToLR(a); lb := ToLR(b);
  BEGIN
    IF la < lb THEN RETURN -1
    ELSIF la > lb THEN RETURN 1
    ELSE RETURN 0
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
  END Format;

BEGIN
END SchemeInexact.
