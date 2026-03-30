(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeComplex;
IMPORT SchemeObject, SchemeNumber, SchemeExact, SchemeInt;
IMPORT SchemeLongReal, Math, Text;
FROM Scheme IMPORT E;

PROCEDURE IsExactZero(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeExact.Is(x) AND SchemeExact.IsZero(x)
  END IsExactZero;

PROCEDURE New(re, im: SchemeObject.T): SchemeObject.T =
  BEGIN
    (* Demote to real if imaginary part is exactly zero *)
    IF IsExactZero(im) THEN
      RETURN re
    END;
    RETURN NEW(T, re := re, im := im)
  END New;

PROCEDURE MakeRectangular(re, im: SchemeObject.T):
    SchemeObject.T RAISES {E} =
  BEGIN
    IF NOT SchemeNumber.Is(re) THEN
      RAISE E("make-rectangular: not a number: re")
    END;
    IF NOT SchemeNumber.Is(im) THEN
      RAISE E("make-rectangular: not a number: im")
    END;
    RETURN New(re, im)
  END MakeRectangular;

PROCEDURE MakePolar(mag, ang: SchemeObject.T):
    SchemeObject.T RAISES {E} =
  VAR m, a: LONGREAL;
      re, im: SchemeObject.T;
  BEGIN
    m := SchemeNumber.ToLongReal(mag);
    a := SchemeNumber.ToLongReal(ang);
    re := SchemeLongReal.FromLR(m * Math.cos(a));
    im := SchemeLongReal.FromLR(m * Math.sin(a));
    RETURN New(re, im)
  END MakePolar;

PROCEDURE RealPart(x: T): SchemeObject.T =
  BEGIN
    RETURN x.re
  END RealPart;

PROCEDURE ImagPart(x: T): SchemeObject.T =
  BEGIN
    RETURN x.im
  END ImagPart;

PROCEDURE Magnitude(x: SchemeObject.T): SchemeObject.T RAISES {E} =
  VAR re, im: LONGREAL;
  BEGIN
    TYPECASE x OF
      T(c) =>
      re := SchemeNumber.ToLongReal(c.re);
      im := SchemeNumber.ToLongReal(c.im);
      RETURN SchemeLongReal.FromLR(Math.sqrt(re*re + im*im))
    ELSE
      RETURN SchemeNumber.Abs(x)
    END
  END Magnitude;

PROCEDURE Angle(x: SchemeObject.T): SchemeObject.T RAISES {E} =
  VAR re, im: LONGREAL;
  BEGIN
    TYPECASE x OF
      T(c) =>
      re := SchemeNumber.ToLongReal(c.re);
      im := SchemeNumber.ToLongReal(c.im);
      RETURN SchemeLongReal.FromLR(Math.atan2(im, re))
    ELSE
      (* Real number: angle is 0 for positive/zero, pi for negative *)
      VAR v := SchemeNumber.ToLongReal(x); BEGIN
        IF v < 0.0d0 THEN
          RETURN SchemeLongReal.FromLR(Math.Pi)
        ELSE
          RETURN SchemeLongReal.FromLR(0.0d0)
        END
      END
    END
  END Angle;

PROCEDURE Add(a, b: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Add(a.re, b.re),
               SchemeNumber.Add(a.im, b.im))
  END Add;

PROCEDURE Sub(a, b: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Sub(a.re, b.re),
               SchemeNumber.Sub(a.im, b.im))
  END Sub;

PROCEDURE Mul(a, b: T): SchemeObject.T RAISES {E} =
  (* (a.re + a.im*i) * (b.re + b.im*i) =
     (a.re*b.re - a.im*b.im) + (a.re*b.im + a.im*b.re)*i *)
  VAR re := SchemeNumber.Sub(
              SchemeNumber.Mul(a.re, b.re),
              SchemeNumber.Mul(a.im, b.im));
      im := SchemeNumber.Add(
              SchemeNumber.Mul(a.re, b.im),
              SchemeNumber.Mul(a.im, b.re));
  BEGIN
    RETURN New(re, im)
  END Mul;

PROCEDURE Div(a, b: T): SchemeObject.T RAISES {E} =
  (* (a.re + a.im*i) / (b.re + b.im*i) =
     ((a.re*b.re + a.im*b.im) + (a.im*b.re - a.re*b.im)*i)
     / (b.re^2 + b.im^2) *)
  VAR denom := SchemeNumber.Add(
                 SchemeNumber.Mul(b.re, b.re),
                 SchemeNumber.Mul(b.im, b.im));
      reNum := SchemeNumber.Add(
                 SchemeNumber.Mul(a.re, b.re),
                 SchemeNumber.Mul(a.im, b.im));
      imNum := SchemeNumber.Sub(
                 SchemeNumber.Mul(a.im, b.re),
                 SchemeNumber.Mul(a.re, b.im));
  BEGIN
    RETURN New(SchemeNumber.Div(reNum, denom),
               SchemeNumber.Div(imNum, denom))
  END Div;

PROCEDURE Neg(a: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Neg(a.re),
               SchemeNumber.Neg(a.im))
  END Neg;

PROCEDURE Equal(a, b: T): BOOLEAN RAISES {E} =
  BEGIN
    RETURN SchemeNumber.Equal(a.re, b.re) AND
           SchemeNumber.Equal(a.im, b.im)
  END Equal;

PROCEDURE Format(x: T): TEXT =
  VAR reTxt := SchemeNumber.Format(x.re);
      imTxt := SchemeNumber.Format(x.im);
  BEGIN
    (* Check if imaginary part is negative *)
    IF SchemeExact.Is(x.im) THEN
      IF SchemeExact.IsNegative(x.im) THEN
        RETURN reTxt & imTxt & "i"
      ELSE
        RETURN reTxt & "+" & imTxt & "i"
      END
    ELSE
      (* For inexact, check the text for a leading minus *)
      IF imTxt # NIL AND
         Text.Length(imTxt) > 0 AND
         Text.GetChar(imTxt, 0) = '-' THEN
        RETURN reTxt & imTxt & "i"
      ELSE
        RETURN reTxt & "+" & imTxt & "i"
      END
    END
  END Format;

BEGIN
  (* SchemeInt.Zero used in IsExactZero — ensure SchemeInt is initialized *)
  EVAL SchemeInt.Zero;
END SchemeComplex.
