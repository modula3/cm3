(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE SchemeDual;
IMPORT SchemeObject, SchemeNumber, SchemeExact, SchemeInt, SchemeComplex;
FROM Scheme IMPORT E;
IMPORT Text;

PROCEDURE IsExactZero(x: SchemeObject.T): BOOLEAN =
  BEGIN
    RETURN SchemeExact.Is(x) AND SchemeExact.IsZero(x)
  END IsExactZero;

PROCEDURE New(re, eps: SchemeObject.T): SchemeObject.T =
  BEGIN
    (* Demote to real if epsilon part is exactly zero *)
    IF IsExactZero(eps) THEN
      RETURN re
    END;
    RETURN NEW(T, re := re, eps := eps)
  END New;

PROCEDURE RealPart(x: T): SchemeObject.T =
  BEGIN
    RETURN x.re
  END RealPart;

PROCEDURE EpsilonPart(x: T): SchemeObject.T =
  BEGIN
    RETURN x.eps
  END EpsilonPart;

(* Promote a non-dual to dual with eps=0 *)
PROCEDURE ToDual(x: SchemeObject.T): T =
  BEGIN
    TYPECASE x OF
      T(d) => RETURN d
    ELSE
      RETURN NEW(T, re := x, eps := SchemeInt.Zero)
    END
  END ToDual;

PROCEDURE Add(a, b: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Add(a.re, b.re),
               SchemeNumber.Add(a.eps, b.eps))
  END Add;

PROCEDURE Sub(a, b: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Sub(a.re, b.re),
               SchemeNumber.Sub(a.eps, b.eps))
  END Sub;

PROCEDURE Mul(a, b: T): SchemeObject.T RAISES {E} =
  (* (a.re + a.eps*e) * (b.re + b.eps*e) =
     a.re*b.re + (a.re*b.eps + a.eps*b.re)*e
     (the a.eps*b.eps*e^2 term vanishes) *)
  BEGIN
    RETURN New(SchemeNumber.Mul(a.re, b.re),
               SchemeNumber.Add(
                 SchemeNumber.Mul(a.re, b.eps),
                 SchemeNumber.Mul(a.eps, b.re)))
  END Mul;

PROCEDURE Div(a, b: T): SchemeObject.T RAISES {E} =
  (* (a.re + a.eps*e) / (b.re + b.eps*e) =
     a.re/b.re + (a.eps*b.re - a.re*b.eps)/b.re^2 * e *)
  VAR b2 := SchemeNumber.Mul(b.re, b.re);
  BEGIN
    RETURN New(SchemeNumber.Div(a.re, b.re),
               SchemeNumber.Div(
                 SchemeNumber.Sub(
                   SchemeNumber.Mul(a.eps, b.re),
                   SchemeNumber.Mul(a.re, b.eps)),
                 b2))
  END Div;

PROCEDURE Neg(a: T): SchemeObject.T RAISES {E} =
  BEGIN
    RETURN New(SchemeNumber.Neg(a.re),
               SchemeNumber.Neg(a.eps))
  END Neg;

PROCEDURE Equal(a, b: T): BOOLEAN RAISES {E} =
  BEGIN
    RETURN SchemeNumber.Equal(a.re, b.re) AND
           SchemeNumber.Equal(a.eps, b.eps)
  END Equal;

PROCEDURE IsNegative(x: SchemeObject.T): BOOLEAN =
  BEGIN
    IF SchemeExact.Is(x) THEN
      RETURN SchemeExact.IsNegative(x)
    ELSE
      VAR txt := SchemeNumber.Format(x); BEGIN
        RETURN txt # NIL AND Text.Length(txt) > 0 AND
               Text.GetChar(txt, 0) = '-'
      END
    END
  END IsNegative;

PROCEDURE NeedParens(x: SchemeObject.T): BOOLEAN =
  (* Only complex epsilon parts need parentheses *)
  BEGIN
    RETURN x # NIL AND ISTYPE(x, SchemeComplex.T)
  END NeedParens;

PROCEDURE Format(x: T): TEXT =
  VAR reTxt := SchemeNumber.Format(x.re);
      epsTxt := SchemeNumber.Format(x.eps);
      reIsZero := FALSE;
  BEGIN
    TRY reIsZero := SchemeNumber.IsZero(x.re) EXCEPT E => END;

    (* Wrap complex parts in parens *)
    IF NeedParens(x.eps) THEN
      epsTxt := "(" & epsTxt & ")"
    END;

    IF reIsZero THEN
      (* Just the epsilon part *)
      RETURN epsTxt & "eps"
    ELSE
      IF NeedParens(x.re) THEN
        reTxt := "(" & reTxt & ")"
      END;
      (* Check for negative epsilon to use - instead of +- *)
      IF IsNegative(x.eps) THEN
        RETURN reTxt & epsTxt & "eps"
      ELSE
        RETURN reTxt & "+" & epsTxt & "eps"
      END
    END
  END Format;

BEGIN
  EVAL SchemeInt.Zero;
END SchemeDual.
