GENERIC MODULE ResidueClassBasic(R, GCD);
(* Arithmetic for Modula-3, see doc for details

   Abstract: ResidueClass numbers and basic operations *)

IMPORT Arithmetic;

<* UNUSED *>
CONST
  Module = "ResidueClassBasic.";


PROCEDURE NewZero (d: R.T; ): T =
  BEGIN
    <* ASSERT NOT R.IsZero(d) *>
    RETURN T{R.Zero, d};
  END NewZero;

PROCEDURE NewOne (d: R.T; ): T =
  BEGIN
    <* ASSERT NOT R.IsZero(d) *>
    RETURN T{R.One, d};
  END NewOne;

PROCEDURE FromRepresentative (x, d: R.T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    <* ASSERT NOT R.IsZero(d) *>
    RETURN T{R.Mod(x, d), d};
  END FromRepresentative;

PROCEDURE ToRepresentative (READONLY x: T; ): R.T =
  BEGIN
    RETURN x.r;
  END ToRepresentative;


PROCEDURE AssertEqualIdeal (READONLY x, y: T; ) =
  BEGIN
    <* ASSERT R.Equal(x.d, y.d) *>
  END AssertEqualIdeal;

PROCEDURE Add (READONLY x, y: T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    AssertEqualIdeal(x, y);
    RETURN T{R.Mod(R.Add(x.r, y.r), x.d), x.d};
  END Add;

PROCEDURE Sub (READONLY x, y: T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    AssertEqualIdeal(x, y);
    RETURN T{R.Mod(R.Sub(x.r, y.r), x.d), x.d};
  END Sub;

PROCEDURE Neg (READONLY x: T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    RETURN T{R.Mod(R.Neg(x.r), x.d), x.d};
  END Neg;

PROCEDURE IsZero (READONLY x: T; ): BOOLEAN =
  BEGIN
    RETURN R.IsZero(x.r);
  END IsZero;

PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN =
  BEGIN
    AssertEqualIdeal(x, y);
    RETURN R.Equal(x.r, y.r);
  END Equal;


PROCEDURE Mul (READONLY x, y: T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    AssertEqualIdeal(x, y);
    RETURN T{R.Mod(R.Mul(x.r, y.r), x.d), x.d};
  END Mul;

PROCEDURE Div (READONLY x, y: T; ): T RAISES {Arithmetic.Error} =
  VAR c: ARRAY [0 .. 1], [0 .. 1] OF R.T;
  BEGIN
    AssertEqualIdeal(x, y);

    GCD.Bezout(x.r, x.d, y.r, c);
    RETURN T{c[0, 0], x.d};
  END Div;

PROCEDURE Rec (READONLY x: T; ): T RAISES {Arithmetic.Error} =
  VAR c: ARRAY [0 .. 1], [0 .. 1] OF R.T;
  BEGIN
    GCD.Bezout(x.r, x.d, R.One, c);
    RETURN T{c[0, 0], x.d};
  END Rec;


PROCEDURE Square (READONLY x: T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    RETURN T{R.Mod(R.Mul(x.r, x.r), x.d), x.d};
  END Square;

PROCEDURE Scale (READONLY x: T; y: R.T; ): T =
  <* FATAL Arithmetic.Error *>   (* d must not be zero *)
  BEGIN
    RETURN T{R.Mod(R.Mul(x.r, y), x.d), x.d};
  END Scale;


BEGIN
END ResidueClassBasic.
