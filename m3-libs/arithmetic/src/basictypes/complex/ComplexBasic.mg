GENERIC MODULE ComplexBasic(R);
(*Copyright (c) 1996, m3na project

   Abstract: Complex numbers and basic operations *)

FROM NADefinitions IMPORT Error;

<*UNUSED*>
CONST Module = "ComplexBasic.";
(*==========================*)

(*--------------*)
PROCEDURE Add (READONLY x, y: T): T =
  VAR z: T;
  BEGIN
    z.re := R.Add(x.re, y.re);
    z.im := R.Add(x.im, y.im);
    RETURN z;
  END Add;
(*--------------*)
PROCEDURE Sub (READONLY x, y: T): T =
  VAR z: T;
  BEGIN
    z.re := R.Sub(x.re, y.re);
    z.im := R.Sub(x.im, y.im);
    RETURN z;
  END Sub;

(*-------------------*)
PROCEDURE Neg (READONLY x: T): T =
  VAR z: T;
  BEGIN
    z.re := R.Neg(x.re);
    z.im := R.Neg(x.im);
    RETURN z;
  END Neg;

(*----------------*)
PROCEDURE Conj (READONLY x: T): T =
  VAR z: T;
  BEGIN
    z.re := x.re;
    z.im := R.Neg(x.im);
    RETURN z;
  END Conj;

(*----------------*)
PROCEDURE IsZero (READONLY x: T): BOOLEAN =
  BEGIN
    RETURN R.IsZero(x.re) AND R.IsZero(x.im);
  END IsZero;

(*----------------*)
PROCEDURE Equal (READONLY x, y: T): BOOLEAN =
  BEGIN
    RETURN R.Equal(x.re, y.re) AND R.Equal(x.im, y.im);
  END Equal;


(*----------------*)
PROCEDURE Mul (READONLY x, y: T): T =
  VAR z: T;
  BEGIN
    z.re := R.Sub(R.Mul(x.re, y.re), R.Mul(x.im, y.im));
    z.im := R.Add(R.Mul(x.im, y.re), R.Mul(x.re, y.im));
    RETURN z;
  END Mul;

(*----------------*)
(*multiply x by the conjugate of y*)
PROCEDURE MulConj (READONLY x, y: T): T =
  VAR z: T;
  BEGIN
    z.re := R.Add(R.Mul(x.re, y.re), R.Mul(x.im, y.im));
    z.im := R.Sub(R.Mul(x.im, y.re), R.Mul(x.re, y.im));
    RETURN z;
  END MulConj;

(*----------------*)
(*
Use Karatsuba's trick
tend to produce truncation errors

xr yr - xi yi
xr yi + xi yr

(xr - xi) (yr + yi) - xr yi + xi yr
*)
<*UNUSED*>
PROCEDURE FastMul (READONLY x, y: T): T =
  VAR
    xyri    := R.Mul(R.Sub(x.re, x.im), R.Add(y.re, y.im));
    xryi    := R.Mul(x.re, y.im);
    xiyr    := R.Mul(x.im, y.re);
    z   : T;
  BEGIN
    z.re := R.Add(R.Sub(xyri, xryi), xiyr);
    z.im := R.Add(xryi, xiyr);
    RETURN z;
  END FastMul;

PROCEDURE DivScale (READONLY x, y: T; ): T RAISES {Error} =
  VAR denom := R.Add(R.Mul(y.re, y.re), R.Mul(y.im, y.im));
  BEGIN
    (* Err.divide_by_zero will be thrown by Div*)
    RETURN T{R.Div(x.re, denom), R.Div(x.im, denom)};
  END DivScale;

(*-------------------*)
PROCEDURE Div (READONLY x, y: T): T RAISES {Error} =
  BEGIN
    RETURN DivScale(Mul(x, Conj(y)), y);
  END Div;

(*-------------------*)
PROCEDURE Rec (READONLY x: T): T RAISES {Error} =
  BEGIN
    RETURN DivScale(Conj(x), x);
  END Rec;

(*-------------------*)
(*I have not found a method which a) is generic (works without comparisons)
   b) matches the needs of the Euclidean algorithm for determining the
   greatest common divisor (that is, there must be a magnitude indicator
   which is reduced by a modulo operation).  It work for some Gaussian
   numbers though, but not for all, e.g.  GCD(-3-3i,-1-4i) (the order is
   important, the opposite would work!).  The last example lead to a cycle:
   -3-3i,-1-4i,1-4i,3-3i,4-i,4+i,3+3i,...  In this case the Euclidean
   algorithm may be rewritten to process the pair (x*u,y) for several units
   u in parallel. *)
PROCEDURE Mod (READONLY x, y: T): T RAISES {Error} =
  VAR
    denom := R.Add(R.Mul(y.re, y.re), R.Mul(y.im, y.im));
    (* Err.divide_by_zero will be thrown by Mod*)
    xbig    := MulConj(x, y);
    r   : T;
  BEGIN
    r.re := R.Mod(xbig.re, denom);
    r.im := R.Mod(xbig.im, denom);
    r := Mul(r, y);              (*in fact, r is now AbsSqr(y) as big as
                                    before*)
    r.re := R.Div(r.re, denom);  (*is always divisible*)
    r.im := R.Div(r.im, denom);
    RETURN r;
  END Mod;

(*-------------------*)
PROCEDURE DivMod (READONLY x, y: T): QuotRem RAISES {Error} =
  VAR
    denom := R.Add(R.Mul(y.re, y.re), R.Mul(y.im, y.im));
    (* Err.divide_by_zero will be thrown by Div*)
    xbig := MulConj(x, y);
    re   := R.DivMod(xbig.re, denom);
    im   := R.DivMod(xbig.im, denom);
    r := Mul(T{re.rem, im.rem}, y); (*in fact, r is now AbsSqr(y) as big as
                                       before*)
  BEGIN
    r.re := R.Div(r.re, denom);  (*is always divisible*)
    r.im := R.Div(r.im, denom);
    <*ASSERT Equal(r,Mod(x,y))*>
    RETURN QuotRem{T{re.quot, im.quot}, r};
  END DivMod;


(*-------------------*)
PROCEDURE Square (READONLY x: T): T =
  VAR
    xri: R.T;
    z  : T;
  BEGIN
    z.re := R.Sub(R.Mul(x.re, x.re), R.Mul(x.im, x.im));
    xri := R.Mul(x.re, x.im);
    z.im := R.Add(xri, xri);     (*double*)
    RETURN z;
  END Square;

(*----------------*)
PROCEDURE Scale (READONLY x: T; y: R.T): T =
  VAR z: T;
  BEGIN
    z.re := R.Mul(x.re, y);
    z.im := R.Mul(x.im, y);
    RETURN z;
  END Scale;

(*==========================*)
BEGIN
  Zero := T{re := R.Zero, im := R.Zero};
  One := T{re := R.One, im := R.Zero};
  I := T{re := R.Zero, im := R.One};
  MinusOne := T{re := R.MinusOne, im := R.Zero};
END ComplexBasic.
