MODULE SpecialFunction;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Special Functions

   2/3/96 Harry George Initial version

   2/17/96 Harry George Export to xReal64 *)

IMPORT Arithmetic AS Arith;
FROM LongRealTrans IMPORT Zero, Half, One, Two, Eps, Tiny, Exp, Ln;

CONST Module = "SpecialFunction.";


PROCEDURE Factorial (n: CARDINAL; ): T =
  CONST
    max = 34;
    cache = ARRAY [0 .. max] OF
              T{1.0D0, 1.0D0, 2.0D0, 6.0D0, 24.0D0, 120.0D0, (*0-5*)
                720.0D0, 5040.0D0, 40320.0D0, 362880.0D0, (*6-9*)
                3628800.0D0, 39916800.0D0, 479001600.0D0,
                6227020800.0D0,  (*10-13*)
                87178291200.0D0, 1307674368000.0D0,
                20922789888000.0D0, (*14-16*)
                355687428096000.0D0, 6.402373705728D+15,
                1.21645100408832D+17, (*17-19*)
                2.43290200817664D+18, 5.109094217170944D+19,
                1.124000727777608D+21, (*20-22*)
                2.585201673888498D+22, 6.204484017332394D+23,
                1.551121004333099D+25, (*23-25*)
                4.032914611266057D+26, 1.088886945041835D+28,
                3.048883446117139D+29, (*26-28*)
                8.841761993739702D+30, 2.652528598121911D+32,
                2.652528598121911D+32, (*29-31*)
                2.631308369336935D+35, 8.683317618811886D+36,
                2.952327990396042D+38 (*32-34*)
              };
  BEGIN
    IF n <= max THEN
      RETURN cache[n];
    ELSE
      RETURN Exp(LnGamma(FLOAT(n, T) + One));
    END;
  END Factorial;


CONST max_factln = 70;
VAR factln_cache: ARRAY [2 .. max_factln] OF T;
PROCEDURE LnFactorial (n: CARDINAL; ): T =
  (* returns ln(n!) as a real*)
  VAR z: T;
  BEGIN
    IF n < 2 THEN
      RETURN Zero;
    ELSIF n > max_factln THEN
      RETURN LnGamma(FLOAT(n, T) + One);
    END;

    (*---check the cache---*)
    z := factln_cache[n];
    IF z = Zero THEN
      z := LnGamma(FLOAT(n, T) + One);
      factln_cache[n] := z;
    END;
    RETURN z;
  END LnFactorial;


PROCEDURE Gamma (x: T; ): T =
  (* returns Gamma(x))*)
  BEGIN
    RETURN Exp(LnGamma(x));
  END Gamma;


PROCEDURE LnGamma (x: T; ): T =
  (* returns ln(Gamma(x))*)
  TYPE coefs = ARRAY [0 .. N] OF T;
  CONST
    N         = 6;
    gam       = 5.0d0;
    lnsqrt2pi = 0.918938533204673d0;
    c = coefs{
          1.000000000190015d0, 76.18009172947146d0, -86.50532032941677d0,
          24.01409824083091d0, -1.231739572450155d0, 0.1208650973866179d-2,
          -0.5395239384953d-5};
  VAR
    x1              := FLOAT(x, T) - 1.0d0;
    y, z, series: T;
  BEGIN
    y := x1 + gam + 0.5d0;
    series := c[0] + c[1] / (x1 + 1.0d0) + c[2] / (x1 + 2.0d0)
                + c[3] / (x1 + 3.0d0) + c[4] / (x1 + 4.0d0)
                + c[5] / (x1 + 5.0d0) + c[6] / (x1 + 6.0d0);
    z := Ln(y) * (x1 + 0.5d0) + (-y) + lnsqrt2pi + Ln(series);
    RETURN z;
  END LnGamma;


PROCEDURE Binomial (n, k: CARDINAL; ): T RAISES {Arith.Error} =
  (* returns Binomial coefficient for "n over k"*)
  <* UNUSED *>
  CONST
    ftn = Module & "Binomial";
  VAR z: T;
  BEGIN
    IF k > n THEN
      (* n must be > k*)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    END;
    z := Exp(LnFactorial(n) - LnFactorial(k) - LnFactorial(n - k));
    RETURN z;
  END Binomial;


PROCEDURE GammaP (a, x: T; ): T RAISES {Arith.Error} =
  (* returns incomplete Gamma P(a,x)=Gamma(a,x)/Gamma(a)*)
  <* UNUSED *>
  CONST
    ftn = Module & "GammaP";
  VAR factor := Exp(-LnGamma(a) - x + a * Ln(x));
  BEGIN
    (*---check conditions---*)
    IF a < Zero OR x < Zero THEN
      (* must have a>0 and x>0*)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    END;
    IF x < (a + One) THEN
      RETURN factor * GamSer(a, x);
    ELSE
      RETURN One - factor * GamCF(a, x);
    END;
  END GammaP;


PROCEDURE GammaQ (a, x: T; ): T RAISES {Arith.Error} =
  (* returns incomplete Gamma Q(a,x)=Gamma(a,x)/Gamma(a)*)
  (* also, Q(a,x)=1-P(a,x) *)
  <* UNUSED *>
  CONST
    ftn = Module & "GammaQ";
  VAR factor := Exp(-LnGamma(a) - x + a * Ln(x));
  BEGIN
    (*---check conditions---*)
    IF a < Zero OR x < Zero THEN
      (* must have a>0 and x>0*)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    END;
    IF x < (a + One) THEN
      RETURN One - factor * GamSer(a, x);
    ELSE
      RETURN factor * GamCF(a, x);
    END;
  END GammaQ;


PROCEDURE GamSer (a, x: T; ): T RAISES {Arith.Error} =
  (* helper for GammaP and GammaQ*)
  (* generates Gamma(a,x)/Gamma(a) via series*)
  <* UNUSED *>
  CONST
    ftn = Module & "GamSer";
  CONST
    MaxIter = 90;
    eps     = 5.0D0 * Eps;
  VAR a1n, term, sum: T;
  BEGIN
    (*---initialize---*)
    term := One / a;
    sum := term;
    a1n := a;
    (*---iterate---*)
    FOR i := 1 TO MaxIter DO
      a1n := a1n + One;
      term := term * x / a1n;
      sum := sum + term;
      IF ABS(term) < eps THEN RETURN sum; END;
    END;
    (* if we got here, we are in trouble*)
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END GamSer;


PROCEDURE GamCF (a, x: T; ): T RAISES {Arith.Error} =
  (* helper for GammaP and GammaQ*)
  (* generates Gamma(a,x) via continued fractions*)
  <* UNUSED *>
  CONST
    ftn = Module & "GamCF";
  CONST
    MaxIter = 90;
    eps     = 5.0d0 * Eps;
  VAR f, D, C, m, xa, aj, bj, delta: T;
  BEGIN
    (*---initialize for j=1---*)
    C := x + One + a + One / Tiny;
    IF ABS(C) < Tiny THEN C := Tiny; END;
    xa := x + One - a;
    D := xa;
    IF ABS(D) < Tiny THEN D := Tiny; END;
    D := One / D;
    f := D;

    (*---iterate---*)
    m := Zero;
    FOR j := 1 TO MaxIter DO
      m := m + One;
      bj := xa + Two;
      aj := m * (a - m);
      D := bj + aj * D;
      IF ABS(D) < Tiny THEN D := Tiny; END;
      C := bj + aj / C;
      IF ABS(C) < Tiny THEN C := Tiny; END;
      D := One / D;
      delta := C * D;
      f := f * delta;
      IF ABS(delta - One) < eps THEN RETURN f; END;
      m := m + One;
      bj := m * (m - a);
      aj := aj + Two;
    END;
    (* if we got here, we had a problem*)
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());

  END GamCF;


PROCEDURE Erf (x: T; ): T RAISES {Arith.Error} =
  (* returns error function of x*)
  BEGIN
    IF x < Zero THEN
      RETURN -GammaP(Half, x * x);
    ELSE
      RETURN GammaP(Half, x * x);
    END;
  END Erf;


PROCEDURE ErfC (x: T; ): T RAISES {Arith.Error} =
  (* returns 1-Erf(x) *)
  BEGIN
    IF x < Zero THEN
      RETURN Two - GammaQ(Half, x * x);
    ELSE
      RETURN GammaQ(Half, x * x);
    END;
  END ErfC;


PROCEDURE Beta (x, y: T; ): T =
  (* returns Gamma(x)*Gamma(y)/Gamma(x+y*)
  BEGIN
    RETURN Exp(LnGamma(x) + LnGamma(y) - LnGamma(x + y));
  END Beta;


PROCEDURE BetaCF (a, b, x: T; ): T RAISES {Arith.Error} =
  (* helper for BetaI, returns continued fraction*)
  <* UNUSED *>
  CONST
    ftn = Module & "BetaCF";
  CONST
    bj      = One;
    MaxIter = 90;
    eps     = 5.0D0 * Eps;
  VAR f, D, C, m, am, a2m, aj, delta: T;
  BEGIN
    (*---initialize at j=2---*)
    aj := -(a + b) * x / (a + One);
    D := One + aj;
    C := One;
    IF ABS(D) < Tiny THEN D := Tiny; END;
    D := One / D;
    f := D;

    (*---iterate---*)
    m := Zero;
    FOR j := 3 TO MaxIter DO
      m := m + One;
      am := a + m;
      a2m := am + m;

      (*---a[j]=d[2m]---*)
      aj := m * (b - m) * x / ((a2m - One) * a2m);
      D := bj + aj * D;
      IF ABS(D) < Tiny THEN D := Tiny; END;
      C := bj + aj / C;
      IF ABS(C) < Tiny THEN C := Tiny; END;
      D := One / D;
      delta := C * D;
      f := f * delta;
      IF ABS(delta - One) < eps THEN RETURN f; END;

      (*---a[j]=d[2m+1]---*)
      aj := -am * (am + b) * x / (a2m * (a2m + One));
      D := bj + aj * D;
      IF ABS(D) < Tiny THEN D := Tiny; END;
      C := bj + aj / C;
      IF ABS(C) < Tiny THEN C := Tiny; END;
      D := One / D;
      delta := C * D;
      f := f * delta;
      IF ABS(delta - One) < eps THEN RETURN f; END;
    END;
    (* if we got here, we had a problem*)
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END BetaCF;


PROCEDURE BetaI (a, b, x: T; ): T RAISES {Arith.Error} =
  (* returns incomplete Beta Ix(a,b) *)
  <* UNUSED *>
  CONST
    ftn = Module & "BetaI";
  VAR factor: T;
  BEGIN
    IF a <= Zero OR b <= Zero THEN
      (* must have a>Zero and b>Zero*)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    ELSIF x < Zero OR x > One THEN
      (* must have Zero < x < One*)
      RAISE Arith.Error(NEW(Arith.ErrorOutOfRange).init());
    ELSIF x = Zero OR x = One THEN
      factor := Zero;
    ELSE
      factor := Exp(a * Ln(x) + b * Ln(One - x)
                      - (LnGamma(a) + LnGamma(b) - LnGamma(a + b)));
    END;

    (*---check for convergence condition---*)
    IF x < (a + One) / (a + b + Two) THEN
      RETURN factor * BetaCF(a, b, x) / a;
    ELSE
      RETURN One - factor * BetaCF(a, b, One - x) / b;
    END;
  END BetaI;


BEGIN
  FOR i := FIRST(factln_cache) TO LAST(factln_cache) DO
    factln_cache[i] := Zero;
  END;
END SpecialFunction.
