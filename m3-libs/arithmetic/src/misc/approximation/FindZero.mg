GENERIC MODULE FindZero(R, RT);
(*Copyright (c) 1996, Harry George *)
FROM NADefinitions IMPORT Error, Err;

CONST Module = "FindZero.";
(*==========================*)

(*----------------*)
PROCEDURE BracketOut (func: Ftn;  (*find brackets for this function*)
                      VAR xb: Bracket;  (*starting with these points*)
                      maxiter: CARDINAL := 55 (*growing maxiter times*)
  ): BOOLEAN RAISES {Error} =    (*true if successful*)
  (*Given xb.l,xb.r, search for points (returned in xb.l, xb.r) for which
     func(xb.l) is opposite sign from func(xb.r).  Grow outward from the
     original xb.l,xb.r by golden ratio, for geometric growth.  Return true
     if a good xb.l,xb.r can be found before getting to maxiter, else
     return false.

     requires: xb.l<xb.r. *)
  <*UNUSED*>
  CONST ftn = Module & "BracketOut";
  VAR f1, f2, diff: R.T;
  BEGIN
    IF xb.r - xb.l < RT.Tiny THEN
      (*need xb.l<xb.r*)
      RAISE Error(Err.out_of_range);
    END;
    (*---initialize---*)
    f1 := func(xb.l);
    f2 := func(xb.r);
    (*---loop to completion---*)
    FOR i := 1 TO maxiter DO
      (*---check exit criteria---*)
      IF (f1 < R.Zero AND f2 > R.Zero) OR (f1 > R.Zero AND f2 < R.Zero) THEN
        RETURN TRUE;
      END;
      diff := xb.r - xb.l;
      (*---grow the smallest one---*)
      IF ABS(f1) < ABS(f2) THEN
        xb.l := xb.l - RT.GoldenRatio * diff; (*xb.l gets more negative*)
        f1 := func(xb.l);
      ELSE
        xb.r := xb.r + RT.GoldenRatio * diff; (*xb.r gets more positive*)
        f2 := func(xb.r);
      END;
    END;
    RETURN FALSE;
  END BracketOut;

(*----------------*)
PROCEDURE BracketIn (func: Ftn;  (*find brackets for this function*)
                     READONLY xb: Bracket;  (*starting with these points*)
                     n: CARDINAL;  (*using n equi-sized segments*)
                     VAR (*OUT*) xs: ARRAY OF Bracket;  (*returning pairs
                                                           here*)
  ): CARDINAL                    (*with this count of valid pairs, the rest
                                    of the array won't be touched*)
  RAISES {Error} =
  <*UNUSED*>
  CONST ftn = Module & "BracketIn";
  VAR
    h, x, xh, y, yh: R.T;
    ns             : CARDINAL := 0;
  BEGIN
    IF xb.l >= xb.r THEN
      (*need xb.l<xb.r*)
      RAISE Error(Err.out_of_range);
    END;
    IF NUMBER(xs) < 1 THEN
      (*need 1 <= ns <= size of xb1 and xb2*)
      RAISE Error(Err.out_of_range);
    END;

    h := (xb.r - xb.l) / FLOAT(n, R.T);
    x := xb.l;
    y := func(x);
    ns := 0;
    FOR i := 1 TO n DO
      xh := x + h;
      yh := func(xh);
      IF (y < R.Zero AND yh > R.Zero) OR (y > R.Zero AND yh < R.Zero) THEN
        xs[ns].l := x;
        xs[ns].r := xh;
        INC(ns);
        IF ns = NUMBER(xs) THEN RETURN ns; END;
      END;
      x := xh;
      y := yh;
    END;
    RETURN ns;
  END BracketIn;

(*----------------*)
PROCEDURE Bisection (func: Ftn;  (*find root of this function*)
                     READONLY xb : Bracket;  (*between these brackets*)
                              tol: R.T;      (*to within +/- tolerance*)
                     maxiter := 45 (*but no more than maxiter cuts*)
  ): R.T RAISES {Error} =        (*returning the root*)
  (*Given brackets xb.l,xb.r, find a root via bisection, and refine it to
     within +/- tol *)
  <*UNUSED*>
  CONST ftn = Module & "Bisection";
  VAR h, x, y1, y2, y: R.T;
  BEGIN
    (*---check preconditions---*)
    IF xb.r - xb.l < RT.Tiny THEN
      (*need xb.l<xb.r*)
      RAISE Error(Err.out_of_range);
    END;
    y1 := func(xb.l);
    y2 := func(xb.r);
    IF (y1 > R.Zero AND y2 > R.Zero) OR (y1 < R.Zero AND y2 < R.Zero) THEN
      (*xb.l and xb.r do not bracket a root*)
      RAISE Error(Err.not_bracketed);
    END;

    (*---initialize---*)
    IF y1 > R.Zero THEN
      x := xb.r;
      h := xb.l - xb.r;
    ELSE
      x := xb.l;
      h := xb.r - xb.l;
    END;
    h := h * RT.Half;
    x := x + h;

    (*---loop for maxiter or exit conditions---*)
    FOR i := 1 TO maxiter DO
      y := func(x);
      IF y = R.Zero THEN
        RETURN x;
      ELSIF y < R.Zero THEN
        x := x + h;
      ELSE
        x := x - h;
      END;
      IF ABS(h) < tol THEN RETURN x; END;
      h := h * RT.Half;
    END;
    RAISE Error(Err.not_converging);
  END Bisection;

(*----------------*)
PROCEDURE Brent (func: Ftn;      (*find a root of this function*)
                 READONLY xb : Bracket;  (*between these bracket points*)
                          tol: R.T;      (*to this tolerance*)
                 maxiter := 100  (*with <= maxiter iterations*)
  ): R.T RAISES {Error} =
  (*Use Brent's algorithm to find the real root between the bracket points.
     xb.l and xb.r must be of opposite signs. *)
  <*UNUSED*>
  CONST ftn = Module & "Brent";
  VAR
    a, b, c, fa, fb, fc, diffnext, diff2, delta: R.T;
    min1, min2, tolnext                        : R.T;
    r, s, t, p, q                              : R.T;
  BEGIN
    (*---check for quick victory---*)
    a := xb.l;
    fa := func(a);
    IF ABS(fa) < RT.Tiny THEN RETURN a; END;
    b := xb.r;
    fb := func(b);
    IF ABS(fb) < RT.Tiny THEN RETURN b; END;

    (*---check for bad bracketing---*)
    IF fa * fb > R.Zero THEN
      (*xb.l and xb.r do not bracket root*)
      RAISE Error(Err.not_bracketed);
    END;

    (*---set c at a---*)
    c := xb.l;
    fc := fa;

    (*---loop---*)
    FOR i := 1 TO maxiter DO
      (*---establish preconditions for loop---*)
      (*1.  a and c are same side, with b opposite*)
      IF (fb < R.Zero AND fc < R.Zero) OR (fb > R.Zero AND fc > R.Zero) THEN
        c := a;
        fc := fa;
      END;
      (*2.  fb is smallest of the three*)
      IF ABS(fc) < ABS(fb) THEN
        (*use the smallest one for b*)
        (*and keep c with a*)
        a := b;
        fa := fb;
        b := c;
        fb := fc;
        c := a;
        fc := fa;
      END;

      (*---check for convergence---*)
      (*1.  check for quick victory*)
      IF ABS(fb) < RT.Tiny THEN RETURN b; END;
      (*2.  get estimate of length if we go again*)
      diffnext := RT.Half * (c - b);
      diff2 := R.Two * diffnext;
      (*3.  get practical tolerance*)
      (*the idea is to do worst case machine tol or requested tol*)
      (*where one typically swamps the other*)
      tolnext := RT.Eps * ABS(b) + RT.Half * tol;
      (*4.  check estimate for being too small*)
      IF ABS(diffnext) < tolnext THEN RETURN b; END;

      (*---ready for another attempt---*)
      IF ABS(a - b) >= tolnext AND ABS(fa) > ABS(fb) THEN
        (*---try for quadratic---*)
        (*1.  build p and q, using reduction if possible*)
        s := fb / fa;
        IF a = c THEN            (*reduces to linear*)
          p := diff2 * s;
          q := R.One - s;
        ELSE
          r := fb / fc;
          t := fa / fc;
          p := s * (t * (r - t) * diff2 - (R.One - r) * (b - a));
          q := (t - R.One) * (r - R.One) * (s - R.One);
        END;
        (*2.  need p < q *)
        min1 := FLOAT(1.5D0, R.T) * diff2 * q - ABS(tolnext * q);
        min2 := ABS(diff2 * q);
        IF p < MIN(min1, min2) THEN
          (*ok to interpolate*)
          delta := p / q;
        ELSE
          (*use bisection*)
          delta := diffnext;
        END;
      ELSE                       (*bad candidate for quadratic, need
                                    bisection*)
        delta := diffnext;
      END;

      (*---have diff, so use it---*)
      (*1.  save old b as new a*)
      a := b;
      fa := fb;
      (*2.  get new b*)
      b := b + delta;
      fb := func(b);
    END;
    RAISE Error(Err.not_converging);
  END Brent;

(*---------------------*)
PROCEDURE NewtonRaphson (func: DifFtn;  (*this ftn*)
                         READONLY xb: Bracket;  (*bracketed by these
                                                   points*)
                         xtol: R.T;  (*find root to this x precision*)
                         maxiter := 25 (*with no more than maxiter loops*)
  ): R.T RAISES {Error} =        (*returning root*)
  (*Given a function which returns both f(x) and df(x), and brackets xb.l
     and xb.r, find the root to xtol precision.  Works via newton-raphson
     and bisection.

     *)
  <*UNUSED*>
  CONST ftn = Module & "NewtonRaphson";
  VAR
    y                    : DerivativeArray2;
    delta, root, rootnext: R.T;
    a, b, tmp            : R.T;
  BEGIN
    IF xb.l >= xb.r THEN
      (*need xb.l<xb.r*)
      RAISE Error(Err.not_bracketed);
    END;

    VAR y1, y2: R.T;
    BEGIN
      y := func(xb.l);
      IF y[0] = R.Zero THEN RETURN xb.l; END;
      y1 := y[0];
      y := func(xb.r);
      IF y[0] = R.Zero THEN RETURN xb.r; END;
      y2 := y[0];
      IF (y1 < R.Zero AND y2 < R.Zero) OR (y1 > R.Zero AND y2 > R.Zero) THEN
        RAISE Error(Err.not_bracketed);
      END;

      (*---orient for fa<0, fb>0---*)
      IF y1 < R.Zero THEN
        a := xb.l;
        b := xb.r;
      ELSE
        a := xb.r;
        b := xb.l;
      END;
    END;

    (*---init and loop---*)
    root := a;
    y := func(root);
    FOR i := 1 TO maxiter DO
      IF y[0] = R.Zero THEN RETURN root; END;
      IF y[1] = R.Zero THEN y[1] := RT.Tiny; END;
      delta := y[0] / y[1];
      rootnext := root - delta;
      IF (a - rootnext) * (rootnext - b) >= R.Zero THEN
        (*in bounds and fast enough for newton-raphson*)
        root := rootnext;
      ELSE
        (*out of bounds, need bisect*)
        tmp := root;
        root := RT.Half * (a + b);
        delta := root - tmp;
      END;
      IF ABS(delta) < xtol THEN RETURN root; END;
      y := func(root);
      IF y[0] < R.Zero THEN a := root; ELSE b := root; END;
    END;
    RAISE Error(Err.not_converging);
  END NewtonRaphson;
(*==========================*)
BEGIN
END FindZero.
