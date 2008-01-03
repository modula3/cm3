GENERIC MODULE FindZero(R, RT);
(* Arithmetic for Modula-3, see doc for details *)
IMPORT Arithmetic AS Arith;

CONST Module = "FindZero.";


PROCEDURE AreBracketing (y1, y2: R.T; ): BOOLEAN =
  BEGIN
    RETURN
      (y1 <= R.Zero AND y2 >= R.Zero) OR (y1 >= R.Zero AND y2 <= R.Zero);
  END AreBracketing;

<* INLINE *>
PROCEDURE AssertBracketing (y1, y2: R.T; ) =
  BEGIN
    <* ASSERT AreBracketing(y1, y2),
                "The given values have the same sign and thus may not include a zero." *>
  END AssertBracketing;

PROCEDURE AssertXOrder (READONLY xb: Bracket; ) =
  BEGIN
    <* ASSERT xb.l + RT.Tiny < xb.r *>
  END AssertXOrder;



PROCEDURE BracketOut
  (func: Ftn; VAR xb: Bracket; maxiter: CARDINAL := 55; ): BOOLEAN =

  <* UNUSED *>
  CONST
    ftn = Module & "BracketOut";

  BEGIN
    AssertXOrder(xb);

    VAR
      (*---initialize---*)
      f1 := func(xb.l);
      f2 := func(xb.r);
    BEGIN
      (*---loop to completion---*)
      FOR i := 1 TO maxiter DO
        (*---check exit criteria---*)
        IF AreBracketing(f1, f2) THEN RETURN TRUE; END;
        WITH diff = xb.r - xb.l DO
          (*---grow the smallest one---*)
          IF ABS(f1) < ABS(f2) THEN
            xb.l :=
              xb.l - RT.GoldenRatio * diff; (* xb.l gets more negative *)
            f1 := func(xb.l);
          ELSE
            xb.r :=
              xb.r + RT.GoldenRatio * diff; (* xb.r gets more positive *)
            f2 := func(xb.r);
          END;
        END;
      END;
    END;
    RETURN FALSE;
  END BracketOut;


PROCEDURE BracketIn
  (func: Ftn; READONLY xb: Bracket; n: [1 .. LAST(CARDINAL)]; ):
  REF ARRAY OF Bracket =
  <* UNUSED *>
  CONST
    ftn = Module & "BracketIn";
  VAR
    h, x, xh, y, yh: R.T;
    ns             : CARDINAL := 0;
    xs                        := NEW(REF ARRAY OF Bracket, n);

  BEGIN
    AssertXOrder(xb);

    h := (xb.r - xb.l) / FLOAT(n, R.T);
    x := xb.l;
    y := func(x);
    FOR i := 1 TO n DO
      xh := x + h;
      yh := func(xh);
      IF AreBracketing(y, yh) THEN
        xs[ns].l := x;
        xs[ns].r := xh;
        INC(ns);
      END;
      x := xh;
      y := yh;
    END;

    WITH xs0 = NEW(REF ARRAY OF Bracket, ns) DO
      xs0^ := SUBARRAY(xs^, 0, ns);
      RETURN xs0;
    END;
  END BracketIn;


PROCEDURE Bisection
  (func: Ftn; READONLY xb: Bracket; tol: R.T; maxiter := 45; ): R.T
  RAISES {Arith.Error} =

  <* UNUSED *>
  CONST
    ftn = Module & "Bisection";
  VAR h, x, y1, y2, y: R.T;
  BEGIN
    AssertXOrder(xb);

    y1 := func(xb.l);
    y2 := func(xb.r);

    AssertBracketing(y1, y2);

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
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END Bisection;

PROCEDURE Brent
  (func: Ftn; READONLY xb: Bracket; tol: R.T; maxiter := 100; ): R.T
  RAISES {Arith.Error} =
  <* UNUSED *>
  CONST
    ftn = Module & "Brent";
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

    AssertBracketing(fa, fb);

    (*---set c at a---*)
    c := xb.l;
    fc := fa;

    (*---loop---*)
    FOR i := 1 TO maxiter DO
      (*---establish preconditions for loop---*)
      (* 1.  a and c are same side, with b opposite *)
      IF NOT AreBracketing(fb, fc) THEN c := a; fc := fa; END;
      (* 2.  fb is smallest of the three *)
      IF ABS(fc) < ABS(fb) THEN
        (* use the smallest one for b *)
        (* and keep c with a *)
        a := b;
        fa := fb;
        b := c;
        fb := fc;
        c := a;
        fc := fa;
      END;

      (*---check for convergence---*)
      (* 1.  check for quick victory *)
      IF ABS(fb) < RT.Tiny THEN RETURN b; END;
      (* 2.  get estimate of length if we go again *)
      diffnext := RT.Half * (c - b);
      diff2 := R.Two * diffnext;
      (* 3.  get practical tolerance *)
      (* the idea is to do worst case machine tol or requested tol *)
      (* where one typically swamps the other *)
      tolnext := RT.Eps * ABS(b) + RT.Half * tol;
      (* 4.  check estimate for being too small *)
      IF ABS(diffnext) < tolnext THEN RETURN b; END;

      (*---ready for another attempt---*)
      IF ABS(a - b) >= tolnext AND ABS(fa) > ABS(fb) THEN
        (*---try for quadratic---*)
        (* 1.  build p and q, using reduction if possible *)
        s := fb / fa;
        IF a = c THEN            (* reduces to linear *)
          p := diff2 * s;
          q := R.One - s;
        ELSE
          r := fb / fc;
          t := fa / fc;
          p := s * (t * (r - t) * diff2 - (R.One - r) * (b - a));
          q := (t - R.One) * (r - R.One) * (s - R.One);
        END;
        (* 2.  need p < q *)
        min1 := FLOAT(1.5D0, R.T) * diff2 * q - ABS(tolnext * q);
        min2 := ABS(diff2 * q);
        IF p < MIN(min1, min2) THEN
          (* ok to interpolate *)
          delta := p / q;
        ELSE
          (* use bisection *)
          delta := diffnext;
        END;
      ELSE                       (* bad candidate for quadratic, need
                                    bisection *)
        delta := diffnext;
      END;

      (*---have diff, so use it---*)
      (* 1.  save old b as new a *)
      a := b;
      fa := fb;
      (* 2.  get new b *)
      b := b + delta;
      fb := func(b);
    END;
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END Brent;


PROCEDURE NewtonRaphson
  (func: DifFtn; READONLY xb: Bracket; xtol: R.T; maxiter := 25; ): R.T
  RAISES {Arith.Error} =

  <* UNUSED *>
  CONST
    ftn = Module & "NewtonRaphson";
  VAR
    y                    : DerivativeArray2;
    delta, root, rootnext: R.T;
    a, b, tmp            : R.T;
  BEGIN
    AssertXOrder(xb);

    VAR y1, y2: R.T;
    BEGIN
      y := func(xb.l);
      IF y[0] = R.Zero THEN RETURN xb.l; END;
      y1 := y[0];
      y := func(xb.r);
      IF y[0] = R.Zero THEN RETURN xb.r; END;
      y2 := y[0];
      AssertBracketing(y1, y2);

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
        (* in bounds and fast enough for newton-raphson *)
        root := rootnext;
      ELSE
        (* out of bounds, need bisect *)
        tmp := root;
        root := RT.Half * (a + b);
        delta := root - tmp;
      END;
      IF ABS(delta) < xtol THEN RETURN root; END;
      y := func(root);
      IF y[0] < R.Zero THEN a := root; ELSE b := root; END;
    END;
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END NewtonRaphson;

BEGIN
END FindZero.
