(* $Id$ *)
MODULE Solve;
IMPORT Debug, LRFunction;

PROCEDURE WDB(f : LRFunction.T; 
              x1, x2 : LONGREAL; tol : LONGREAL) : LONGREAL =
  CONST
    ItMax = 200;
    EPS = 3.0d-8;
  VAR
    c, d, e, fc, p, q, r, s, tol1, xm : LONGREAL;
    a := x1;
    b := x2;
    fa, fb : LONGREAL; 
  BEGIN
    IF f.doHints THEN f.evalHint(b) END;

    fa := f.eval(a);
    fb := f.eval(b);
    IF fa > 0.0d0 AND fb > 0.0d0 OR fa < 0.0d0 AND fb < 0.0d0 THEN
      Debug.Error("Solve.WDB: van Wijngaarden-Dekker-Brent method requires bracketed input!")
    END;

    c := b; fc := fb;
    FOR iter := 1 TO ItMax DO
      IF fb > 0.0d0 AND fc > 0.0d0 OR fb < 0.0d0 AND fc < 0.0d0 THEN
        c := a; fc := fa; d := b-a; e := d
      END;
      IF ABS(fc) < ABS(fb) THEN
        a := b;    b := c;   c := a; 
        fa := fb; fb := fc; fc := fa
      END;
      tol1 := 2.0d0*EPS*ABS(b)+0.5d0*tol;
      xm := 0.5d0*(c-b);
      IF ABS(xm) <= tol1 OR fb = 0.0d0 THEN
        RETURN b
      END;
      IF ABS(e) >= tol1 AND ABS(fa) > ABS(fb) THEN
        s := fb/fa;
        IF a = c THEN
          p := 2.0d0 * xm * s; q := 1.0d0 - s
        ELSE
          q := fa/fc; r := fb/fc;
          p := s*(2.0d0*xm*q*(q-r)-(b-a)*(r-1.0d0));
          q := (q-1.0d0)*(r-1.0d0)*(s-1.0d0)
        END;
        IF p > 0.0d0 THEN q := -q END;
        p := ABS(p);
        IF 2.0d0 * p <= MIN(3.0d0*xm*q-ABS(tol1*q),ABS(e*q)) THEN
          e := d; d := p/q
        ELSE
          d := xm; e := d
        END
      ELSE
        d := xm; e := d
      END;
      a := b; fa := fb;
      IF ABS(d) > tol1 THEN
        b := b+d
      ELSE
        b := b + ABS(tol1) * ARRAY BOOLEAN OF LONGREAL 
                             { -1.0d0, 1.0d0 } [xm >= 0.0d0] 
      END;
      fb := f.eval(b)
    END;
    Debug.Error("Solve.WDB: van Wijngaarden-Dekker-Brent method exceeding max iterations!");
    RETURN b
  END WDB;

BEGIN END Solve.
