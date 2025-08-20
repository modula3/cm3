MODULE PolInt;

PROCEDURE Interpolate(READONLY xa, ya : ARRAY OF LONGREAL;
                      x               : LONGREAL) : Result =
  VAR
    n    := NUMBER(xa);
    c, d := NEW(REF ARRAY OF LONGREAL, n);
    dif  := ABS(x - xa[0]);
    dy, y : LONGREAL;
    ns    : CARDINAL := 1; (* FORTRAN conventions *)
    den, dift, ho, hp, w : LONGREAL;
  BEGIN
    <*ASSERT NUMBER(xa) = NUMBER(ya)*>
    FOR i := 0 TO n - 1 DO (* C/Modula indexing *)
      dift := ABS(x - xa[i]);
      IF dift < dif THEN
        ns  := i + 1;
        dif := dift;
      END;
      c[i] := ya[i];
      d[i] := ya[i]
    END;

    y := ya[ns - 1];

    DEC(ns);

    FOR m := 1 TO n - 1 DO (* FORTRAN indexing *)
      FOR i := 1 TO n - m DO (* FORTRAN indexing *)
        ho  := xa[i - 1] - x;
        hp  := xa[i + m - 1] - x;
        w   := c[i + 1 - 1] - d[i - 1];
        den := ho - hp;
        <*ASSERT den # 0.0d0*>
        den := w / den;
        d[i - 1] := hp * den;
        c[i - 1] := ho * den;
      END;
      IF 2 * ns < n - m (* FORTRAN *) THEN
        dy := c[ns + 1 - 1];
      ELSE
        dy := d[ns - 1];
        DEC(ns)
      END;
      y := y + dy;
    END;
    RETURN Result { y, dy }
  END Interpolate;

BEGIN END PolInt.
