GENERIC MODULE PolynomialBasic(R, VR);
(*Copyright (c) 1995, Harry George*)
FROM NADefinitions IMPORT Error, Err;

CONST Module = "PolynomialBasic.";

(*--------------------*)
PROCEDURE Strip (x: T): T =
  VAR n := Size(x);
  BEGIN
    IF n = NUMBER(x^) THEN
      RETURN x;
    ELSE
      VAR y := NEW(T, n);
      BEGIN
        y^ := SUBARRAY(x^, 0, n);
        RETURN y;
      END;
    END;
  END Strip;

(*--------------------*)
PROCEDURE Size (x: T): CARDINAL =
  BEGIN
    FOR n := LAST(x^) TO FIRST(x^) BY -1 DO
      IF NOT R.IsZero(x[n]) THEN RETURN n + 1; END;
    END;
    RETURN 0;
  END Size;


(*-----------------*)
PROCEDURE Add (x, y: T): T =
  VAR
    xl := LAST(x^);
    yl := LAST(y^);
    zn := MAX(NUMBER(x^), NUMBER(y^));
    z  := NEW(T, zn);
  BEGIN
    IF xl >= yl THEN
      FOR i := 0 TO yl DO z[i] := R.Add(x[i], y[i]); END;
      FOR i := yl + 1 TO xl DO z[i] := x[i]; END;
    ELSE
      FOR i := 0 TO xl DO z[i] := R.Add(x[i], y[i]); END;
      FOR i := xl + 1 TO yl DO z[i] := y[i]; END;
    END;
    RETURN Strip(z);
  END Add;
(*-----------------*)
PROCEDURE Sub (x, y: T): T =
  VAR
    xl := LAST(x^);
    yl := LAST(y^);
    zn := MAX(NUMBER(x^), NUMBER(y^));
    z  := NEW(T, zn);
  BEGIN
    IF xl >= yl THEN
      FOR i := 0 TO yl DO z[i] := R.Sub(x[i], y[i]); END;
      FOR i := yl + 1 TO xl DO z[i] := x[i]; END;
    ELSE
      FOR i := 0 TO xl DO z[i] := R.Sub(x[i], y[i]); END;
      FOR i := xl + 1 TO yl DO z[i] := R.Neg(y[i]); END;
    END;
    RETURN Strip(z);
  END Sub;

(*---------------------*)
PROCEDURE Compare ( <*UNUSED*>x, y: T): [-1 .. 1] =
  BEGIN
    <*ASSERT FALSE*>
  END Compare;

(*---------------------*)
PROCEDURE IsZero (x: T): BOOLEAN =
  BEGIN
    RETURN x = NIL OR NUMBER(x^) = 0 OR R.IsZero(x[0]);
  END IsZero;

(*---------------------*)
PROCEDURE Equal (x, y: T): BOOLEAN =
  VAR
    xl := LAST(x^);
    yl := LAST(y^);
  BEGIN
    IF xl >= yl THEN
      FOR i := 0 TO yl DO
        IF NOT R.Equal(x[i], y[i]) THEN RETURN FALSE END
      END;
      FOR i := yl + 1 TO xl DO
        IF NOT R.Equal(x[i], R.Zero) THEN RETURN FALSE END
      END;
    ELSE
      FOR i := 0 TO xl DO
        IF NOT R.Equal(x[i], y[i]) THEN RETURN FALSE END
      END;
      FOR i := xl + 1 TO yl DO
        IF NOT R.Equal(R.Zero, y[i]) THEN RETURN FALSE END
      END;
    END;
    RETURN TRUE;
  END Equal;

(*---------------------*)
PROCEDURE Mul (x, y: T): T =
  VAR
    xnum := Size(x);
    ynum := Size(y);
    z    := NEW(T, xnum + ynum - 1);
  BEGIN
    VR.Clear(z^);

    FOR i := 0 TO xnum - 1 DO
      WITH zdata = SUBARRAY(z^, i, NUMBER(y^)) DO
        FOR j := 0 TO ynum - 1 DO
          zdata[j] := R.Add(zdata[j], R.Mul(x[i], y[j]));
        END;
      END;
    END;
    RETURN z;
  END Mul;

(*---------------------*)
PROCEDURE Div (x, y: T): T RAISES {Error} =
  <*UNUSED*>
  CONST ftn = Module & "Div";
  VAR
    xn                            := NUMBER(x^);
    xl                            := LAST(x^);
    yn                            := NUMBER(y^);
    y0                            := FIRST(y^);
    yl                            := LAST(y^);
    q, r               : T;
    qtmp, ymax         : R.T;
    qn, q0, ql, qi, ri2: CARDINAL;
  BEGIN
    (*---Copy numerator into r---*)
    r := NEW(T, xn);
    r^ := x^;

    (*---check for quick exit---*)
    IF xl < yl THEN
      (*can't do any Divides at all*)
      q := NEW(T, 1);
      q[0] := R.Zero;
      RETURN q;
    END;

    (*---setup quotient---*)
    qn := xn - yn + 1;
    q := NEW(T, qn);
    q0 := FIRST(q^);
    ql := LAST(q^);

    (*---find the dominant denominator term---*)
    ymax := y[yl];


    (*---compute---*)
    qi := ql + 1;
    FOR ri := xl TO (xl - ql) BY -1 DO
      DEC(qi);
      qtmp := R.Div(r[ri], ymax);
      q[qi] := qtmp;
      ri2 := ri;
      r[ri2] := R.Zero;          (*subtraction of values that should be
                                    equal does not work for floating point
                                    numbers*)
      FOR yi := yl - 1 TO y0 BY -1 DO
        DEC(ri2);
        r[ri2] := R.Sub(r[ri2], R.Mul(qtmp, y[yi]));
      END;
    END;
    (*This check will probably fail on floating point numbers*)
    FOR ri := (xl - ql) - 1 TO 0 BY -1 DO
      IF NOT R.Equal(r[ri], R.Zero) THEN RAISE Error(Err.indivisible); END;
    END;
    RETURN q;
  END Div;

(*---------------------*)
PROCEDURE DivMod (x, y: T): QuotRem RAISES {Error} =
  <*UNUSED*>
  CONST ftn = Module & "DivMod";
  VAR
    xn                             := NUMBER(x^);
    xl                             := LAST(x^);
    yn                             := NUMBER(y^);
    y0                             := FIRST(y^);
    yl                             := LAST(y^);
    q, r               : T;
    qtmp               : R.QuotRem;
    ymax               : R.T;
    qn, q0, ql, qi, ri2: CARDINAL;
  BEGIN
    (*---Copy numerator into r---*)
    r := NEW(T, xn);
    r^ := x^;

    (*---check for quick exit---*)
    IF xl < yl THEN
      (*can't do any Divides at all*)
      q := NEW(T, 1);
      q[0] := R.Zero;
      RETURN QuotRem{q, r};
    END;

    (*---setup quotient---*)
    qn := xn - yn + 1;
    q := NEW(T, qn);
    q0 := FIRST(q^);
    ql := LAST(q^);

    (*---find the dominant denominator term---*)
    ymax := y[yl];

    (*---compute---*)
    qi := ql + 1;
    FOR ri := xl TO (xl - ql) BY -1 DO
      DEC(qi);
      qtmp := R.DivMod(r[ri], ymax);
      q[qi] := qtmp.quot;
      r[ri] := qtmp.rem;
      ri2 := ri;
      FOR yi := yl - 1 TO y0 BY -1 DO
        DEC(ri2);
        r[ri2] := R.Sub(r[ri2], R.Mul(qtmp.quot, y[yi]));
      END;
    END;
    RETURN QuotRem{Strip(q), Strip(r)};
  END DivMod;

(*--------------------*)
PROCEDURE Mod (x, y: T): T RAISES {Error} =
  (*Using DivMod is not optimal.  One may save a bit space for the
     quotient.*)
  BEGIN
    RETURN DivMod(x, y).rem;
  END Mod;


(*--------------------*)
(*Horner's scheme*)
PROCEDURE Eval (x: T; xi: R.T): R.T =
  VAR
    l := LAST(x^);
    y := x[l];
  BEGIN
    FOR i := l - 1 TO 0 BY -1 DO y := R.Add(x[i], R.Mul(xi, y)); END;
    RETURN y;
  END Eval;

(*---------------------*)
PROCEDURE Derive (x: T;          (*differentiate polynomial*)
  ): T =
  VAR
    y   := NEW(T, LAST(x^));
    fac := R.Zero;
  BEGIN
    FOR n := 0 TO LAST(y^) DO
      fac := R.Add(fac, R.One);
      y[n] := R.Mul(x[n + 1], fac);
    END;
    RETURN y;
  END Derive;

(*---------------------*)
PROCEDURE EvalDerivative (x: T; xi: R.T; n: CARDINAL; ): REF ARRAY OF R.T =
  (*Given a poly with coefs x, find the value at xi as pd[0], and nd more
     EvalDerivativeatives as pd[1]..pd[pdl]. *)
  VAR
    xf             := FIRST(x^);
    xl             := LAST(x^);
    pd             := NEW(REF ARRAY OF R.T, n);
    pdl            := LAST(pd^);
    fact, fac: R.T;
  BEGIN
    (*---initialize f(xi) and clear f'(xi), f"(xi)...---*)
    pd[0] := x[xl];
    FOR i := 1 TO pdl DO pd[i] := R.Zero; END;

    (*---collect the raw values---*)
    FOR i := xl - 1 TO xf BY -1 DO
      FOR j := pdl TO 1 BY -1 DO
        pd[j] := R.Add(pd[j - 1], R.Mul(xi, pd[j]));
      END;
      pd[0] := R.Add(x[i], R.Mul(xi, pd[0]));
    END;

    (*---fix the factorials---*)
    fact := R.One;
    fac := R.Zero;
    FOR i := 0 TO pdl DO
      pd[i] := R.Mul(pd[i], fact);
      fac := R.Add(fac, R.One);
      fact := R.Mul(fact, fac);
    END;

    RETURN pd;
  END EvalDerivative;

(*--------------------*)
(*Horner's scheme with polynomial as argument*)
PROCEDURE Compose (x, y: T;      (*y(x) - apply y on the values of x*)
  ): T =
  (*VAR z := V.FromScalar(y[LAST(y^)]);*)
  VAR z := NEW(T, 1);
  BEGIN
    z[0] := y[LAST(y^)];
    FOR i := LAST(y^) - 1 TO 0 BY -1 DO
      z := Mul(x, z);
      z[0] := R.Add(z[0], y[i]);
    END;
    RETURN z;
  END Compose;

(*==========================*)
BEGIN
END PolynomialBasic.
