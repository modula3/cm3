GENERIC MODULE FractionBasic(R, GCD);
(*Copyright (c) 1996, m3na project

   Abstract: Fraction numbers and basic operations *)

FROM NADefinitions IMPORT Error, Err;

<*UNUSED*>
CONST Module = "FractionBasic.";
(*==========================*)

(*----------------*)
PROCEDURE Cancel (READONLY x: T): T RAISES {Error} =
  VAR gcd := GCD.GCD(x.n, x.d);
  BEGIN
    RETURN T{R.Div(x.n, gcd), R.Div(x.d, gcd)};
  END Cancel;

(*----------------*)
PROCEDURE Add (READONLY x, y: T): T (*return x+y*) =
  VAR
    gcd := GCD.GCD(x.d, y.d);
    xdc := R.Div(x.d, gcd);
    ydc := R.Div(y.d, gcd);
    z := T{n := R.Add(R.Mul(x.n, ydc), R.Mul(y.n, xdc)), d :=
           R.Mul(xdc, y.d) (*least common multiple*)};
  <*FATAL Error*>(*Division will always succeed*)
  BEGIN
    RETURN Cancel(z);            (*final cancellation is necessary as the
                                    example 1/2+1/2 shows, if the
                                    denominators are different it may be
                                    unnecessary*)
  END Add;

(*----------------*)
PROCEDURE Sub (READONLY x, y: T): T (*return x-y*) =
  VAR
    gcd := GCD.GCD(x.d, y.d);
    xdc := R.Div(x.d, gcd);
    ydc := R.Div(y.d, gcd);
    z := T{n := R.Sub(R.Mul(x.n, ydc), R.Mul(y.n, xdc)), d :=
           R.Mul(xdc, y.d) (*least common multiple*)};
  <*FATAL Error*>(*Division will always succeed*)
  BEGIN
    RETURN Cancel(z);
  END Sub;

(*----------------*)
PROCEDURE Neg (READONLY x: T): T (*return -x *) =
  BEGIN
    RETURN T{R.Neg(x.n), x.d};
  END Neg;

(*----------------*)
PROCEDURE Conj (READONLY x: T): T (*return complex conjugate of x*) =
  BEGIN
    RETURN x;
  END Conj;

(*----------------*)
PROCEDURE IsZero (READONLY x: T): BOOLEAN (*return x=0*) =
  BEGIN
    RETURN R.IsZero(x.n);
  END IsZero;

(*----------------*)
PROCEDURE Equal (READONLY x, y: T): BOOLEAN (*return x=y*) =
  BEGIN
    (*comparing component-wise may fail if the field has more than one unit
       (say e.g.  -1), in this case the fraction representation is not
       unique!*)
    RETURN R.Equal(R.Mul(x.n, y.d), R.Mul(y.n, x.d));
  END Equal;

(*----------------*)
PROCEDURE Compare (READONLY x, y: T): [-1 .. 1] =
  BEGIN
    RETURN R.Compare(R.Mul(x.n, y.d), R.Mul(y.n, x.d));
  END Compare;


(*----------------*)
PROCEDURE Mul (READONLY x, y: T): T (*return x*y*) =
  VAR
    gcd    := GCD.GCD(x.n, y.d);
    z  : T;
  <*FATAL Error*>(*Division will always succeed*)
  BEGIN
    z.n := R.Div(x.n, gcd);
    z.d := R.Div(y.d, gcd);

    gcd := GCD.GCD(y.n, x.d);
    z.n := R.Mul(z.n, R.Div(y.n, gcd));
    z.d := R.Mul(z.d, R.Div(x.d, gcd));
    RETURN z;
  END Mul;

(*----------------*)
PROCEDURE Div (READONLY x, y: T): T RAISES {Error} (*return x/y*) =
  VAR
    gcd    := GCD.GCD(x.n, y.n);
    z  : T;
  BEGIN
    z.n := R.Div(x.n, gcd);
    z.d := R.Div(y.n, gcd);

    gcd := GCD.GCD(y.d, x.d);
    z.n := R.Mul(z.n, R.Div(y.d, gcd));
    z.d := R.Mul(z.d, R.Div(x.d, gcd));

    RETURN z;
  END Div;

(*----------------*)
PROCEDURE Rec (READONLY x: T): T RAISES {Error} (*return 1/x*) =
  BEGIN
    IF R.IsZero(x.n) THEN RAISE Error(Err.divide_by_zero); END;
    RETURN T{x.d, x.n};
  END Rec;

(*----------------*)
PROCEDURE Mod ( <*UNUSED*>READONLY x: T; READONLY y: T): T
  RAISES {Error} (*return x mod y*) =
  BEGIN
    IF R.IsZero(y.n) THEN RAISE Error(Err.divide_by_zero); END;
    RETURN Zero;
  END Mod;

(*----------------*)
PROCEDURE DivMod (x, y: T): QuotRem
  RAISES {Error} (*return x/y and write the remainder (0) in r*) =
  BEGIN
    RETURN QuotRem{Div(x, y), Zero};
  END DivMod;

(*----------------*)
PROCEDURE IntMod (READONLY x, y: T): T RAISES {Error} (*return x mod y*) =
  VAR
    gcd := GCD.GCD(x.d, y.d);
    xdc := R.Div(x.d, gcd);
    ydc := R.Div(y.d, gcd);
    z := T{n := R.Mod(R.Mul(x.n, ydc), R.Mul(y.n, xdc)), d :=
           R.Mul(xdc, y.d) (*least common multiple*)};
  BEGIN
    RETURN Cancel(z);
  END IntMod;


(*----------------*)
PROCEDURE Square (READONLY x: T): T (*return x*x*) =
  BEGIN
    RETURN T{R.Mul(x.n, x.n), R.Mul(x.d, x.d)};
    (*RETURN T{R.Square(x.n),R.Square(x.d)};*)
  END Square;

(*----------------*)
PROCEDURE Scale (READONLY x: T; y: R.T): T (*return x*y*) =
  <*FATAL Error*>
  BEGIN
    RETURN Cancel(T{R.Mul(x.n, y), x.d});
    (*RETURN T{R.Scale(x.n,y),x.d};*)
  END Scale;



(*==========================*)
BEGIN
  Zero := T{n := R.Zero, d := R.One};
  One := T{n := R.One, d := R.One};
  (* MinusOne := T{n:=R.MinusOne, d:=R.One}; *)
END FractionBasic.
