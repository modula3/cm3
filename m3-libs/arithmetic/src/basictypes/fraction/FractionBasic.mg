GENERIC MODULE FractionBasic(R);
(*Copyright (c) 1996, m3na project
  
Abstract: Fraction numbers and basic operations

*)

FROM xUtils IMPORT Error,Err;

<*UNUSED*> CONST Module = "FractionBasic.";
(*==========================*)

(*----------------*)
PROCEDURE Cancel (READONLY x : T):T RAISES {Error} =
VAR
  gcd : R.T;
BEGIN
  gcd := R.GCD (x.n, x.d);
  RETURN T{R.Div(x.n,gcd),R.Div(x.d,gcd)};
END Cancel;

(*----------------*)
PROCEDURE Add(READONLY x,y:T):T  (*return x+y*) =
VAR
  gcd,
  xdc, ydc : R.T;
  z : T;
BEGIN
  TRY
    gcd := R.GCD (x.d, y.d);
    xdc := R.Div (x.d, gcd);
    ydc := R.Div (y.d, gcd);
    z.n := R.Add (R.Mul(x.n,ydc), R.Mul(y.n,xdc));
    z.d := R.Mul (xdc, y.d);  (*least common multiple*)
    RETURN Cancel(z);  (*final cancellation is necessary as the example 1/2+1/2 shows,
                         if the denominators are different it may be unnecessary*)
  EXCEPT
    | Error(err) => EVAL err; (*dummy*)
  END;
  RETURN Zero;  (*must never occur*)
END Add;

(*----------------*)
PROCEDURE Sub(READONLY x,y:T):T  (*return x-y*) =
VAR
  gcd,
  xdc, ydc : R.T;
  z : T;
BEGIN
  TRY
    gcd := R.GCD (x.d, y.d);
    xdc := R.Div (x.d, gcd);
    ydc := R.Div (y.d, gcd);
    z.n := R.Sub (R.Mul(x.n,ydc), R.Mul(y.n,xdc));
    z.d := R.Mul (xdc, y.d);  (*least common multiple*)
    RETURN Cancel(z);
  EXCEPT
    | Error(err) => EVAL err; (*dummy*)
  END;
  RETURN Zero;  (*must never occur*)
END Sub;

(*----------------*)
PROCEDURE Neg(READONLY x:T):T    (*return -x *) =
BEGIN
  RETURN T{R.Neg(x.n),x.d};
END Neg;

(*----------------*)
PROCEDURE Conj(READONLY x:T):T   (*return complex conjugate of x*) =
BEGIN
  RETURN x;
END Conj;

(*----------------*)
PROCEDURE Equal(READONLY x,y:T):BOOLEAN  (*return x=y*) =
BEGIN
  (*comparing component-wise may fail if the field has more than one unit (say e.g. -1),
    in this ccase the fraction representation is not unique!*)
  RETURN R.Equal(R.Mul(x.n,y.d),R.Mul(y.n,x.d));
END Equal;

PROCEDURE Compare(READONLY x,y:T) : [-1..1] =
BEGIN
  RETURN R.Compare(R.Mul(x.n,y.d),R.Mul(y.n,x.d));
END Compare;


(*----------------*)
PROCEDURE Mul(READONLY x,y:T):T  (*return x*y*) =
VAR
  gcd : R.T;
  z   : T;
BEGIN
  TRY
    gcd := R.GCD(x.n, y.d);
    z.n := R.Div(x.n, gcd);
    z.d := R.Div(y.d, gcd);

    gcd := R.GCD(y.n, x.d);
    z.n := R.Mul(z.n, R.Div(y.n, gcd));
    z.d := R.Mul(z.d, R.Div(x.d, gcd));
  EXCEPT
    | Error(err) => EVAL err; (*dummy*)
  END;
  RETURN z;
END Mul;

(*----------------*)
PROCEDURE Div(READONLY x,y:T):T RAISES {Error}  (*return x/y*) =
VAR
  gcd : R.T;
  z   : T;
BEGIN
  gcd := R.GCD(x.n, y.n);
  z.n := R.Div(x.n, gcd);
  z.d := R.Div(y.n, gcd);

  gcd := R.GCD(y.d, x.d);
  z.n := R.Mul(z.n, R.Div(y.d, gcd));
  z.d := R.Mul(z.d, R.Div(x.d, gcd));

  RETURN z;
END Div;

(*----------------*)
PROCEDURE Rec(READONLY x:T):T RAISES {Error}    (*return 1/x*) =
BEGIN
  IF R.Equal(x.n,R.Zero) THEN
    RAISE Error(Err.divide_by_zero);
  END;
  RETURN T{x.d,x.n};
END Rec;

(*----------------*)
PROCEDURE Mod(<*UNUSED*> READONLY x:T; READONLY y:T):T RAISES {Error}  (*return x mod y*) =
BEGIN
  IF R.Equal(y.n,R.Zero) THEN
    RAISE Error(Err.divide_by_zero);
  END;
  RETURN Zero;
END Mod;

(*----------------*)
PROCEDURE DivMod(READONLY x,y:T;VAR r:T):T RAISES {Error}  (*return x/y and write the remainder (0) in r*) =
BEGIN
  r:=Zero;
  RETURN Div(x,y);
END DivMod;

(*----------------*)
PROCEDURE IntMod(READONLY x,y:T):T RAISES {Error}  (*return x mod y*) =
VAR
  gcd,
  xdc, ydc : R.T;
  z : T;
BEGIN
  gcd := R.GCD (x.d, y.d);
  xdc := R.Div (x.d, gcd);
  ydc := R.Div (y.d, gcd);
  z.n := R.Mod (R.Mul(x.n,ydc), R.Mul(y.n,xdc));
  z.d := R.Mul (xdc, y.d);  (*least common multiple*)
  RETURN Cancel(z);
END IntMod;


(*----------------*)
PROCEDURE Square(READONLY x:T):T         (*return x*x*) =
BEGIN
  RETURN T{R.Mul(x.n,x.n),R.Mul(x.d,x.d)};
  (*RETURN T{R.Square(x.n),R.Square(x.d)};*)
END Square;

(*----------------*)
PROCEDURE Scale (READONLY x:T; y:R.T):T  (*return x*y*) =
BEGIN
  RETURN T{R.Mul(x.n,y),x.d};
  (*RETURN T{R.Scale(x.n,y),x.d};*)
END Scale;



(*==========================*)
BEGIN
END FractionBasic.
