GENERIC MODULE ComplexFast(R);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Complex numbers and basic operations *)

IMPORT FloatMode;
IMPORT Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "ComplexFast.";

(*
  All routines with direct access to infix operators.
  May be useful as long as the compiler cannot handle INLINE procedures.
*)


PROCEDURE FromInteger (x: INTEGER; ): T =
  BEGIN
    RETURN T{FLOAT(x, R.T), R.Zero};
  END FromInteger;


PROCEDURE Add (READONLY x, y: T; ): T =
  VAR z: T;
  BEGIN
    z.re := x.re + y.re;
    z.im := x.im + y.im;
    RETURN z;
  END Add;


PROCEDURE Sub (READONLY x, y: T; ): T =
  VAR z: T;
  BEGIN
    z.re := x.re - y.re;
    z.im := x.im - y.im;
    RETURN z;
  END Sub;


PROCEDURE Neg (READONLY x: T; ): T =
  VAR z: T;
  BEGIN
    z.re := -x.re;
    z.im := -x.im;
    RETURN z;
  END Neg;


PROCEDURE Conj (READONLY x: T; ): T =
  VAR z: T;
  BEGIN
    z.re := x.re;
    z.im := -x.im;
    RETURN z;
  END Conj;


PROCEDURE IsZero (READONLY x: T; ): BOOLEAN =
  BEGIN
    RETURN x.re = R.Zero AND x.im = R.Zero;
  END IsZero;


PROCEDURE Equal (READONLY x, y: T; ): BOOLEAN =
  BEGIN
    RETURN x.re = y.re AND x.im = y.im;
  END Equal;



PROCEDURE Mul (READONLY x, y: T; ): T =
  VAR z: T;
  BEGIN
    z.re := x.re * y.re - x.im * y.im;
    z.im := x.im * y.re + x.re * y.im;
    RETURN z;
  END Mul;


PROCEDURE Div (READONLY x0, y0: T; ): T RAISES {Arith.Error} =
  VAR
    x        := Normalize(x0);
    y        := Scalb(y0, -x.exp);
    denom    := y.re * y.re + y.im * y.im;
    z    : T;
  <* FATAL FloatMode.Trap *>
  BEGIN
    (*avoid overflow and underflow by conditioning*)
    IF denom = R.Zero THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    END;
    z.re := (x.val.re * y.re + x.val.im * y.im) / denom;
    z.im := (-x.val.re * y.im + x.val.im * y.re) / denom;
    RETURN z;
  END Div;


PROCEDURE Rec (READONLY x0: T; ): T RAISES {Arith.Error} =
  VAR
    x        := Normalize(x0);
    denom    := x.val.re * x.val.re + x.val.im * x.val.im;
    z    : T;
  <* FATAL FloatMode.Trap *>
  BEGIN
    IF denom = R.Zero THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    END;
    z.re := x.val.re / denom;
    z.im := -x.val.im / denom;
    RETURN Scalb(z, -x.exp);
  END Rec;


PROCEDURE Mod (<* UNUSED *> READONLY x: T; READONLY y: T; ): T
  RAISES {Arith.Error} =
  BEGIN
    IF y.re = R.Zero AND y.im = R.Zero THEN
      RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
    END;
    RETURN Zero;
  END Mod;


PROCEDURE DivMod (READONLY x, y: T; ): QuotRem RAISES {Arith.Error} =
  BEGIN
    RETURN QuotRem{Div(x, y), Zero};
  END DivMod;


PROCEDURE Square (READONLY x: T; ): T =
  VAR z: T;
  BEGIN
    z.re := x.re * x.re - x.im * x.im;
    z.im := x.im * x.re * R.Two;
    RETURN z;
  END Square;


PROCEDURE Scale (READONLY x: T; y: R.T; ): T =
  VAR z: T;
  BEGIN
    z.re := y * x.re;
    z.im := y * x.im;
    RETURN z;
  END Scale;


<* UNUSED *>
PROCEDURE ScaleInt (x: T; y: INTEGER; ): T =
  VAR
    yr    := FLOAT(y, R.T);
    z : T;
  BEGIN
    z.re := x.re * yr;
    z.im := x.im * yr;
    RETURN z;
  END ScaleInt;




PROCEDURE Normalize (READONLY x: T; ): TExp =
  <* FATAL FloatMode.Trap *>
  BEGIN
    IF NOT IsZero(x) THEN
      VAR exp := ILogb(x);
      BEGIN
        RETURN TExp{Scalb(x, -exp), exp};
      END;
    ELSE
      (* ILogb(0)=-Infinity *)
      RETURN TExp{x, 0};
    END;
  END Normalize;

(*
PROCEDURE Normalize (READONLY x: T; VAR exp:INTEGER;): T =
  BEGIN
    exp:=0;
    IF x.re#R.Zero THEN exp:=exp+R.ILogb(x.re) END;
    IF x.im#R.Zero THEN exp:=exp+R.ILogb(x.im) END;
    exp := exp DIV 2;
    (*exp := ILogb (x);   ILogb(0)=-Infinity *)
    RETURN Scalb(x,-exp);
  END Normalize;
*)

PROCEDURE ILogb (READONLY x: T; ): INTEGER =
  BEGIN
    RETURN R.ILogb(ABS(x.re) + ABS(x.im)) DIV 2;
  END ILogb;

PROCEDURE Scalb (READONLY x: T; exp: INTEGER; ): T
  RAISES {FloatMode.Trap} =
  BEGIN
    RETURN T{R.Scalb(x.re, exp), R.Scalb(x.im, exp)};
  END Scalb;


BEGIN
END ComplexFast.
