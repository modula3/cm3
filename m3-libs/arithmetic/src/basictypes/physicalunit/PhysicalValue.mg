GENERIC MODULE PhysicalValue(R);
(*Copyright (c) 1996, m3na project

*)

IMPORT PhysicalUnit AS U;

FROM NADefinitions IMPORT Error,Err;

<*UNUSED*> CONST Module = "PhysicalValue.";
(*==========================*)

(*--------------*)
PROCEDURE Add(READONLY x,y:T):T RAISES {Error}=
BEGIN
  (*if the value is zero, different units don't matter*)
  IF R.IsZero(y.val) THEN
    RETURN x;
  ELSIF R.IsZero(x.val) THEN
    RETURN y;
  ELSE
    IF NOT U.Equal(x.unit,y.unit) THEN
      RAISE Error(Err.unit_mismatch);
    END;
    RETURN T{R.Add(x.val,y.val),x.unit};
  END;
END Add;
(*--------------*)
PROCEDURE Sub(READONLY x,y:T):T RAISES {Error}=
BEGIN
  (*if the value is zero, different units don't matter*)
  IF R.IsZero(y.val) THEN
    RETURN x;
  ELSIF R.IsZero(x.val) THEN
    RETURN Neg(y);
  ELSE
    IF NOT U.Equal(x.unit,y.unit) THEN
      RAISE Error(Err.unit_mismatch);
    END;
    RETURN T{R.Sub(x.val,y.val),x.unit};
  END;
END Sub;

(*-------------------*)
PROCEDURE Neg(READONLY x : T) : T =
  BEGIN
    RETURN T{R.Neg(x.val),x.unit};
  END Neg;

(*----------------*)
PROCEDURE Conj(READONLY x:T):T=
  BEGIN
    RETURN T{R.Conj(x.val),x.unit};
  END Conj;

(*----------------*)
PROCEDURE IsZero(READONLY x:T):BOOLEAN =
  BEGIN
    RETURN R.IsZero(x.val);
  END IsZero;

(*----------------*)
PROCEDURE IsScalar(READONLY x:T):BOOLEAN =
  BEGIN
    RETURN U.IsZero(x.unit) OR R.IsZero(x.val);
  END IsScalar;

(*----------------*)
PROCEDURE Equal(READONLY x,y:T):BOOLEAN =
  BEGIN
    RETURN R.Equal(x.val,y.val) AND U.Equal(x.unit,y.unit);
  END Equal;


(*----------------*)
PROCEDURE Mul(READONLY x,y:T):T=
  BEGIN
    RETURN T{R.Mul(x.val,y.val),U.Add(x.unit,y.unit)};
  END Mul;

(*-------------------*)
PROCEDURE Div(READONLY x,y : T) : T RAISES {Error} =
  BEGIN
    RETURN T{R.Div(x.val,y.val),U.Sub(x.unit,y.unit)};
  END Div;

(*-------------------*)
PROCEDURE Rec(READONLY x : T) : T RAISES {Error} =
  BEGIN
    RETURN T{R.Rec(x.val),U.Neg(x.unit)};
  END Rec;

(*-------------------*)
PROCEDURE Mod(READONLY x,y:T):T RAISES {Error} =
  BEGIN
    RETURN T{R.Mod(x.val,y.val),x.unit};
  END Mod;

(*-------------------*)
PROCEDURE DivMod(READONLY x,y:T;VAR r:T):T RAISES {Error} =
  VAR
    q:T;
  BEGIN
    q.val:=R.DivMod(x.val,y.val,r.val);
    r.unit:=x.unit;
    q.unit:=U.Sub(x.unit,y.unit);
    RETURN q;
  END DivMod;


(*-------------------*)
PROCEDURE Square(READONLY x : T) : T =
  BEGIN
(*
    RETURN T{R.Square(x.val),U.Scale(x.unit,2)};
*)
    RETURN T{R.Mul(x.val,x.val),U.Scale(x.unit,2)};
  END Square;

(*----------------*)
PROCEDURE Scale(READONLY x:T; y:R.T):T=
  BEGIN
    RETURN T{R.Mul(x.val,y),x.unit};
  END Scale;

(*==========================*)
BEGIN
  Zero     := T{val:=R.Zero,     unit:=U.New()};
  One      := T{val:=R.One,      unit:=Zero.unit};
END PhysicalValue.
