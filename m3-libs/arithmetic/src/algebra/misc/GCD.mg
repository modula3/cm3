GENERIC MODULE GCD(R);
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "GCD.";
(*==========================*)

(*----------------------*)
PROCEDURE GCD(u,v:T):T=
(*returns the greatest common denominator for u and v.*)
(*use Euclid's algorithm*)
VAR
  w:T;
BEGIN
  TRY
    WHILE NOT R.IsZero(u) DO
      w:=R.Mod(v,u);
      v:=u;
      u:=w;
    END;
  (*
    WHILE u#0 DO
      w:=v MOD u;
      v:=u;
      u:=w;
    END;
  *)
  EXCEPT
    Error(err) => <*ASSERT err#Err.divide_by_zero*>
  END;
  RETURN v;
END GCD;

PROCEDURE LCM(u,v:T):T =
BEGIN
  TRY
    RETURN R.Mul(R.Div(u,GCD(u,v)),v);
  EXCEPT
    Error(err) =>
      <*ASSERT err#Err.indivisible*>
      RETURN R.Zero;
  END;
END LCM;

(*
PROCEDURE BezoutGCD(u,v:T; VAR (*OUT*) x,y : T) =
BEGIN
  
END BezoutGCD;

PROCEDURE Bezout(u,v,w:T; VAR (*OUT*) x,y : T) RAISES {Error} =
BEGIN
  
END Bezout;
*)

(*==========================*)
BEGIN
END GCD.
