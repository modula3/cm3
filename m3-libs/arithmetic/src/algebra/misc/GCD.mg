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

PROCEDURE BezoutGCD(u,v:T; VAR (*OUT*) x,y : T) : T =
(*
/ u \ - / q0 1 \ / q1 1 \ ... / gcd(u,v) \
\ v / - \ 1  0 / \ 1  0 /     \    0     /

... / 0  1  \ / 0  1  \ / u \ - / gcd(u,v) \
    \ 1 -q1 / \ 1 -q0 / \ v / - \    0     /

/ a 1 \ / 0  1 \ - / 1 0 \
\ 1 0 / \ 1 -a / - \ 0 1 /
*)
VAR
  w,q:T;
  mat:ARRAY [0..2],[0..1] OF T;
BEGIN
  mat[0,0] := R.One;
  mat[0,1] := R.Zero;
  mat[1,0] := R.Zero;
  mat[1,1] := R.One;
  TRY
    WHILE NOT R.IsZero(u) DO
      q:=R.DivMod(v,u,w);
      v:=u;
      u:=w;
      mat[2,0]:=R.Sub(mat[0,0],R.Mul(mat[1,0],q));
      mat[2,1]:=R.Sub(mat[0,1],R.Mul(mat[1,1],q));
      SUBARRAY(mat,0,2) := SUBARRAY(mat,1,2);
    END;
    x:=mat[0,0];
    y:=mat[0,1];
  EXCEPT
    Error(err) => <*ASSERT err#Err.divide_by_zero*>
  END;
  RETURN v;
END BezoutGCD;

(*
PROCEDURE Bezout(u,v,w:T; VAR (*OUT*) x,y : T) RAISES {Error} =
BEGIN
  
END Bezout;
*)

(*==========================*)
BEGIN
END GCD.
