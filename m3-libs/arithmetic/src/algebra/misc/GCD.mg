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
    WHILE NOT R.IsZero(v) DO
      w:=R.Mod(u,v);
      u:=v;
      v:=w;
    END;
  (*
    WHILE v#0 DO
      w:=u MOD v;
      u:=v;
      v:=w;
    END;
  *)
  EXCEPT
    Error(err) => <*ASSERT err#Err.divide_by_zero*>
  END;
  RETURN u;
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

PROCEDURE BezoutGCD(u,v:T; VAR (*OUT*) c : ARRAY [0..1],[0..1] OF T) : T =
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
  next:ARRAY [0..1] OF T;
BEGIN
  c[0,0] := R.One;
  c[0,1] := R.Zero;
  c[1,0] := R.Zero;
  c[1,1] := R.One;
  TRY
    WHILE NOT R.IsZero(v) DO
      q:=R.DivMod(u,v,w);
      u:=v;
      v:=w;
      next[0]:=R.Sub(c[0,0],R.Mul(c[0,0],q));
      next[1]:=R.Sub(c[0,1],R.Mul(c[0,1],q));
      c[0] := c[1];
      c[1] := next;
    END;
  EXCEPT
    Error(err) => <*ASSERT err#Err.divide_by_zero*>
  END;
  RETURN u;
END BezoutGCD;

PROCEDURE Bezout(u,v,w:T; VAR (*OUT*) c : ARRAY [0..1],[0..1] OF T) RAISES {Error} =
(*
The former routine gives us c00, c01, c10, c11 for each u,v with
  c00*u + c01*v = gcd(u,v)
  c10*u + c11*v = 0
Now let
  k = w / gcd(u,v)  (if this is indivisible the Bezout equation is not solvable)
then a solution is
  k*c00*u + k*c01*v  = w
but the coefficients k*c00 and k*c01 are to large and can be reduced:
  (k*c00-f*c10)*u + (k*c01-f*c11)*v = w
By dividing k*c00 by c10 with remainder one obtains the smallest possible coefficient for u and the coefficent for v cannot not to large since it is limitted by w and u and its coefficient.
*)
VAR
  gcd, k, f, r : T;
BEGIN
  gcd := BezoutGCD(u,v,c);
  k := R.Div(w,gcd);
  c[0,0] := R.Mul(c[0,0],k);
  c[0,1] := R.Mul(c[0,1],k);
  (*reduce c[0,0] and c[0,1]*)
  f := R.DivMod(c[0,0],c[1,0],r);
  c[0,0] := r;
  c[0,1] := R.Sub(c[0,1],R.Mul(f,c[1,1]));
END Bezout;

PROCEDURE MACDecompose(u,v:T; VAR (*OUT*) mac : MAC) : T =
VAR
  w:T;
BEGIN
  TRY
    mac:=NIL;
    WHILE NOT R.IsZero(v) DO
      VAR
        newmac := NEW(MAC);
      BEGIN
        newmac.factor:=R.DivMod(u,v,w);
        newmac.next:=mac;
        mac:=newmac;
      END;
      u:=v;
      v:=w;
    END;
  EXCEPT
    Error(err) => <*ASSERT err#Err.divide_by_zero*>
  END;
  RETURN u;
END MACDecompose;

(*==========================*)
BEGIN
END GCD.
