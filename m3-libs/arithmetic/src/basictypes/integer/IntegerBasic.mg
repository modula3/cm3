GENERIC MODULE IntegerBasic();
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error, Err;

CONST Module = "IntegerBasic.";
(*==========================*)
(*----------------------*)

PROCEDURE Add(x,y:T):T = BEGIN RETURN x+y END Add;
PROCEDURE Sub(x,y:T):T = BEGIN RETURN x-y END Sub;
PROCEDURE Neg(x:T):T   = BEGIN RETURN -x  END Neg;
PROCEDURE Conj(x:T):T  = BEGIN RETURN  x  END Conj;

PROCEDURE IsZero(x:T):BOOLEAN =
  BEGIN
    RETURN x=Zero;
  END IsZero;

PROCEDURE Mul(x,y:T):T = BEGIN RETURN x*y END Mul;
PROCEDURE Div(x,y:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    IF x MOD y # 0 THEN RAISE Error(Err.indivisible) END;
    RETURN x DIV y
  END Div;
PROCEDURE Mod(x,y:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    RETURN x MOD y;
  END Mod;
PROCEDURE DivMod(x,y:T;VAR r:T):T RAISES {Error} =
  BEGIN
    IF y=0 THEN RAISE Error(Err.divide_by_zero) END;
    r:=x MOD y;
    RETURN x DIV y;
  END DivMod;

(*============================*)
(* Factoring                  *)
(*============================*)

(*------------------------*)
PROCEDURE Factor(n:T;      (*factor this number*)
                 ):Array = (*giving primes and multiplicity*)
(*e.g., factor(24) gives 2^3 * 3^1 or {{2,3},{3,1}}*)
<*UNUSED*>CONST ftn = Module & "factor";
VAR
  len : [0..BITSIZE(T)] := 0;
  pl  : ARRAY [0..BITSIZE(T)] OF T;
  res : Array;
(* 1 means, the number can't be prime because it is divisible by 2,3,5 *)
(* 101010101010101010101010101010 *)
(* 100100100100100100100100100100 *)
(* 100001000010000100001000010000 *)
(* ------------------------------ *)
(* 101111101110101110101110111110 *)
CONST
   pr30ar = ARRAY OF T {2,3,5,7,11,13,17,19,23,29};
  mod30ar = ARRAY OF T {2,6,4,2,4,2,4,6};

VAR
  prime : T;

BEGIN
  (* check the first prime factors manually *)
  FOR i:=0 TO LAST(pr30ar) DO
    prime := pr30ar[i];
    WHILE (n MOD prime)=0 DO
      n := n DIV prime;
      pl[len] := prime;
      INC(len);
    END;
  END;

  (* check higher prime factors by skipping many non-primes *)
  WHILE prime*prime <= n DO
    FOR i:=0 TO LAST(mod30ar) DO
      INC (prime, mod30ar[i]);
      WHILE (n MOD prime)=0 DO
        n := n DIV prime;
        pl[len] := prime;
        INC(len);
      END;
    END;
  END;
  IF n>1 THEN
    pl[len] := n;
    INC(len);
  END;

  res:=NEW(Array,len);
  FOR i:=0 TO LAST(res^) DO
    res[i] := pl[i];
  END;

  RETURN res;
END Factor;

(*
PROCEDURE Factor(n:T;          (*factor this number*)
                 ):PowerArray= (*giving primes and multiplicity*)
(*e.g., factor(24) gives 2^3 * 3^1 or {{2,3},{3,1}}*)
*)
(*
PROCEDURE Factor(n:CARDINAL;   (*factor this number*)
                 VAR p,m:Array (*giving primes and multiplicity*)
                 ):CARDINAL=   (*and count of factors*)
(*e.g., factor(24) gives 2^3 * 3^1 or:
   p=[2,3]
   m=[3,1]
   return=2;
p and m are created by the procedure.
*)
CONST ftn = Module & "factor";
CONST MAXFACTOR = 30;
VAR
  i:CARDINAL;
  tmp:=n;
  ndx:=0;
BEGIN
  p:=NEW(Array,MAXFACTOR);
  m:=NEW(Array,MAXFACTOR);
  i:=2;
  WHILE i<=tmp DO
    IF isprime(i) AND (tmp MOD i = 0) THEN
      p[ndx]:=i; m[ndx]:=0;
      REPEAT
        tmp:=tmp DIV i;
        INC(m[ndx]);
      UNTIL tmp MOD i # 0;
      INC(ndx);
    END;
    INC(i);
  END;
  RETURN ndx;
END Factor;
*)

(*==========================*)
BEGIN
END IntegerBasic.
