MODULE NumberTheory;
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

(*FROM xUtils IMPORT Error, Err;*)

CONST Module = "NumberTheory.";
(*==========================*)

(*============================*)
(* Factoring                  *)
(*============================*)


(*------------------------*)
PROCEDURE FactorPrivate (n:T;VAR pl:ARRAY PowerRange OF T) : PowerRange =
<*UNUSED*>CONST ftn = Module & "FactorPrivate";
VAR
  len : PowerRange := 0;
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

  RETURN len;
END FactorPrivate;

PROCEDURE Factor(n:T):Array =
VAR
  len : PowerRange;
  pl  : ARRAY PowerRange OF T;
  res : Array;
BEGIN
  len:=FactorPrivate(n,pl);
  res:=NEW(Array,len);
  res^:=SUBARRAY(pl,0,len);
  RETURN res;
END Factor;

PROCEDURE FactorPower(n:T):PowerArray =
VAR
  numprime,
  len     : PowerRange;
  pl      : ARRAY PowerRange OF T;
  res     : PowerArray;
  lastfac : T;
BEGIN
  len:=FactorPrivate(n,pl);
  numprime:=len;
  IF len>0 THEN
    FOR i:=1 TO len-1 DO
      IF pl[i]=pl[i-1] THEN
        DEC(numprime);
      END;
    END;
    res:=NEW(PowerArray,numprime);
    lastfac:=0;
    VAR
      j : [-1..BITSIZE(T)] := -1;
    BEGIN
      FOR i:=0 TO len-1 DO
        IF pl[i]=lastfac THEN
          INC(res[j].exp);
        ELSE
          INC(j);
          res[j].p   := pl[i];
          res[j].exp := 1;
        END;
        lastfac:=pl[i];
      END;
    END;
    RETURN res;
  ELSE
    RETURN NEW(PowerArray,0);
  END;
END FactorPower;

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
    IF IsPrime(i) AND (tmp MOD i = 0) THEN
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
END NumberTheory.
