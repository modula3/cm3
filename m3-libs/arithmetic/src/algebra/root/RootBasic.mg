GENERIC MODULE RootBasic(P,R);
(*Copyright (c) 1995, Harry George

Abstract: Roots.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error;

<*UNUSED*> CONST Module = "RootBasic.";


(*-----------------*)
(*handles the case that the leading coefficient c is not 1
  multiplies all roots with c to fix that,
  callers must remember this constant for interpreting the power sum*)
PROCEDURE ToPowerSumSeqExt(x:T):REF PowerSumSeq=
VAR
  c,pow:R.T;
BEGIN
  IF R.Equal(x[LAST(x^)],R.One) THEN
    RETURN ToPowerSumSeq(x);
  ELSE
    VAR
      y := NEW(T,NUMBER(x^));
    BEGIN
      c:=x[LAST(x^)];
      pow:=c;
      y[LAST(y^)  ]:=R.One;
      y[LAST(y^)-1]:=x[LAST(x^)-1];
      FOR j:=LAST(x^)-2 TO 0 BY -1 DO
        y[j]:=R.Mul(x[j],pow);
        pow:=R.Mul(pow,c);
      END;
      RETURN ToPowerSumSeq(y);
    END;
  END;
END ToPowerSumSeqExt;


(*-----------------*)
PROCEDURE Add(
               x,y:T):T=
<*FATAL Error*> (*'indivisible' cannot occur*)
VAR
  px,py:REF PowerSumSeq;
BEGIN
  px:=ToPowerSumSeq(x);
  py:=ToPowerSumSeq(y);
  RETURN FromPowerSumSeq(px^);
END Add;
(*-----------------*)
PROCEDURE Sub(
               x,y:T):T=
BEGIN
  RETURN Add(x,Neg(y));
END Sub;

(*---------------------*)
PROCEDURE Neg(x:T):T =
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=LAST(x^)-1 TO 0 BY -2 DO
    y[i+1] :=       x[i+1];
    y[i  ] := R.Neg(x[i  ]);
  END;
  IF NUMBER(x^) MOD 2 # 0 THEN
    y[0] := x[0];
  END;
  RETURN y;
END Neg;

(*---------------------*)
PROCEDURE IsZero(x:T):BOOLEAN =
VAR
  nonzerofound := FALSE;
BEGIN
  IF x=NIL OR NUMBER(x^)<=1 THEN
    RETURN FALSE;
  END;
  FOR j:=0 TO LAST(x^) DO
    IF NOT R.IsZero(x[j]) THEN
      IF nonzerofound THEN
        (*two non-zero coefficients found,
          there must be roots different from zero*)
        RETURN FALSE;
      ELSE
        nonzerofound:=TRUE;
      END;
    END;
  END;
  (*at least one non-zero coefficient must found,
    if we arrive here it was exactly one no-zero coefficient*)
  RETURN nonzerofound;
END IsZero;

(*---------------------*)
PROCEDURE Mul(
               x,y:T):T=
<*FATAL Error*> (*'indivisible' cannot occur*)
VAR
  px,py:REF PowerSumSeq;
  cx,cy:R.T;
BEGIN
  cx:=x[LAST(x^)]; px:=ToPowerSumSeq(x);
  cy:=y[LAST(y^)]; py:=ToPowerSumSeq(y);
  RETURN FromPowerSumSeq(px^);
END Mul;

(*---------------------*)
PROCEDURE Div(
               x,y:T):T RAISES {Error}=
BEGIN
  RETURN Mul(x,Rec(y));
END Div;

(*---------------------*)
PROCEDURE Rec(READONLY x:T):T RAISES {Error}=
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  FOR j:=0 TO LAST(x^) DO
    y[j]:=x[LAST(x^)-j];
  END;
  RETURN y;
END Rec;

(*---------------------*)
PROCEDURE DivMod(
               x,y:T;
           VAR r:T):T RAISES {Error} =
BEGIN
  r:=Zero;
  RETURN Mul(x,Rec(y));
END DivMod;

(*--------------------*)
PROCEDURE Mod(x,y:T):T RAISES {Error} =
BEGIN
  RETURN Zero;
END Mod;

(*--------------------*)
PROCEDURE GCD(x,y:T):T=
VAR
  z:T;
BEGIN
  WHILE NOT IsZero(y) DO
(*
    z:=P.Reduce(x,y);
*)
    x:=y;
    y:=z;
  END;
  RETURN x;
END GCD;

(*--------------------*)
PROCEDURE ElimMultRoots(x:T):T=
BEGIN
  (*we need a special GCD for this purpose*)
  RETURN (GCD(x,P.Derive(x)));
END ElimMultRoots;

(*given the sequence x of power sums,
  return the next one with respect to the polynomial y*)
(*consider x as a cyclic buffer for successive sums of powers of the roots*)
PROCEDURE GetNextPowerSum(READONLY x:PowerSumSeq; k:CARDINAL; y:T):R.T=
VAR
  c,pow,z:R.T;
BEGIN
  c:=y[LAST(y^)];
  z:=R.Zero;
  FOR j:=LAST(x^) TO 0 BY -1 DO
    IF k=0 THEN
      k:=LAST(x);
    ELSE
      DEC(k);
    END;
    z:=R.Add(z,R.Mul());
  END;
END GetNextPowerSum;

PROCEDURE PowN(READONLY x:T;
                        y:CARDINAL):T=
BEGIN
  RETURN x;
END PowN;


(*
  s[k] - the elementary symmetric polynomial of degree k
  p[k] - sum of the k-th power
  
  s(t) := s[0] + s[1]*t + s[2]*t^2 + ...
  p(t) :=        p[1]*t + p[2]*t^2 + ...
  
  Then it holds
    t*s'(t) + p(-t)*s(t) = 0
  This can be proven by considering p as sum of geometric series
  and differentiating s in the root-wise factored form.
  
  The differential equation can be separated for each power of t
  which leads to a recurrence equation:
    n*s[n] + Sum((-1)^j*p[j]*s[n-j],j from 1 to n) = 0

  Note that we index the coefficients the other way round
  and that the coefficients of the polynomial
  are not pure elementary symmetric polynomials of the roots
  but have alternating signs, too.
*)

PROCEDURE ToPowerSumSeq(x:T):REF PowerSumSeq=
VAR
  y:=NEW(T,NUMBER(x^)-1);
  sum:R.T;
  div:R.T;
BEGIN
  <*ASSERT R.Equal(x[LAST(x^)],R.One)*>
  x[LAST(x^)]:=R.One;
  div:=R.One;
  FOR n:=1 TO LAST(x^) DO
    sum:=R.Mul(x[LAST(x^)-n],div);
    FOR j:=1 TO n-1 DO
      sum:=R.Add(sum,R.Mul(y[j-1],x[LAST(x^)-n+j]));
    END;
    y[n-1]:=R.Neg(sum);  (*R.Div(sum,x[LAST(x^)]), but it is only divisible if the leading coefficient is one, what we asserted at the beginning*)
    div:=R.Add(div,R.One);
  END;
  RETURN y;
END ToPowerSumSeq;

PROCEDURE FromPowerSumSeq(READONLY x:PowerSumSeq):T RAISES{Error}=
VAR
  y:=NEW(T,NUMBER(x)+1);
  sum:R.T;
  div:R.T;
BEGIN
  y[LAST(y^)]:=R.One;
  div:=R.One;
  FOR n:=1 TO LAST(y^) DO
    sum:=R.Zero;
    FOR j:=1 TO n DO
      sum:=R.Add(sum,R.Mul(x[j-1],y[LAST(y^)-n+j]));
    END;
    y[LAST(y^)-n]:=R.Neg(R.Div(sum,div));
    div:=R.Add(div,R.One);
  END;
  RETURN y;
END FromPowerSumSeq;

(*==========================*)
BEGIN
  Zero:=NEW(T,2); Zero^ := TBody{R.Zero,R.One};
  One :=NEW(T,2); One^  := TBody{R.MinusOne,R.One};
END RootBasic.
