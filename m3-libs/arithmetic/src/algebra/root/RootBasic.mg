GENERIC MODULE RootBasic(P,R);
(*Copyright (c) 1995, Harry George

Abstract: Roots.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error;
IMPORT NumberTheory AS NT;

<*UNUSED*> CONST Module = "RootBasic.";


(*-----------------*)
(*handles the case that the leading coefficient c is not 1
  multiplies all roots with c to fix that,
  callers must remember this constant for interpreting the power sum*)
PROCEDURE ScaleUpRoots(x:T):T=
BEGIN
  IF R.Equal(x[LAST(x^)],R.One) THEN
    RETURN x;
  ELSE
    VAR
      c,pow:R.T;
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
      RETURN y;
    END;
  END;
END ScaleUpRoots;

PROCEDURE ScaleUpRootsFull(VAR x:TBody; c:R.T)=
VAR
  pow:R.T;
BEGIN
  pow:=c;
  FOR j:=LAST(x)-1 TO 0 BY -1 DO
    x[j]:=R.Mul(x[j],pow);
    pow:=R.Mul(pow,c);
  END;
END ScaleUpRootsFull;

(*-----------------*)
(*reverse ScaleUpRoots*)
(*
PROCEDURE ScaleDownRoots(x:T; c:R.T):T=
<*FATAL Error*> (*Err.indivisible should not occur, if x and c are chosen properly*)
BEGIN
  IF R.Equal(c,R.One) THEN
    RETURN x;
  ELSE
    VAR
      pow:R.T;
      y := NEW(T,NUMBER(x^));
    BEGIN
      pow:=c;
      y[LAST(y^)  ]:=c;
      y[LAST(y^)-1]:=x[LAST(x^)-1];
      FOR j:=LAST(x^)-2 TO 0 BY -1 DO
        y[j]:=R.Div(x[j],pow);
        pow:=R.Mul(pow,c);
      END;
      RETURN y;
    END;
  END;
END ScaleDownRoots;
*)

(*reverse ScaleUpRoots
  divide all roots by c and
  scale the coefficients by c^(n-exp) to achieve fraction free coefficients*)
PROCEDURE ScaleDownRoots(VAR x:TBody; c:R.T; exp:INTEGER)=
<*FATAL Error*> (*Err.indivisible should not occur, if x and c are chosen properly*)
BEGIN
  IF NOT R.Equal(c,R.One) THEN
    VAR
      pow:R.T;
    BEGIN
      pow:=c;
      FOR j:=exp+1 TO LAST(x) DO
        x[j]:=R.Mul(x[j],pow);
        pow:=R.Mul(pow,c);
      END;
      pow:=c;
      FOR j:=exp-1 TO 0 BY -1 DO
        x[j]:=R.Div(x[j],pow);
        pow:=R.Mul(pow,c);
      END;
    END;
  END;
END ScaleDownRoots;


PROCEDURE FromCardinal(n:CARDINAL):R.T=
VAR
  z:R.T;
BEGIN
  z:=R.Zero;
  FOR j:=1 TO n DO
    z:=R.Add(z,R.One);
  END;
  RETURN z;
END FromCardinal;

(*-----------------*)
PROCEDURE Add(
               x,y:T):T=
<*FATAL Error*> (*'indivisible' cannot occur*)
VAR
  qx,qy,qz:T;  (*polynomials with scaled roots, necessary for eliminating non-one leading coefficients*)
  px,py,pz:REF PowerSumSeq;
  cx,cy:R.T;
  nrx,nry:R.T;  (*number of roots*)
  binom,
  rn,rk,rnp,
  sum:R.T;
  nx:CARDINAL:=LAST(x^);
  ny:CARDINAL:=LAST(y^);
  nz:CARDINAL:=nx*ny;
BEGIN
  cx:=x[nx];
  cy:=y[ny];
  qx:=ScaleUpRoots(x);
  qy:=ScaleUpRoots(y);
  ScaleUpRootsFull(qx^,cy);
  ScaleUpRootsFull(qy^,cx);
  px:=ToPowerSumSeq(qx,nz);
  py:=ToPowerSumSeq(qy,nz);
  pz:=NEW(REF PowerSumSeq,nz);
  nrx:=FromCardinal(nx);
  nry:=FromCardinal(ny);
  rn:=R.Zero;
  FOR n:=0 TO LAST(pz^) DO
    rn:=R.Add(rn,R.One);
    rk:=R.Zero;
    binom:=R.One;
    rnp:=rn;
    (*applying binomial theorem:
      (x+y)^n=x^n+n*x^(n-1)*y+...+y^n*)
    sum:=R.Add(R.Mul(nrx,py[n]),R.Mul(nry,px[n]));
    FOR k:=1 TO n DO
      rk:=R.Add(rk,R.One);
      binom:=R.Div(R.Mul(binom,rnp),rk);
      rnp:=R.Sub(rnp,R.One);
      sum:=R.Add(sum,R.Mul(binom,R.Mul(px[k-1],py[n-k])));
    END;
    pz[n]:=sum;
  END;
  qz:=FromPowerSumSeq(pz^);
  ScaleDownRoots(qz^,cx,nz-ny);
  ScaleDownRoots(qz^,cy,nz-nx);
  RETURN qz;
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
  qx,qy:T;  (*polynomials with scaled roots, necessary for eliminating non-one leading coefficients*)
  px,py:REF PowerSumSeq;
  cx,cy:R.T;
  nx:CARDINAL:=LAST(x^);
  ny:CARDINAL:=LAST(y^);
  nz:CARDINAL:=nx*ny;
  z:T;
BEGIN
  cx:=x[LAST(x^)];
  cy:=y[LAST(y^)];
  qx:=ScaleUpRoots(x);
  qy:=ScaleUpRoots(y);
  px:=ToPowerSumSeq(qx,nz);
  py:=ToPowerSumSeq(qy,nz);
  (*assume that py is an independent buffer
    which can be messed here*)
  FOR j:=0 TO LAST(py^) DO
    py[j] := R.Mul(px[j],py[j]);
  END;
  z:=FromPowerSumSeq(py^);
(*more factors might be canceled in some cases (e.g. 1 as zero?)
  ScaleDownRoots(z^,cx,nx+2*ny-4);
  ScaleDownRoots(z^,cy,ny+2*nx-4);
*)
  ScaleDownRoots(z^,cx,nz-ny);
  ScaleDownRoots(z^,cy,nz-nx);
  RETURN z; (*for testing*)
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
(*simple and inefficient method: successively multiply linear factors*)
PROCEDURE FromRoots(READONLY root : ARRAY OF R.T):T=
VAR
  z:=P.One;
  fac:=P.New(1);
BEGIN
  fac[1]:=R.One;
  FOR j:=0 TO LAST(root) DO
    fac[0]:=R.Neg(root[j]);
    z:=P.Mul(z,fac);
  END;
  RETURN z;
END FromRoots;

(*--------------------*)
PROCEDURE ElimMultRoots(x:T):T=
BEGIN
  (*we need a special GCD for this purpose*)
  RETURN (GCD(x,P.Derive(x)));
END ElimMultRoots;


(*--------------------*)
(*given the sequence x of power sums,
  return the next one with respect to the polynomial y*)
(*consider x as a cyclic buffer for successive sums of powers of the roots*)
PROCEDURE GetNextPowerSum(READONLY x:PowerSumSeq; k:CARDINAL; y:T):R.T=
VAR
  z:R.T;
BEGIN
  z:=R.Zero;
  FOR j:=LAST(x) TO 0 BY -1 DO
    IF k=0 THEN
      k:=LAST(x);
    ELSE
      DEC(k);
    END;
    z:=R.Add(z,R.Mul(x[k],y[j]));
  END;
  RETURN R.Neg(z);
END GetNextPowerSum;

(*--------------------*)
PROCEDURE PowNSlow(x:T;
                    y:CARDINAL):T=
(*select each n-th element from the power sum sequence*)
VAR
  ps,psz:REF PowerSumSeq;
  j,k,l,m:CARDINAL;
  cp,cx,powcx:R.T;
  qx,qz:T;
BEGIN
  IF y=0 THEN
    RETURN One;
  ELSE
    cx:=x[LAST(x^)];
    qx:=ScaleUpRoots(x);

    ps:=ToPowerSumSeq(qx,LAST(qx^));
    psz:=NEW(REF PowerSumSeq,NUMBER(ps^));
    k:=y-1;
    j:=0;
    WHILE k<NUMBER(ps^) DO
      psz[j]:=ps[k];
      INC(j);
      INC(k,y);
    END;
    l:=0;
    m:=NUMBER(ps^);
    WHILE j<NUMBER(psz^) DO
      WHILE m<=k DO
        cp:=GetNextPowerSum(ps^,l,qx);
        ps[l]:=cp;
        INC(m);
        INC(l);
        IF l=NUMBER(ps^) THEN
          l:=0;
        END;
      END;
      psz[j]:=cp;
      INC(j);
      INC(k,y);
    END;
    qz:=FromPowerSumSeq(psz^);

    (*powcx:=cx^n*)
    powcx:=cx;
    FOR i:=2 TO y DO
      powcx:=R.Mul(powcx,cx);
    END;
    ScaleDownRoots(qz^,powcx,LAST(qz^)-1);
    RETURN qz;
  END;
END PowNSlow;

(*--------------------*)
(*speed up by factorizing the exponent into primes*)
PROCEDURE PowN(x:T;
               y:CARDINAL):T=
VAR
  pr:NT.Array;
BEGIN
  pr:=NT.Factor(y);
  FOR j:=0 TO LAST(pr^) DO
    x:=PowNSlow(x,pr[j]);
  END;
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

PROCEDURE ToPowerSumSeq(x:T;m:CARDINAL):REF PowerSumSeq=
VAR
  y:=NEW(T,m);
  sum:R.T;
  div:R.T;
BEGIN
  <*ASSERT R.Equal(x[LAST(x^)],R.One)*>
  x[LAST(x^)]:=R.One;
  div:=R.One;
  FOR n:=1 TO MIN(m,LAST(x^)) DO
    sum:=R.Mul(x[LAST(x^)-n],div);
    FOR j:=1 TO n-1 DO
      sum:=R.Add(sum,R.Mul(y[j-1],x[LAST(x^)-n+j]));
    END;
    y[n-1]:=R.Neg(sum);  (*R.Div(sum,x[LAST(x^)]), but it is only divisible if the leading coefficient is one, what we asserted at the beginning*)
    div:=R.Add(div,R.One);
  END;
  IF m>LAST(x^) THEN
    FOR n:=LAST(x^) TO LAST(y^) DO
      y[n]:=GetNextPowerSum(SUBARRAY(y^,n-LAST(x^),LAST(x^)),0,x);
    END;
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
