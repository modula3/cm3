GENERIC MODULE RootBasic(R);
(*Copyright (c) 1995, Harry George

Abstract: Roots.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "RootBasic.";

(*--------------------*)
PROCEDURE New(
               n:CARDINAL):T=
BEGIN
  RETURN NEW(T,n+1);
END New;

(*--------------------*)
PROCEDURE Copy(
               x:T):T=
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  y^:=x^;
  RETURN y;
END Copy;

(*--------------------*)
PROCEDURE Strip(
                x:T):T=
VAR
  n:=LAST(x^);
BEGIN
  IF NOT R.IsZero(x[n]) THEN
    RETURN x;
  ELSE
    REPEAT
      DEC(n);
    UNTIL n=FIRST(x^) OR NOT R.IsZero(x[n]);
  END;
  VAR
    y:=NEW(T,n+1);
  BEGIN
    y^:=SUBARRAY(x^,0,NUMBER(y^));
    RETURN y;
  END;
END Strip;


(*-----------------*)
PROCEDURE Add(
               x,y:T):T=
VAR
  xl:=LAST(x^);
  yl:=LAST(y^);
  zn:=MAX(NUMBER(x^),NUMBER(y^));
  z:=NEW(T,zn);
BEGIN
  IF xl>=yl THEN
    FOR i:=0    TO yl DO z[i]:=R.Add(x[i],y[i]); END;
    FOR i:=yl+1 TO xl DO z[i]:=      x[i];       END;
  ELSE
    FOR i:=0    TO xl DO z[i]:=R.Add(x[i],y[i]); END;
    FOR i:=xl+1 TO yl DO z[i]:=           y[i];  END;
  END;
  RETURN Strip(z);
END Add;
(*-----------------*)
PROCEDURE Sub(
               x,y:T):T=
VAR
  xl:=LAST(x^);
  yl:=LAST(y^);
  zn:=MAX(NUMBER(x^),NUMBER(y^));
  z:=NEW(T,zn);
BEGIN
  IF xl>=yl THEN
    FOR i:=0    TO yl DO z[i]:=R.Sub(x[i],y[i]); END;
    FOR i:=yl+1 TO xl DO z[i]:=      x[i];       END;
  ELSE
    FOR i:=0    TO xl DO z[i]:=R.Sub(x[i],y[i]); END;
    FOR i:=xl+1 TO yl DO z[i]:=     R.Neg(y[i]); END;
  END;
  RETURN Strip(z);
END Sub;

(*---------------------*)
PROCEDURE Neg(x:T):T =    (*return -x *)
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    y[i] := R.Neg(x[i]);
  END;
  RETURN y;
END Neg;

(*---------------------*)
PROCEDURE IsZero(x:T):BOOLEAN =
BEGIN
  RETURN x=NIL OR NUMBER(x^)=0 OR R.IsZero(x[0]);
END IsZero;

(*---------------------*)
PROCEDURE Mul(
               x,y:T):T=
VAR
  z:=NEW(T,NUMBER(x^)+NUMBER(y^)-1);
BEGIN
  FOR i:=FIRST(z^) TO LAST(z^) DO z[i]:=R.Zero; END;

  FOR i:=FIRST(x^) TO LAST(x^) DO
    FOR j:=FIRST(y^) TO LAST(y^) DO
      z[i+j]:=R.Add(z[i+j],R.Mul(x[i],y[j]));
    END;
  END;
  RETURN Strip(z);
END Mul;

(*---------------------*)
PROCEDURE Div(
               x,y:T):T RAISES {Error}=
<*UNUSED*>
CONST ftn = Module & "Div";
VAR
  xn:=NUMBER(x^);                xl:=LAST(x^);
  yn:=NUMBER(y^); y0:=FIRST(y^); yl:=LAST(y^);
  q,r:T;
  qtmp,ymax:R.T;
  qn,q0,ql,qi,ri2:CARDINAL;
BEGIN
  (*---Copy numerator into r---*)
  r:=NEW(T,xn); r^:=x^;

  (*---check for quick exit---*)
  IF xl<yl THEN
    (*can't do any Divides at all*)
    q:=NEW(T,1); q[0]:=R.Zero;
    RETURN q;
  END;

  (*---setup quotient---*)
  qn:=xn-yn+1;
  q:=NEW(T,qn); q0:=FIRST(q^); ql:=LAST(q^);

  (*---find the dominant denominator term---*)
  ymax:=y[yl];


  (*---compute---*)
  qi:=ql+1;
  FOR ri:=xl TO (xl-ql) BY-1 DO
    DEC(qi);
    qtmp:=R.Div(r[ri],ymax);
    q[qi]:=qtmp;
    ri2:=ri;
    r[ri2]:=R.Zero;  (*subtraction of values that should be equal does not work for floating point numbers*)
    FOR yi:=yl-1 TO y0 BY -1 DO
      DEC(ri2);
      r[ri2]:=R.Sub(r[ri2],R.Mul(qtmp,y[yi]));
    END;
  END;
  (*This check will probably fail on floating point numbers*)
  FOR ri:=(xl-ql)-1 TO 0 BY -1 DO
    IF NOT R.Equal(r[ri],R.Zero) THEN
      RAISE Error(Err.indivisible);
    END;
  END;
  RETURN q;
END Div;

(*---------------------*)
PROCEDURE DivMod(
               x,y:T;
           VAR r:T):T RAISES {Error} =
<*UNUSED*>
CONST ftn = Module & "DivMod";
VAR
  xn:=NUMBER(x^);                xl:=LAST(x^);
  yn:=NUMBER(y^); y0:=FIRST(y^); yl:=LAST(y^);
  q:T;
  qtmp,ymax:R.T;
  qn,q0,ql,qi,ri2:CARDINAL;
BEGIN
  (*---Copy numerator into r---*)
  r:=NEW(T,xn); r^:=x^;

  (*---check for quick exit---*)
  IF xl<yl THEN
    (*can't do any Divides at all*)
    q:=NEW(T,1); q[0]:=R.Zero;
    RETURN q;
  END;

  (*---setup quotient---*)
  qn:=xn-yn+1;
  q:=NEW(T,qn); q0:=FIRST(q^); ql:=LAST(q^);

  (*---find the dominant denominator term---*)
  ymax:=y[yl];


  (*---compute---*)
  qi:=ql+1;
  FOR ri:=xl TO (xl-ql) BY-1 DO
    DEC(qi);
    qtmp:=R.DivMod(r[ri],ymax,r[ri]);
    q[qi]:=qtmp;
    ri2:=ri;
    FOR yi:=yl-1 TO y0 BY -1 DO
      DEC(ri2);
      r[ri2]:=R.Sub(r[ri2],R.Mul(qtmp,y[yi]));
    END;
  END;
  r := Strip(r);
  RETURN Strip(q);
END DivMod;

(*--------------------*)
PROCEDURE Mod(x,y:T):T RAISES {Error} =
(*Using DivMod is not optimal.
  One may save a bit space for the quotient.*)
VAR
  z:T;
BEGIN
  EVAL DivMod(x,y,z);
  RETURN z;
END Mod;

(*--------------------*)
PROCEDURE ElimMultRoots(x:T):T=
BEGIN
  (*we need a special GCD for this purpose*)
  (*RETURN (GCD(x,P.Derive(x)));*)
  RETURN x;
END ElimMultRoots;

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

PROCEDURE ToPowerSumSeq(x:T):REF ARRAY OF R.T=
VAR
  y:=NEW(T,NUMBER(x^)-1);
BEGIN
  <*ASSERT R.Equal(x[LAST(x^)],R.One)*>
  RETURN y;
END ToPowerSumSeq;

PROCEDURE FromPowerSumSeq(READONLY x:ARRAY OF R.T):T RAISES{Error}=
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
