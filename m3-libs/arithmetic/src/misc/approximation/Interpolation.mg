GENERIC MODULE Interpolation(RT,R);
(*Copyright (c) 1996, Harry George

Abstract: Interpolation routines.

12/28/95  Harry George    Initial version

1/29/96   Harry George    converted to m3na format
2/17/96   Harry George    converted to ADT format
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "Interpolation.";
(*==========================*)

(*------------------*)
PROCEDURE Linear(
                 READONLY xa,ya:ARRAY OF R.T;
                 x:R.T;
                 ):R.T=
(*Given an interpolation table with xa input and ya output,
do linear interpolation for x.
*)
VAR
  n:=NUMBER(xa); n1:=0; nn:=n-1;
  diffbest,diff:R.T;
  ndx,ndx1,ndx2:CARDINAL;
  x1,x2,y1,y2:R.T;
BEGIN
  (*---find the best start point---*)
  ndx:=n1; (*this is arbitrary, but fix the FOR loop if you change*)
  diffbest:=ABS(x-xa[ndx]);
  FOR i:=n1+1 TO nn DO
    diff:=ABS(x-xa[i]);
    IF diff < RT.Tiny THEN
      (*quick victory*)
      RETURN ya[i];
    ELSIF diff<diffbest THEN
      ndx:=i; diffbest:=diff;
    END;
  END;

  (*---find the best partner---*)
  IF    ndx=n1 THEN
    ndx1:=n1; ndx2:=n1+1;
  ELSIF ndx=nn THEN
    ndx1:=nn-1; ndx2:=nn;
  ELSIF ABS(x-xa[ndx-1])<ABS(x-xa[ndx+1]) THEN
    ndx1:=ndx-1; ndx2:=ndx;
  ELSE
    ndx1:=ndx; ndx2:=ndx+1;
  END;

  (*---compute the y value---*)
  x1:=xa[ndx1]; y1:=ya[ndx1];
  x2:=xa[ndx2]; y2:=ya[ndx2];
  RETURN y1+((y2-y1)/(x2-x1))*(x-x1);
END Linear;

(*------------------*)
PROCEDURE Newton(
                 READONLY xa,ya:ARRAY OF R.T;
                 x:R.T;
                 VAR dy:R.T;
                 ):R.T  RAISES {Error}=
(*Given an interpolation table with xa input and ya output,
do Newton polynomial interpolation for x.  Report error estimate as dy.
Partial access: Give the starting index and the length to be used.
*)
<*UNUSED*>
CONST ftn = Module & "Newton";
VAR
  xn,xn1,xnn,n,col_n:CARDINAL;
  c:=NEW(REF ARRAY OF R.T,NUMBER(xa)+1);
  d:=NEW(REF ARRAY OF R.T,NUMBER(xa)+1);
  ndx:CARDINAL:=1;
  y,xi,xim,den,factor,diff,difftmp:R.T;
BEGIN
  IF NUMBER(xa) # NUMBER(ya) THEN
    RAISE Error(Err.bad_size);
  END;

  (*use the full tables*)
  xn:=NUMBER(xa); xn1:=0; xnn:=xn-1;
  (*either way, c and d are 0..n but we use 1..n*)
  n:=xn;

  (*---find starting y---*)
  ndx:=xnn;              (*get a starter x*)
  diff:=ABS(x-xa[ndx]);  (*and its difference from true x*)
  FOR i:=xn1 TO xnn DO
    difftmp:=ABS(x-xa[i]);
    IF difftmp < RT.Tiny THEN
      y:=ya[i]; dy:=R.Zero;
      RETURN y;
    ELSIF difftmp < diff THEN (*found a better one*)
      ndx:=i;  diff:=difftmp;
    END;
    c[i-xn1+1]:=ya[i];  (*c and d are 1..xn*)
  END;
  c[0]:=R.Zero;  (*even though we don't use it*)
  d^:=c^;     (*load d from c, thus from ya*)

  y:=ya[ndx]; (*use the best ndx to get starting y*)

  (*---compute and use c and d---*)
  DEC(ndx,xn1);  (*adjust for partial access*)
  col_n:=n;  (*originally there are n in the col*)
  FOR m:=1 TO n-1 DO
    DEC(col_n); (*each col recalc loses 1 cell*)
    FOR i:=1 TO col_n DO
      xi:=xa[xn1+i-1];  xim:=xa[xn1+(i-1)+m];
      den:=xi-xim;
      IF ABS(den) < RT.Tiny THEN
        RAISE Error(Err.divide_by_zero);
      END;
      factor:=(c[i+1]-d[i])/den;
      d[i]:=(xim-x)*factor;
      c[i]:=(xi-x)*factor;
    END;
    (*---which correction to use?---*)
    IF ndx*2 >= col_n THEN
      (*we are at or below the center, need to move up*)
      dy:=d[ndx]; DEC(ndx);
    ELSE
      (*we are above the center, need to move down*)
      dy:=c[ndx+1];
      (*don't need to adjust ndx, because it is effectively
      moved down when we slide the next col up*)
    END;
    (*---update y---*)
    y:=y+dy;
  END;
  RETURN y;
END Newton;

(*------------------*)
PROCEDURE CubicHermite(
                 READONLY xa,ya:ARRAY OF R.T;  (*interpolation nodes*)
                 x:R.T;                        (*the function argument*)
                 ):R.T=

  PROCEDURE InterpolateQuadratic(READONLY xb,yb:ARRAY [0..2] OF R.T):R.T=
  VAR
    x01:=xb[0]-xb[1];
    x12:=xb[1]-xb[2];
    x02:=xb[0]-xb[2];
    xx0:=x-xb[0];
    xx1:=x-xb[1];
    xx2:=x-xb[2];
    sum:R.T;
  BEGIN
    sum:=   -yb[1]*xx0*xx2/(x01*x12);
    sum:=sum+yb[0]*xx1*xx2/(x01*x02);
    sum:=sum+yb[2]*xx0*xx1/(x12*x02);
    RETURN sum;
  END InterpolateQuadratic;

  (*probably not very efficient*)
  PROCEDURE InterpolateHalf(READONLY xb,yb:ARRAY [0..2] OF R.T):R.T=
  CONST
    Three = FLOAT(3,R.T);
  VAR
    x01:=xb[0]-xb[1];
    x12:=xb[1]-xb[2];
    x02:=xb[0]-xb[2];
    xin12:=(x-xb[2])/x12;
    hermy1,        (*p(x[1])=1, p(x'[1])=0, p(x[2])=0, p(x'[2])=0*)
    hermdy1 : R.T; (*p(x[1])=0, p(x'[1])=1, p(x[2])=0, p(x'[2])=0*)
    sum:R.T;
  BEGIN
    hermy1 :=xin12*xin12*(Three-R.Two*xin12);
    hermdy1:=xin12*xin12*(x-xb[1]);
    sum:=(hermdy1*(x01-x12)/(x01*x12)+hermy1)*yb[1];
    sum:=sum+hermdy1*x12/(x01*x02)*yb[0];
    sum:=sum-hermdy1*x01/(x12*x02)*yb[2];
    RETURN sum;
  END InterpolateHalf;

  PROCEDURE InterpolatePiece(READONLY xb,yb:ARRAY [0..3] OF R.T):R.T=
  BEGIN
    RETURN InterpolateHalf(SUBARRAY(xb,0,3),SUBARRAY(yb,0,3)) +
           InterpolateHalf(ARRAY OF R.T{xb[3],xb[2],xb[1]},
                           ARRAY OF R.T{yb[3],yb[2],yb[1]});
  END InterpolatePiece;

BEGIN
  IF x<=xa[1] THEN
    RETURN InterpolateQuadratic(SUBARRAY(xa,0,3),SUBARRAY(ya,0,3));
  ELSE
    FOR j:=2 TO LAST(xa)-1 DO
      IF (*xa[j-1]<x AND*) x<=xa[j] THEN
        RETURN InterpolatePiece(SUBARRAY(xa,j-2,4),SUBARRAY(ya,j-2,4));
      END;
    END;
    RETURN InterpolateQuadratic(SUBARRAY(xa,LAST(xa)-2,3),SUBARRAY(xa,LAST(xa)-2,3));
  END;
END CubicHermite;


(*==========================*)
BEGIN
END Interpolation.
