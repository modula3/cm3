GENERIC MODULE Interpolation(RT,R);
(*Copyright (c) 1996, Harry George

Abstract: Interpolation routines.

12/28/95  Harry George    Initial version

1/29/96   Harry George    converted to m3na format
2/17/96   Harry George    converted to ADT format
*)
FROM xUtils IMPORT Error,Err;
FROM xReal64 IMPORT REAL64,Array,Zero,TINY;

CONST Module = "Interpolation.";
(*==========================*)

(*------------------*)
PROCEDURE linear(
                 READONLY xa,ya:ARRAY OF REAL64;(*interp table*)
                 x:REAL64;                      (*the input*)
                 ):REAL64=
(*Given an interpolation table with xa input and ya output,
do linear interpolation for x.
*)
VAR
  n:=NUMBER(xa); n1:=0; nn:=n-1;
  diffbest,diff:REAL64;
  ndx,ndx1,ndx2:CARDINAL;
  x1,x2,y1,y2:REAL64;
BEGIN
  (*---find the best start point---*)
  ndx:=n1; (*this is arbitrary, but fix the FOR loop if you change*)
  diffbest:=ABS(x-xa[ndx]);
  FOR i:=n1+1 TO nn DO
    diff:=ABS(x-xa[i]);
    IF diff < TINY THEN
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
END linear;

(*------------------*)
PROCEDURE newt(
                 READONLY xa,ya:ARRAY OF REAL64;(*interp table*)
                 x:REAL64;             (*the input*)
                 VAR dy:REAL64;        (*the error estimate*)
                 start,len:CARDINAL:=0 (*for partial access*)
                 ):REAL64  RAISES {Error}=
(*Given an interpolation table with xa input and ya output,
do Newton polynomial interpolation for x.  Report error estimate as dy.
Partial access: Give the starting index and the length to be used.
*)
CONST ftn = Module & "newt";
VAR
  xn,xn1,xnn,n,col_n:CARDINAL;
  c,d:Array;
  ndx:CARDINAL:=1;
  y,xi,xim,den,factor,diff,difftmp:REAL64;
BEGIN
  IF NUMBER(xa) # NUMBER(ya) THEN
    RAISE Error(Err.bad_size);
  END;

  IF len # 0 THEN
    (*use the start and len data for partial access*)
    IF start+len >= NUMBER(xa) THEN
      (*partial interp exceeds table length*)
      RAISE Error(Err.bad_size);
    END;
    xn:=len; xn1:=start; xnn:=xn1+xn-1;
  ELSE
    (*use the full tables*)
    xn:=NUMBER(xa); xn1:=0; xnn:=xn-1;
  END;
  (*either way, c and d are 0..n but we use 1..n*)
  n:=xn;
  c:=NEW(Array,n+1);
  d:=NEW(Array,n+1);

  (*---find starting y---*)
  ndx:=xnn;              (*get a starter x*)
  diff:=ABS(x-xa[ndx]);  (*and its difference from true x*)
  FOR i:=xn1 TO xnn DO
    difftmp:=ABS(x-xa[i]);
    IF difftmp < TINY THEN
      y:=ya[i]; dy:=Zero;
      RETURN y;
    ELSIF difftmp < diff THEN (*found a better one*)
      ndx:=i;  diff:=difftmp;
    END;
    c[i-xn1+1]:=ya[i];  (*c and d are 1..xn*)
  END;
  c[0]:=Zero;  (*even though we don't use it*)
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
      IF ABS(den) < TINY THEN
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
END newt;


(*==========================*)
BEGIN
END Interpolation.
