GENERIC MODULE PolynomialFast(R);
(*Copyright (c) 1995, Harry George

Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "PolynomialFast.";

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
  n:=NUMBER(x^);
  z:=NEW(T,n);
BEGIN
  z^:=x^;
  RETURN z;
END Copy;

(*--------------------*)
(*
PROCEDURE Zero( 
               x:T)=
VAR
  n:=NUMBER(x^); n1:=0; nn:=n-1;
BEGIN
  FOR i:=n1 TO nn DO
    x[i]:=R.Zero;
  END;
END Zero;
(*--------------------*)
PROCEDURE One( 
               x:T)=
VAR
  n:=NUMBER(x^); n0:=0; nn:=n-1;
BEGIN
  x[0]:=R.One;
  FOR i:=n0+1 TO nn DO
    x[i]:=R.Zero;
  END;
END One;
*)
(*--------------------*)
<*OBSOLETE*>
PROCEDURE Eval( 
                x:T;
                xi:R.T
                ):R.T=
VAR
  n:=NUMBER(x^); nn:=n-1;
  z:=x[nn];
BEGIN
  FOR i:=nn-1 TO 1 BY -1 DO
    z:=x[i]+xi*z;
  END;
  z:=x[0]+z;  (*is this correct?*)
  RETURN z;
END Eval;

(*-----------------*)
PROCEDURE Add( 
               x,y:T):T=
VAR
  p1n:=NUMBER(x^); p1nn:=p1n-1;
  p2n:=NUMBER(y^); p2nn:=p2n-1;
  maxn:=MAX(p1n,p2n);
  z:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO z[i]:=x[i]+y[i]; END;
    FOR i:=p2nn+1 TO p1nn DO z[i]:=x[i];       END;
  ELSE
    FOR i:=0 TO p1nn      DO z[i]:=x[i]+y[i]; END;
    FOR i:=p1nn+1 TO p2nn DO z[i]:=      y[i]; END;
  END;
  RETURN z;
END Add;
(*-----------------*)
PROCEDURE Sub( 
               x,y:T):T=
VAR
  p1n:=NUMBER(x^); p1nn:=p1n-1;
  p2n:=NUMBER(y^); p2nn:=p2n-1;
  maxn:=MAX(p1n,p2n);
  z:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO z[i]:=x[i]-y[i]; END;
    FOR i:=p2nn+1 TO p1nn DO z[i]:=x[i];       END;
  ELSE
    FOR i:=0 TO p1nn      DO z[i]:=x[i]-y[i]; END;
    FOR i:=p1nn+1 TO p2nn DO z[i]:=     -y[i]; END;
  END;
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN =
VAR
  p1nn:=LAST(x^);
  p2nn:=LAST(y^);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO IF NOT x[i]#y[i]  THEN RETURN FALSE END END;
    FOR i:=p2nn+1 TO p1nn DO IF NOT x[i]#R.Zero THEN RETURN FALSE END END;
  ELSE
    FOR i:=0 TO p1nn      DO IF NOT x[i]#y[i]  THEN RETURN FALSE END END;
    FOR i:=p1nn+1 TO p2nn DO IF NOT R.Zero#y[i] THEN RETURN FALSE END END;
  END;
  RETURN TRUE;
END Equal;

(*---------------------*)
PROCEDURE Mul( 
               x,y:T):T=
VAR
  p1n:=NUMBER(x^); p2n:=NUMBER(y^);
  pn:=p1n+p2n-1; p0:=0; pnn:=pn-1;
  z:=NEW(T,pn);
BEGIN
  FOR i:=p0 TO pnn DO z[i]:=R.Zero; END;

  FOR i:=0 TO p1n-1 DO
    FOR j:=0 TO p2n-1 DO
      z[i+j]:=z[i+j]+x[i]*y[j];
    END;
  END;
  RETURN z;
END Mul;

(*---------------------*)
PROCEDURE Div( 
               x,y:T):T RAISES {Error}=
VAR
  r,q:T;
BEGIN
  q:=DivMod(x,y,r);
  IF NOT Equal(r,Zero) THEN
    RAISE Error(Err.indivisible);
  END;
  RETURN q;
END Div;

(*---------------------*)
PROCEDURE DivMod( 
               x,y:T;
           VAR r:T):T=
<*UNUSED*>
CONST ftn = Module & "DivMod";
VAR
  p1n:=NUMBER(x^);                  p1nn:=LAST(x^); 
  p2n:=NUMBER(y^); p20:=FIRST(y^); p2nn:=LAST(y^);
  q:T;
  qtmp,p2max:R.T;
  qn,q0,qnn,qi,ri2:CARDINAL;
BEGIN
  (*---Copy numerator into r---*)
  r:=NEW(T,p1n); r^:=x^;

  (*---check for quick exit---*)
  IF p1nn<p2nn THEN
    (*can't do any DivModides at all*)
    q:=NEW(T,1); q[0]:=R.Zero;
    RETURN q;
  END;

  (*---setup quotient---*)
  qn:=p1n-p2n+1;
  q:=NEW(T,qn); q0:=FIRST(q^); qnn:=LAST(q^);

  (*---find the dominant denominator term---*)
  p2max:=y[p2nn];


  (*---compute---*)
  qi:=qnn+1;
  FOR ri:=p1nn TO (p1nn-qnn) BY-1 DO
    DEC(qi);
    qtmp:=r[ri]/p2max;
    q[qi]:=qtmp;
    ri2:=ri+1;
    FOR p2i:=p2nn TO p20 BY -1 DO
      DEC(ri2);
      r[ri2]:=r[ri2]-qtmp*y[p2i];
    END;
  END;
  RETURN q;
END DivMod;

(*-----------------------*)
(*
PROCEDURE deflate( 
                   x:T;
                   c:R.T;
                   VAR rem:R.T)=
VAR
  pnn:=LAST(x^);
  b,psave:R.T;
BEGIN
  b:=x[pnn]; psave:=x[pnn-1]; x[pnn-1]:=b;
  FOR i:=pnn-2 TO 1 BY -1 DO
    b:=psave+c*b;
    psave:=x[i]; x[i]:=b;
  END;
  rem:=x[0]+c*x[1];
END deflate;
*)

(*---------------------*)
PROCEDURE Derive(x:T;           (*differentiate polynomial*)
                 ):T =
VAR
  q:=NEW(T,LAST(x^));
BEGIN
  FOR n:=0 TO LAST(q^) DO
    q[n]:=x[n+1]*FLOAT(n+1,R.T);
  END;
  RETURN q;
END Derive;

(*---------------------*)
PROCEDURE EvalDerivate( 
                 x:T;      (*Evaluate the poly with these coefs*)
                 xi:R.T;    (*for this argument*)
             VAR pd:ARRAY OF R.T;  (*returning x(xi), x'(xi)...*)
                 nd:CARDINAL  (*for up to nd EvalDerivateatives*)
                 ) RAISES {Error}=
(*Given a poly with coefs x, find the value at xi as pd[0],
and nd more EvalDerivateatives as pd[1]..pd[nd].

raises:
   Err.bad_size if nd>NUMBER(pd)+1 
*)
VAR
  p0:=FIRST(x^); pnn:=LAST(x^);
  pdnn:=nd; (*may be using part of pd vector*)
  fact,fac:R.T;
BEGIN
  IF nd>NUMBER(pd)+1 OR nd>pnn THEN
    RAISE Error(Err.bad_size);
  END;

  (*---initialize f(xi) and clear f'(xi), f"(xi)...---*)
  pd[0]:=x[pnn];
  FOR i:=1 TO pdnn DO pd[i]:=R.Zero; END;
  
  (*---collect the raw values---*)
  FOR i:=pnn-1 TO p0 BY -1 DO
    FOR j:=pdnn TO 1 BY -1 DO
      pd[j]:=pd[j-1]+xi*pd[j];
    END;
    pd[0]:=x[i]+xi*pd[0];
  END;

  (*---fix the factorials---*) 
  fact:=R.One;
  fac:=R.Zero;
  FOR i:=0 TO pdnn DO
    pd[i]:=fact*pd[i];
    fac:=fac+R.One;
    fact:=fact*fac;
  END;

END EvalDerivate; 

(*==========================*)
BEGIN
  Zero:=NEW(T,1); Zero[0] := R.Zero;
  One :=NEW(T,1); One [0] := R.One;
END PolynomialFast.
