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
PROCEDURE Eval( 
                x:T;
                xi:R.T
                ):R.T=
VAR
  l:=LAST(x^);
  y:=x[l];
BEGIN
  FOR i:=l-1 TO 0 BY -1 DO
    y:=x[i]+xi*y;
  END;
  RETURN y;
END Eval;

(*-----------------*)
PROCEDURE Add( 
               x,y:T):T=
VAR
  xn:=NUMBER(x^); xl:=xn-1;
  yn:=NUMBER(y^); yl:=yn-1;
  zn:=MAX(xn,yn);
  z:=NEW(T,zn);
BEGIN
  IF xl>=yl THEN
    FOR i:=0    TO yl DO z[i]:=x[i]+y[i]; END;
    FOR i:=yl+1 TO xl DO z[i]:=x[i];      END;
  ELSE
    FOR i:=0    TO xl DO z[i]:=x[i]+y[i]; END;
    FOR i:=xl+1 TO yl DO z[i]:=     y[i]; END;
  END;
  RETURN z;
END Add;
(*-----------------*)
PROCEDURE Sub( 
               x,y:T):T=
VAR
  xn:=NUMBER(x^); xl:=xn-1;
  yn:=NUMBER(y^); yl:=yn-1;
  zn:=MAX(xn,yn);
  z:=NEW(T,zn);
BEGIN
  IF xl>=yl THEN
    FOR i:=0    TO yl DO z[i]:=x[i]-y[i]; END;
    FOR i:=yl+1 TO xl DO z[i]:=x[i];      END;
  ELSE
    FOR i:=0    TO xl DO z[i]:=x[i]-y[i]; END;
    FOR i:=xl+1 TO yl DO z[i]:=    -y[i]; END;
  END;
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN =
VAR
  xl:=LAST(x^);
  yl:=LAST(y^);
BEGIN
  IF xl>=yl THEN
    FOR i:=0    TO yl DO IF x[i]#y[i]   THEN RETURN FALSE END END;
    FOR i:=yl+1 TO xl DO IF x[i]#R.Zero THEN RETURN FALSE END END;
  ELSE
    FOR i:=0    TO xl DO IF x[i]#y[i]   THEN RETURN FALSE END END;
    FOR i:=xl+1 TO yl DO IF R.Zero#y[i] THEN RETURN FALSE END END;
  END;
  RETURN TRUE;
END Equal;

(*---------------------*)
PROCEDURE Mul( 
               x,y:T):T=
VAR
  xn:=NUMBER(x^);
  yn:=NUMBER(y^);
  zn:=xn+yn-1; zf:=0; zl:=zn-1;
  z:=NEW(T,zn);
BEGIN
  FOR i:=zf TO zl DO z[i]:=R.Zero; END;

  FOR i:=0 TO xn-1 DO
    FOR j:=0 TO yn-1 DO
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
    (*can't do any DivModides at all*)
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
    qtmp:=r[ri]/ymax;
    q[qi]:=qtmp;
    ri2:=ri+1;
    FOR yi:=yl TO y0 BY -1 DO
      DEC(ri2);
      r[ri2]:=r[ri2]-qtmp*y[yi];
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
  xl:=LAST(x^);
  b,psave:R.T;
BEGIN
  b:=x[xl]; psave:=x[xl-1]; x[xl-1]:=b;
  FOR i:=xl-2 TO 1 BY -1 DO
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
  xf:=FIRST(x^); xl:=LAST(x^);
  pdl:=nd; (*may be using part of pd vector*)
  fact,fac:R.T;
BEGIN
  IF nd>NUMBER(pd)+1 OR nd>xl THEN
    RAISE Error(Err.bad_size);
  END;

  (*---initialize f(xi) and clear f'(xi), f"(xi)...---*)
  pd[0]:=x[xl];
  FOR i:=1 TO pdl DO pd[i]:=R.Zero; END;
  
  (*---collect the raw values---*)
  FOR i:=xl-1 TO xf BY -1 DO
    FOR j:=pdl TO 1 BY -1 DO
      pd[j]:=pd[j-1]+xi*pd[j];
    END;
    pd[0]:=x[i]+xi*pd[0];
  END;

  (*---fix the factorials---*) 
  fact:=R.One;
  fac:=R.Zero;
  FOR i:=0 TO pdl DO
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
