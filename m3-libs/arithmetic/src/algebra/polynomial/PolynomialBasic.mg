GENERIC MODULE PolynomialBasic(R);
(*Copyright (c) 1995, Harry George

Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "PolynomialBasic.";

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
  q:=NEW(T,NUMBER(x^));
BEGIN
  q^:=x^;
  RETURN q;
END Copy;

(*--------------------*)
PROCEDURE Strip( 
                x:T):T=
VAR
  n:=LAST(x^);
BEGIN
  IF R.Equal(x[n],R.Zero) THEN
    RETURN x;
  ELSE
    REPEAT
      DEC(n);
    UNTIL n=FIRST(x^) OR NOT R.Equal(x[n],R.Zero);
  END;
  VAR
    q:=NEW(T,n+1);
  BEGIN
    q^:=SUBARRAY(x^,0,NUMBER(q^));
(*
  WHILE n>=FIRST(x^) DO
    q[n]:=x[n];
  END;
*)
    RETURN q;
  END;
END Strip;


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
(*Horner's scheme*)
PROCEDURE Eval( 
                x:T;
                xi:R.T
                ):R.T=
VAR
  nn:=LAST(x^);
  y:=x[nn];
BEGIN
  FOR i:=nn-1 TO 0 BY -1 DO
    y:=R.Add(x[i],R.Mul(xi,y));
  END;
  RETURN y;
END Eval;

(*-----------------*)
PROCEDURE Add( 
               x,y:T):T=
VAR
  p1nn:=LAST(x^);
  p2nn:=LAST(y^);
  maxn:=MAX(NUMBER(x^),NUMBER(y^));
  z:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO z[i]:=R.Add(x[i],y[i]); END;
    FOR i:=p2nn+1 TO p1nn DO z[i]:=      x[i];        END;
  ELSE
    FOR i:=0 TO p1nn      DO z[i]:=R.Add(x[i],y[i]); END;
    FOR i:=p1nn+1 TO p2nn DO z[i]:=            y[i];  END;
  END;
  RETURN Strip(z);
END Add;
(*-----------------*)
PROCEDURE Sub( 
               x,y:T):T=
VAR
  p1nn:=LAST(x^);
  p2nn:=LAST(y^);
  maxn:=MAX(NUMBER(x^),NUMBER(y^));
  z:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO z[i]:=R.Sub(x[i],y[i]); END;
    FOR i:=p2nn+1 TO p1nn DO z[i]:=      x[i];        END;
  ELSE
    FOR i:=0 TO p1nn      DO z[i]:=R.Sub(x[i],y[i]); END;
    FOR i:=p1nn+1 TO p2nn DO z[i]:=      R.Neg(y[i]); END;
  END;
  RETURN Strip(z);
END Sub;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN =
VAR
  p1nn:=LAST(x^);
  p2nn:=LAST(y^);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO IF NOT R.Equal(x[i],y[i])  THEN RETURN FALSE END END;
    FOR i:=p2nn+1 TO p1nn DO IF NOT R.Equal(x[i],R.Zero) THEN RETURN FALSE END END;
  ELSE
    FOR i:=0 TO p1nn      DO IF NOT R.Equal(x[i],y[i])  THEN RETURN FALSE END END;
    FOR i:=p1nn+1 TO p2nn DO IF NOT R.Equal(R.Zero,y[i]) THEN RETURN FALSE END END;
  END;
  RETURN TRUE;
END Equal;

(*---------------------*)
PROCEDURE Mul( 
               x,y:T):T=
VAR
  p1n:=NUMBER(x^);
  p2n:=NUMBER(y^);
  pn:=p1n+p2n-1;
  z:=NEW(T,pn);
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO x[i]:=R.Zero; END;

  FOR i:=0 TO p1n-1 DO
    FOR j:=0 TO p2n-1 DO
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
  p1n:=NUMBER(x^);                  p1nn:=LAST(x^); 
  p2n:=NUMBER(y^); p20:=FIRST(y^); p2nn:=LAST(y^);
  q,r:T;
  qtmp,p2max:R.T;
  qn,q0,qnn,qi,ri2:CARDINAL;
BEGIN
  (*---Copy numerator into r---*)
  r:=NEW(T,p1n); r^:=x^;

  (*---check for quick exit---*)
  IF p1nn<p2nn THEN
    (*can't do any Divides at all*)
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
    qtmp:=R.Div(r[ri],p2max);
    q[qi]:=qtmp;
    ri2:=ri;
    r[ri2]:=R.Zero;  (*subtraction of values that should be equal does not work for floating point numbers*)
    FOR p2i:=p2nn-1 TO p20 BY -1 DO
      DEC(ri2);
      r[ri2]:=R.Sub(r[ri2],R.Mul(qtmp,y[p2i]));
    END;
  END;
  (*This check will probably fail on floating point numbers*)
  FOR ri:=(p1nn-qnn)-1 TO 0 BY -1 DO
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
    (*can't do any Divides at all*)
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
    qtmp:=R.DivMod(r[ri],p2max,r[ri]);
    q[qi]:=qtmp;
    ri2:=ri;
    FOR p2i:=p2nn-1 TO p20 BY -1 DO
      DEC(ri2);
      r[ri2]:=R.Sub(r[ri2],R.Mul(qtmp,y[p2i]));
    END;
  END;
  r := Strip(r);
  RETURN Strip(q);
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
  fac:=R.Zero;
BEGIN
  FOR n:=0 TO LAST(q^) DO
    fac:=R.Add(fac,R.One);
    q[n]:=R.Mul(x[n+1],fac);
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
      pd[j]:=R.Add(pd[j-1],R.Mul(xi,pd[j]));
    END;
    pd[0]:=R.Add(x[i],R.Mul(xi,pd[0]));
  END;

  (*---fix the factorials---*) 
  fact:=R.One;
  fac:=R.Zero;
  FOR i:=0 TO pdnn DO
    pd[i]:=R.Mul(pd[i],fact);
    fac:=R.Add(fac,R.One);
    fact:=R.Mul(fact,fac);
  END;

END EvalDerivate;

(*==========================*)
BEGIN
  Zero:=NEW(T,1); Zero[0] := R.Zero;
  One :=NEW(T,1); One [0] := R.One;
END PolynomialBasic.
