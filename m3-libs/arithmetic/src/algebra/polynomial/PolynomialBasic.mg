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
               p:T):T=
VAR
  n:=NUMBER(p^);
  tmp:=NEW(T,n);
BEGIN
  tmp^:=p^;
  RETURN tmp;
END Copy;

(*--------------------*)
(*
PROCEDURE Zero( 
               p:T)=
VAR
  n:=NUMBER(p^); n1:=0; nn:=n-1;
BEGIN
  FOR i:=n1 TO nn DO
    p[i]:=R.Zero;
  END;
END Zero;
(*--------------------*)
PROCEDURE One( 
               p:T)=
VAR
  n:=NUMBER(p^); n0:=0; nn:=n-1;
BEGIN
  p[0]:=R.One;
  FOR i:=n0+1 TO nn DO
    p[i]:=R.Zero;
  END;
END One;
*)
(*--------------------*)
PROCEDURE Eval( 
                p:T;
                x:R.T
                ):R.T=
VAR
  nn:=LAST(p^);
  y:=p[nn];
BEGIN
  FOR i:=nn-1 TO 0 BY -1 DO
    y:=R.Add(p[i],R.Mul(x,y));
  END;
  RETURN y;
END Eval;

(*-----------------*)
PROCEDURE Add( 
               p1,p2:T):T=
VAR
  p1nn:=LAST(p1^);
  p2nn:=LAST(p2^);
  maxn:=MAX(NUMBER(p1^),NUMBER(p2^));
  p:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO p[i]:=R.Add(p1[i],p2[i]); END;
    FOR i:=p2nn+1 TO p1nn DO p[i]:=      p1[i];        END;
  ELSE
    FOR i:=0 TO p1nn      DO p[i]:=R.Add(p1[i],p2[i]); END;
    FOR i:=p1nn+1 TO p2nn DO p[i]:=            p2[i];  END;
  END;
  RETURN p;
END Add;
(*-----------------*)
PROCEDURE Sub( 
               p1,p2:T):T=
VAR
  p1nn:=LAST(p1^);
  p2nn:=LAST(p2^);
  maxn:=MAX(NUMBER(p1^),NUMBER(p2^));
  p:=NEW(T,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO p[i]:=R.Sub(p1[i],p2[i]); END;
    FOR i:=p2nn+1 TO p1nn DO p[i]:=      p1[i];        END;
  ELSE
    FOR i:=0 TO p1nn      DO p[i]:=R.Sub(p1[i],p2[i]); END;
    FOR i:=p1nn+1 TO p2nn DO p[i]:=      R.Neg(p2[i]); END;
  END;
  RETURN p;
END Sub;

(*---------------------*)
PROCEDURE Equal(p1,p2:T):BOOLEAN =
VAR
  p1nn:=LAST(p1^);
  p2nn:=LAST(p2^);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO IF NOT R.Equal(p1[i],p2[i])  THEN RETURN FALSE END END;
    FOR i:=p2nn+1 TO p1nn DO IF NOT R.Equal(p1[i],R.Zero) THEN RETURN FALSE END END;
  ELSE
    FOR i:=0 TO p1nn      DO IF NOT R.Equal(p1[i],p2[i])  THEN RETURN FALSE END END;
    FOR i:=p1nn+1 TO p2nn DO IF NOT R.Equal(R.Zero,p2[i]) THEN RETURN FALSE END END;
  END;
  RETURN TRUE;
END Equal;

(*---------------------*)
PROCEDURE Mul( 
               p1,p2:T):T=
VAR
  p1n:=NUMBER(p1^);
  p2n:=NUMBER(p2^);
  pn:=p1n+p2n-1;
  p:=NEW(T,pn);
BEGIN
  FOR i:=FIRST(p^) TO LAST(p^) DO p[i]:=R.Zero; END;

  FOR i:=0 TO p1n-1 DO
    FOR j:=0 TO p2n-1 DO
      p[i+j]:=R.Add(p[i+j],R.Mul(p1[i],p2[j]));
    END;
  END;
  RETURN p;
END Mul;

(*---------------------*)
PROCEDURE DivMod( 
               p1,p2:T;
           VAR r:T):T RAISES {Error} =
<*UNUSED*>
CONST ftn = Module & "DivMod";
VAR
  p1n:=NUMBER(p1^);                  p1nn:=LAST(p1^); 
  p2n:=NUMBER(p2^); p20:=FIRST(p2^); p2nn:=LAST(p2^);
  q:T;
  qtmp,p2max:R.T;
  qn,q0,qnn,qi,ri2:CARDINAL;
BEGIN
  (*---Copy numerator into r---*)
  r:=NEW(T,p1n); r^:=p1^;

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
  p2max:=p2[p2nn];


  (*---compute---*)
  qi:=qnn+1;
  FOR ri:=p1nn TO (p1nn-qnn) BY-1 DO
    DEC(qi);
    qtmp:=R.Div(r[ri],p2max);
    q[qi]:=qtmp;
    ri2:=ri+1;
    FOR p2i:=p2nn TO p20 BY -1 DO
      DEC(ri2);
      r[ri2]:=R.Sub(r[ri2],R.Mul(qtmp,p2[p2i]));
    END;
  END;
  RETURN q;
END DivMod;

(*-----------------------*)
(*
PROCEDURE deflate( 
                   p:T;
                   c:R.T;
                   VAR rem:R.T)=
VAR
  pnn:=LAST(p^);
  b,psave:R.T;
BEGIN
  b:=p[pnn]; psave:=p[pnn-1]; p[pnn-1]:=b;
  FOR i:=pnn-2 TO 1 BY -1 DO
    b:=psave+c*b;
    psave:=p[i]; p[i]:=b;
  END;
  rem:=p[0]+c*p[1];
END deflate;
*)

(*---------------------*)
PROCEDURE Derive(p:T;           (*differentiate polynomial*)
                 ):T =
VAR
  q:=NEW(T,LAST(p^));
BEGIN
  FOR n:=0 TO LAST(q^) DO
    q[n]:=R.ScaleInt(p[n+1],n+1);
  END;
  RETURN q;
END Derive;

(*---------------------*)
PROCEDURE EvalDerivate( 
                 p:T;      (*Evaluate the poly with these coefs*)
                 x:R.T;    (*for this argument*)
             VAR pd:ARRAY OF R.T;  (*returning p(x), p'(x)...*)
                 nd:CARDINAL  (*for up to nd EvalDerivateatives*)
                 ) RAISES {Error}=
(*Given a poly with coefs p, find the value at x as pd[0],
and nd more EvalDerivateatives as pd[1]..pd[nd].

raises:
   Err.bad_size if nd>NUMBER(pd)+1 
*)
VAR
  p0:=FIRST(p^); pnn:=LAST(p^);
  pdnn:=nd; (*may be using part of pd vector*)
  fact:R.T;
BEGIN
  IF nd>NUMBER(pd)+1 OR nd>pnn THEN
    RAISE Error(Err.bad_size);
  END;

  (*---initialize f(x) and clear f'(x), f"(x)...---*)
  pd[0]:=p[pnn];
  FOR i:=1 TO pdnn DO pd[i]:=R.Zero; END;
  
  (*---collect the raw values---*)
  FOR i:=pnn-1 TO p0 BY -1 DO
    FOR j:=pdnn TO 1 BY -1 DO
      pd[j]:=R.Add(pd[j-1],R.Mul(x,pd[j]));
    END;
    pd[0]:=R.Add(p[i],R.Mul(x,pd[0]));
  END;

  (*---fix the factorials---*) 
  fact:=R.One;
  FOR i:=0 TO pdnn DO
    pd[i]:=R.Mul(pd[i],fact);
	fact:=R.ScaleInt(fact,i+1);
  END;

END EvalDerivate; 

(*==========================*)
BEGIN
  Zero:=NEW(T,1); Zero[0] := R.Zero;
  One :=NEW(T,1); One [0] := R.One;
END PolynomialBasic.
