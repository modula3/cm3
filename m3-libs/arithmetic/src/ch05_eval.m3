MODULE ch05_eval EXPORTS nr;
(*Copyright (c) 1995, Harry George
  
Abstract: Implementation of Modula-3 version of
          NR92, ch 5.

12/27/95  Harry George    Initial version
*)
IMPORT IO,Fmt,Wr,TextWr,Math,nrp;

CONST Module = "ch05_eval.";
(*----------------------*)
PROCEDURE eulsum(VAR sum:REAL32;    (*partial sum to date*)
                    term:REAL32;    (*jth value*)
                   jterm:CARDINAL;  (*which j*)
                    wksp:Vector;    (*workspace for calcs*)
               VAR nterm:CARDINAL   (*how high is n so far*)
                        ) RAISES {Error}=
(*Used to evaluate summation series.  The caller has to provide
vars for wksp and nterm, but never modify them directly.
Start with jterm:=1 and term:=<first value>.  Then for
each call thereafter do the next jterm and term, in order.
Sum remains up to date, so you can quit whenever your exit criteria
are met.
*)
CONST ftn = Module & "eulsum";
VAR
  tmp1,tmp2:REAL32;
BEGIN
  IF nterm+1 > LAST(wksp^) THEN
    RAISE Error(Err.bad_size);
  END;
  
  IF jterm=1 THEN
    nterm:=1;
    wksp[1]:=term;
    sum:=0.5*term;
  ELSE
    tmp1:=wksp[1];
    FOR j:=1 TO nterm-1 DO
      tmp2:=wksp[j+1];
      wksp[j+1]:=0.5*(wksp[j]+tmp1);
      tmp1:=tmp2;
    END;
    wksp[nterm+1]:=0.5*(wksp[nterm]+tmp1);
    IF ABS(wksp[nterm+1]) < ABS(wksp[nterm]) THEN
      (*increase p, and use longer table*)
      INC(nterm);
      sum:=sum+0.5*wksp[nterm];
    ELSE
      (*increase n, table doesn't change*)
      sum:=sum+wksp[nterm+1];
    END;
  END;
END eulsum;
(*----------------------*)
PROCEDURE ratval(x:REAL64;         (*evaluate at x*)
                 num,den:dVector   (*coeffs*)
                 ):REAL64=
(*Given x and the coeffs for the numerator and denominator in a
rational function, determine R(x):
  R(x)= P(x)/Q(x)=num/den=(p0+p1*x+p2*x^2...)/(q0+q1*x+q2*x^2...)
*)
CONST dTINY=1.0d-100;
VAR
  n:=NUMBER(num^); n1:=0; nn:=n-1;
  d:=NUMBER(den^); d1:=0; dd:=d-1;
  sum_num,sum_den:REAL64;
BEGIN
  FOR i:=nn TO n1 BY -1 DO sum_num:=sum_num*x+num[i]; END;
  FOR i:=dd TO d1 BY -1 DO sum_den:=sum_den*x+den[i]; END;
  IF ABS(sum_den)<dTINY THEN sum_den:=dTINY; END;
  RETURN sum_num/sum_den;
END ratval;


(*---------------------*)
PROCEDURE quadreal (a,b,c:REAL32;       (*coefs*)
                    VAR x1,x2:COMPLEX)= (*results*)  
(*Given a*x^2+b*x+c=0, solve for x.*)
VAR
  disc,q:REAL32;
BEGIN
  disc:=b*b-4.0*a*c;
  IF disc<0.0 THEN
    (*use the complex version*)
    quadcmpx(Complex(a,0.0),Complex(b,0.0),Complex(c,0.0),
             x1,x2);
    RETURN;
  END;

  q:=-0.5*(b+sgn(b)*sqrt(disc));
  x1.re:=q/a; x1.im:=0.0;
  x2.re:=c/q; x2.im:=0.0;
END quadreal;
(*---------------------*)
PROCEDURE quadcmpx (a,b,c:COMPLEX;       (*coefs*)
                    VAR x1,x2:COMPLEX)= (*results*)  
(*Given a*x^2+b*x+c=0, solve for x.*)
CONST
  c4=COMPLEX{re:=4.0,im:=0.0};
  c05=COMPLEX{re:=-0.5,im:=0.0};
VAR
  disc,disc_sqrt,q:COMPLEX;  
BEGIN
  disc:=Csub(Cmul(b,b),Cmul(c4,Cmul(a,c)));
  disc_sqrt:=Csqrt(disc);

  (*---set sign of sqrt via eqn 5.6.6---*)
  IF Cmul(Conjg(b),disc_sqrt).re<0.0 THEN
    disc_sqrt.re:=-disc_sqrt.re;
  END;

  (*---calculate per eqn 5.6.4, 5.6.5.---*)  
  q:=Cmul(c05,Cadd(b,disc_sqrt));
  x1:=Cdiv(q,a);
  x2:=Cdiv(c,q);
END quadcmpx;
(*---Polynomials---*)
PROCEDURE Padd(u,v:Vector):Vector=
(*return u+v, where both are polynomials of form
|  p=a0+a1*x^1+a2*x^2...
and the coefs a[i] are in the vector p[i].
*)
VAR
  un:=NUMBER(u^); unn:=un-1;
  vn:=NUMBER(v^); vnn:=vn-1;
  maxn:=MAX(un,vn);
  p:=NEW(Vector,maxn);
BEGIN
  IF unn>=vnn THEN
    FOR i:=0 TO vnn     DO p[i]:=u[i]+v[i]; END;
    FOR i:=vnn+1 TO unn DO p[i]:=u[i];      END;
  ELSE
    FOR i:=0 TO unn     DO p[i]:=u[i]+v[i]; END;
    FOR i:=unn+1 TO vnn DO p[i]:=     v[i]; END;
  END;
  RETURN p;
END Padd;
(*---------------------*)
PROCEDURE Psub(u,v:Vector):Vector=
(*return u-v, where both are polynomials of form
|  p=a0+a1*x^1+a2*x^2...
and the coefs a[i] are in the vector p[i].
*)
VAR
  un:=NUMBER(u^); unn:=un-1;
  vn:=NUMBER(v^); vnn:=vn-1;
  maxn:=MAX(un,vn);
  p:=NEW(Vector,maxn);
BEGIN
  IF unn>=vnn THEN
    FOR i:=0 TO vnn     DO p[i]:=u[i]-v[i]; END;
    FOR i:=vnn+1 TO unn DO p[i]:=u[i];      END;
  ELSE
    FOR i:=0 TO unn     DO p[i]:=u[i]-v[i]; END;
    FOR i:=unn+1 TO vnn DO p[i]:=    -v[i]; END;
  END;
  RETURN p;
END Psub;

(*---------------------*)
PROCEDURE Pmul(u,v:Vector):Vector=
(*return u*v, where both are polynomials of form
|  p=a0+a1*x^1+a2*x^2...
and the coefs a[i] are in the vector p[i].
*)
VAR
  un:=NUMBER(u^); vn:=NUMBER(v^);
  pn:=un+vn-1; p0:=0; pnn:=pn-1;
  p:=NEW(Vector,pn);
BEGIN
  FOR i:=p0 TO pnn DO p[i]:=0.0; END;

  FOR i:=0 TO un-1 DO
    FOR j:=0 TO vn-1 DO
      p[i+j]:=p[i+j]+u[i]*v[j];
    END;
  END;
  RETURN p;
END Pmul;

(*---------------------*)
PROCEDURE Pdiv(u,v:Vector;
           VAR q,r:Vector)=
(*return u,v as quotient q and remainder r,
where u,v,q,r are polynomials of form
|  p=a0+a1*x^1+a2*x^2...
and the coefs a[i] are in the vector p[i].
*)
CONST ftn = Module & "Pdiv";
VAR
  un:=NUMBER(u^); u0:=FIRST(u^); unn:=LAST(u^); 
  vn:=NUMBER(v^); v0:=FIRST(v^); vnn:=LAST(v^);
  qtmp,vmax:REAL32;
  qn,q0,qnn,qi,ri2:CARDINAL;
BEGIN
  (*---copy numerator into r---*)
  r:=NEW(Vector,un); r^:=u^;

  (*---check for quick exit---*)
  IF unn<vnn THEN
    (*can't do any divides at all*)
    q:=NEW(Vector,1); q[0]:=0.0;
    RETURN;
  END;

  (*---setup quotient---*)
  qn:=un-vn+1;
  q:=NEW(Vector,qn); q0:=FIRST(q^); qnn:=LAST(q^);

  (*---find the dominant denominator term---*)
  vmax:=v[vnn];


  (*---compute---*)
  qi:=qnn+1;
  FOR ri:=unn TO (unn-qnn) BY-1 DO
    DEC(qi);
    qtmp:=r[ri]/vmax;
    q[qi]:=qtmp;
    ri2:=ri+1;
    FOR vi:=vnn TO v0 BY -1 DO
      DEC(ri2);
      r[ri2]:=r[ri2]-qtmp*v[vi];
    END;
  END;
END Pdiv;

(*---------------------*)
PROCEDURE ddpoly(c:Vector;    (*evaluate the poly with these coefs*)
                 x:REAL32;    (*at this point*)
                 pd:Vector;   (*returning p(x), p'(x)...*)
                 nd:CARDINAL  (*and up to nd derivatives*)
                 ) RAISES {Error}=
(*Given a poly with coefs c, find the value at x as pd[0],
and nd more derivatives as pd[1]..pd[nd].

raises:
   Err.bad_size if nd>NUMBER(pd)+1 
*)
VAR
  cn:=NUMBER(c^); c0:=FIRST(c^); cnn:=LAST(c^);
  pdn:=nd+1; pd0:=0; pdnn:=nd; (*may be using part of pd vector*)
BEGIN
  IF nd>NUMBER(pd^)+1 OR nd>cnn THEN
    RAISE Error(Err.bad_size);
  END;

  (*---initialize f(x) and clear f'(x), f"(x)...---*)
  pd[0]:=c[cnn];
  FOR i:=1 TO pdnn DO pd[i]:=0.0; END;
  
  (*---collect the raw values---*)
  FOR i:=cnn-1 TO c0 BY -1 DO
    FOR j:=pdnn TO 1 BY -1 DO
      pd[j]:=pd[j-1]+x*pd[j];
    END;
    pd[0]:=pd[0]*x+c[i];
  END;

  (*---fix the factorials---*) 
  FOR i:=0 TO pdnn DO
    pd[i]:=factrl(i)*pd[i];
  END;
 
END ddpoly; 
(*----------------------*)
PROCEDURE Ptext(poly:Vector;
                x:TEXT:="x";       
                style:Fmt.Style:=Fmt.Style.Fix;
                prec:CARDINAL:=1
                ):TEXT=
(*Generate a text object for the polynomial poly, in form:
| p0+p1*x+p3*x^2...
|where p0,p1,... are poly[0],poly[1],...
*)
VAR
  n:=NUMBER(poly^); n1:=0; nn:=n-1;
  wr:=NEW(TextWr.T).init(); 
BEGIN
  Wr.PutText(wr,"(" & Fmt.Real(poly[n1],style,prec));
  FOR i:=n1+1 TO nn DO
    Wr.PutText(wr,"+" & Fmt.Real(poly[i],style,prec));
    Wr.PutText(wr,"*" & x & "^" & Fmt.Int(i));
  END;
  Wr.PutText(wr,")");
  RETURN TextWr.ToText(wr);
END Ptext;
(*----------------------*)
PROCEDURE dPtext(poly:dVector;
                x:TEXT:="x";       
                style:Fmt.Style:=Fmt.Style.Fix;
                prec:CARDINAL:=1
                ):TEXT=
(*Generate a text object for the polynomial poly, in form:
| p0+p1*x+p3*x^2...
|where p0,p1,... are poly[0],poly[1],...
*)
VAR
  n:=NUMBER(poly^); n1:=0; nn:=n-1;
  wr:=NEW(TextWr.T).init(); 
BEGIN
  Wr.PutText(wr,"(" & Fmt.LongReal(poly[n1],style,prec));
  FOR i:=n1+1 TO nn DO
    Wr.PutText(wr,"+" & Fmt.LongReal(poly[i],style,prec));
    Wr.PutText(wr,"*" & x & "^" & Fmt.Int(i));
  END;
  Wr.PutText(wr,")");
  RETURN TextWr.ToText(wr);
END dPtext;

(*===chebyshev approximations===*)
REVEAL
  ChebyApprox = PublicCheby BRANDED OBJECT
    c:Vector;
  OVERRIDES
    init:=ChebyInit;
    findm:=ChebyFindM;
    derivative:=ChebyDerivative;
    integral:=ChebyIntegral;
    eval:=ChebyEval;
  END;
(*---------------------------*)
PROCEDURE ChebyInit(self:ChebyApprox;
                    func:Ftn;
                    maxn:CARDINAL:=30   (*number of coeffs to find*)
                    ):ChebyApprox=
VAR
  n:=maxn; n1:=0; nn:=n-1; nr:=FLOAT(n,REAL32);
  f:=NEW(Vector,n+1);  (*we skip f[0]*)
  x,factor,sum,jr,kr:REAL32;
BEGIN
  self.c:=NEW(Vector,maxn);

  (*---load up the function values---*)
  FOR k:=1 TO n DO
    kr:=FLOAT(k,REAL32);
    x:=cos(Math.Pi*(kr-0.5)/nr);
    f[k]:=func(x);
  END;

  (*---compute coeffs---*)
  factor:=2.0/nr;
  FOR j:=0 TO n-1 DO
    jr:=FLOAT(j,REAL32);
    sum:=0.0;
    FOR k:=1 TO n DO
      kr:=FLOAT(k,REAL32);
      sum:=sum+f[k]*cos(Math.Pi*jr*(kr-0.5)/nr);
    END;
    self.c[j]:=factor*sum;
  END;
  RETURN self;
END ChebyInit;
(*--------------------------*)
PROCEDURE ChebyFindM(self:ChebyApprox;
                     prec:REAL32:=0.0001 (*use m coeffs to get precision*)
                        ):CARDINAL RAISES {Error}=
                   
CONST ftn = Module & "ChebyFindM";
VAR
  m:CARDINAL:=0;
BEGIN
  FOR i:=0 TO LAST(self.c^)-1 DO
    IF ABS(self.c[i])<=prec AND ABS(self.c[i+1])<=prec THEN
      RETURN i;
    END;
  END;
  RAISE Error(Err.not_converging);
  RETURN 0;  
END ChebyFindM;

(*--------------------------*)
PROCEDURE ChebyDerivative(self:ChebyApprox;
                          m:CARDINAL):ChebyApprox=    (*return derivative*)
VAR
  n:=NUMBER(self.c^);
  tmp:=NEW(ChebyApprox);
BEGIN
  tmp.c:=NEW(Vector,n);
  tmp.c[n-1]:=0.0;
  tmp.c[n-2]:=FLOAT(2*(n-1),REAL32)*self.c[n-1];
  FOR j:=n-3 TO 0 DO
    tmp.c[j]:=tmp.c[j+2]+FLOAT(2*(j+1))*self.c[j+1];
  END;
  RETURN tmp;
END ChebyDerivative;
(*--------------------------*)
PROCEDURE ChebyIntegral(self:ChebyApprox;
                        m:CARDINAL):ChebyApprox=      (*return integral*)
VAR
  n:=NUMBER(self.c^);  n1:=0;  nn:=n-1;
  tmp:=NEW(ChebyApprox);
BEGIN
  tmp.c:=NEW(Vector,n);
  tmp.c[n1]:=0.0;
  FOR j:=n1+1 TO nn-1 DO
    tmp.c[j]:=(self.c[j-1]-self.c[j+1])/FLOAT(2*(j-1),REAL32);
  END;
  tmp.c[nn]:=(self.c[nn-1]-0.0)/FLOAT(2*(nn-1),REAL32);
  RETURN tmp;
END ChebyIntegral;
(*--------------------------*)
PROCEDURE ChebyEval(self:ChebyApprox;
                    x:REAL32;                         (*a<x<b*)
                    m:CARDINAL):REAL32 RAISES {Error}=(*return f(x)*)
CONST ftn = Module & "ChebyEval";
VAR
  n1:=0; nn:=m;
  dj:=0.0; djp1:=0.0; djp2:=0.0; save:=0.0;
BEGIN
  IF m>LAST(self.c^) THEN
    RAISE Error(Err.bad_size);
  END;
  IF x<-1.0 OR x > 1.0 THEN
    (*need -1<x<+1*)
    RAISE Error(Err.out_of_range);
  END;  
  FOR j:=m-1 TO 1 BY -1 DO
    dj:=2.0*x*djp1-djp2+self.c[j];
    djp2:=djp1;
    djp1:=dj;  
  END;
  RETURN x*djp1-djp2+0.5*self.c[0];
    
END ChebyEval;
(*==========================*)
BEGIN
END ch05_eval.
