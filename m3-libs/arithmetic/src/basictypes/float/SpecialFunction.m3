MODULE SpecialFunction;
(*Copyright (c) 1996, m3na project
  
Abstract: Special Functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Export to xReal64

*)
FROM xUtils IMPORT Error,Err;
FROM LongRealTrans IMPORT Zero, Half, One, Two, Tiny, Eps, Exp, Log;

CONST Module = "xSpecFtn.";
(*==========================*)
PROCEDURE factorial(n:CARDINAL):T=
CONST
  max = 34;
  cache=ARRAY [0..max] OF T{
1.0D0, 1.0D0, 2.0D0, 6.0D0, 24.0D0, 120.0D0,                      (*0-5*)     
720.0D0, 5040.0D0, 40320.0D0, 362880.0D0,                         (*6-9*)
3628800.0D0, 39916800.0D0, 479001600.0D0, 6227020800.0D0,         (*10-13*)
87178291200.0D0, 1307674368000.0D0, 20922789888000.0D0,           (*14-16*)
355687428096000.0D0, 6.402373705728D+15, 1.21645100408832D+17,    (*17-19*)
2.43290200817664D+18, 5.109094217170944D+19,1.124000727777608D+21,(*20-22*)
2.585201673888498D+22,6.204484017332394D+23,1.551121004333099D+25,(*23-25*)
4.032914611266057D+26,1.088886945041835D+28,3.048883446117139D+29,(*26-28*)
8.841761993739702D+30,2.652528598121911D+32,2.652528598121911D+32,(*29-31*)
2.631308369336935D+35,8.683317618811886D+36,2.952327990396042D+38 (*32-34*)
}; 	
BEGIN
  IF n<=max THEN
    RETURN cache[n];
  ELSE
    RETURN Exp(ln_gamma(FLOAT(n,T)+One));
  END;
END factorial;
(*--------------------*)
CONST max_factln = 70; 
VAR   factln_cache:ARRAY [2..max_factln] OF T;
PROCEDURE ln_factorial(n:CARDINAL):T=
(*returns ln(n!) as a real*)
VAR
  tmp:T;
BEGIN
  IF n<2 THEN
    RETURN Zero;
  ELSIF n>max_factln THEN
    RETURN ln_gamma(FLOAT(n,T)+One);
  END;

  (*---check the cache---*)
  tmp:=factln_cache[n];
  IF tmp=Zero THEN
    tmp:=ln_gamma(FLOAT(n,T)+One);
    factln_cache[n]:=tmp;
  END;
  RETURN tmp;  
END ln_factorial;

(*--------------------*)
PROCEDURE gamma(z:T):T=
(*returns gamma(z))*)
BEGIN
  RETURN Exp(ln_gamma(z));
END gamma;

(*--------------------*)
PROCEDURE ln_gamma(z:T):T=
(*returns ln(gamma(z))*)
TYPE coefs=ARRAY [0..N] OF T;
CONST
  N=6;
  gam=5.0d0;
  lnsqrt2pi=0.918938533204673d0;
  c=coefs{1.000000000190015d0,
          76.18009172947146d0,
          -86.50532032941677d0,
          24.01409824083091d0,
          -1.231739572450155d0,
          0.1208650973866179d-2,
          -0.5395239384953d-5};
VAR
  z1:=FLOAT(z,T)-1.0d0;
  x,tmp,series:T;       
BEGIN
  x:=z1+gam+0.5d0;
  series:=c[0]+c[1]/(z1+1.0d0)
              +c[2]/(z1+2.0D0)
              +c[3]/(z1+3.0d0)
              +c[4]/(z1+4.0d0)
              +c[5]/(z1+5.0d0)
              +c[6]/(z1+6.0d0);
  tmp:=Log(x)*(z1+0.5d0)+(-x)+lnsqrt2pi+Log(series);
  RETURN tmp;    
END ln_gamma;
(*--------------------*)
PROCEDURE binomial(n,k:CARDINAL):T RAISES {Error}=
(*returns binomial coefficient for "n over k"*)
CONST ftn = Module & "binomial";
VAR tmp:T;
BEGIN
  IF k>n THEN
    (*n must be > k*)
    RAISE Error(Err.out_of_range);
  END;
  tmp:=Exp(ln_factorial(n)-ln_factorial(k)-ln_factorial(n-k));
  RETURN tmp;
END binomial;
(*-------------------*)
PROCEDURE gamma_p(a,x:T):T RAISES {Error}=
(*returns incomplete gamma P(a,x)=gamma(a,x)/Gamma(a)*)
CONST ftn = Module & "gamma_p";
VAR
  factor:=Exp(-ln_gamma(a)-x+a*Log(x));
BEGIN
  (*---check conditions---*)
  IF a<Zero OR x<Zero THEN
    (*must have a>0 and x>0*)
    RAISE Error(Err.out_of_range);
  END;
  IF x<(a+One) THEN
    RETURN factor*gamser(a,x);
  ELSE
    RETURN One-factor*gamcf(a,x);
  END;
END gamma_p;
(*-------------------*)
PROCEDURE gamma_q(a,x:T):T RAISES {Error}=
(*returns incomplete gamma Q(a,x)=Gamma(a,x)/Gamma(a)*)
(*also, Q(a,x)=1-P(a,x) *)
CONST ftn = Module & "gamma_q";
VAR
  factor:=Exp(-ln_gamma(a)-x+a*Log(x));
BEGIN
  (*---check conditions---*)
  IF a<Zero OR x<Zero THEN
    (*must have a>0 and x>0*)
    RAISE Error(Err.out_of_range);
  END;
  IF x<(a+One) THEN
    RETURN One-factor*gamser(a,x);
  ELSE
    RETURN factor*gamcf(a,x);
  END;
END gamma_q;
(*-------------------*)
PROCEDURE gamser(a,x:T):T RAISES {Error}=
(*helper for gamma_p and gamma_q*)
(*generates gamma(a,x)/Gamma(a) via series*)
CONST
  ftn = Module & "gamser";
  MaxIter=90;
  eps=5.0D0*Eps;
VAR
  a1n,term,sum:T;
BEGIN
  (*---initialize---*)
  term:=One/a;
  sum:=term;
  a1n:=a;
  (*---iterate---*)
  FOR i:=1 TO MaxIter DO
    a1n:=a1n+One;
    term:=term*x/a1n;
    sum:=sum+term;
    IF ABS(term) < eps THEN
      RETURN sum;
    END;
  END;
  (*if we got here, we are in trouble*)
  RAISE Error(Err.not_converging);
END gamser;
(*-------------------*)
PROCEDURE gamcf(a,x:T):T RAISES {Error}=
(*helper for gamma_p and gamma_q*)
(*generates Gamma(a,x) via continued fractions*)
CONST
  ftn = Module & "gamcf";
  MaxIter=90;
  eps=5.0d0*Eps;
VAR
  f,D,C,m,xa,aj,bj,delta:T;
BEGIN
  (*---initialize for j=1---*)
  C:=x+One+a+One/Tiny;
  IF ABS(C)<Tiny THEN C:=Tiny; END;
  xa:=x+One-a;
  D:=xa;
  IF ABS(D)<Tiny THEN D:=Tiny; END;
  D:=One/D;
  f:=D;
   
  (*---iterate---*)
  m:=Zero;
  FOR j:=1 TO MaxIter DO
    m:=m+One;
    bj:=xa+Two;
    aj:=m*(a-m);
    D:=bj+aj*D;
    IF ABS(D)<Tiny THEN D:=Tiny; END;
    C:=bj+aj/C;
    IF ABS(C)<Tiny THEN C:=Tiny; END;
    D:=One/D;
    delta:=C*D;
    f:=f*delta;
    IF ABS(delta-One)< eps THEN
      RETURN f;
    END;
    m:=m+One; bj:=m*(m-a); aj:=aj+Two;
  END;
  (*if we got here, we had a problem*)
  RAISE Error(Err.not_converging);

END gamcf;
(*--------------------*)
PROCEDURE erf(x:T):T RAISES {Error}=
(*returns error function of x*)
BEGIN
  IF x<Zero THEN
    RETURN -gamma_p(Half,x*x);
  ELSE
    RETURN  gamma_p(Half,x*x);
  END;
END erf;
(*--------------------*)
PROCEDURE erfc(x:T):T RAISES {Error}=
(*returns 1-erf(x) *)
BEGIN
  IF x < Zero THEN
    RETURN Two - gamma_q(Half,x*x);
  ELSE
    RETURN gamma_q(Half,x*x);
  END;
END erfc;
(*--------------------*)
PROCEDURE beta(z,w:T):T=
(*returns gamma(z)*gamma(w)/gamma(z+w)*)
BEGIN
  RETURN Exp(ln_gamma(z)+ln_gamma(w)-ln_gamma(z+w));
END beta;
(*--------------------*)
PROCEDURE betacf(a,b,x:T):T RAISES {Error}=
(*helper for betai, returns continued fraction*)
CONST
  ftn = Module & "betacf";
  bj=One;
  eps=5.0D0*Eps;
  MaxIter=90;
VAR
  f,D,C,m,am,a2m,aj,delta:T;
BEGIN
  (*---initialize at j=2---*)
  aj:=-(a+b)*x/(a+One);
  D:=One+aj;
  C:=One;
  IF ABS(D)<Tiny THEN D:=Tiny; END;
  D:=One/D;
  f:=D;
  
  (*---iterate---*)
  m:=Zero;
  FOR j:=3 TO MaxIter DO
    m:=m+One; am:=a+m; a2m:=am+m;

    (*---a[j]=d[2m]---*)
    aj:=m*(b-m)*x/((a2m-One)*a2m);
    D:=bj+aj*D;
    IF ABS(D)<Tiny THEN D:=Tiny; END;
    C:=bj+aj/C;
    IF ABS(C)<Tiny THEN C:=Tiny; END;
    D:=One/D;
    delta:=C*D;
    f:=f*delta;
    IF ABS(delta-One)<eps THEN RETURN f; END;

    (*---a[j]=d[2m+1]---*)
    aj:=-am*(am+b)*x/(a2m*(a2m+One));
    D:=bj+aj*D;
    IF ABS(D)<Tiny THEN D:=Tiny; END;
    C:=bj+aj/C;
    IF ABS(C)<Tiny THEN C:=Tiny; END;
    D:=One/D;
    delta:=C*D;
    f:=f*delta;
    IF ABS(delta-One)<eps THEN RETURN f; END;
  END;
  (*if we got here, we had a problem*)
  RAISE Error(Err.not_converging);
END betacf;
(*--------------------*)
PROCEDURE betai(a,b,x:T):T RAISES {Error}=
(*returns incomplete beta Ix(a,b) *)
CONST ftn = Module & "betai";
VAR
  factor:T;
BEGIN
  IF    a<=Zero OR b<=Zero THEN
    (*must have a>Zero and b>Zero*)
    RAISE Error(Err.out_of_range);
  ELSIF x < Zero OR x > One THEN
    (*must have Zero < x < One*)
    RAISE Error(Err.out_of_range);
  ELSIF x = Zero OR x = One THEN
    factor:=Zero;
  ELSE
    factor:=Exp(a*Log(x)+b*Log(One-x)
             -(ln_gamma(a)+ln_gamma(b)-ln_gamma(a+b)));
  END;

  (*---check for convergence condition---*)
  IF x < (a+One)/(a+b+Two) THEN
    RETURN factor*betacf(a,b,x)/a;
  ELSE
    RETURN One-factor*betacf(a,b,One-x)/b;
  END;
END betai;
(*==========================*)
BEGIN
  FOR i:=FIRST(factln_cache) TO LAST(factln_cache) DO
    factln_cache[i]:=Zero;
  END;
END SpecialFunction.
