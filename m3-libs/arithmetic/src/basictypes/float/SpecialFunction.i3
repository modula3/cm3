INTERFACE SpecialFunction;
(*The interface is Public Domain.
  The supporting implementations are copyrighted,
  but may be used free of charge so long as
  appropriate credit is given.  

  WARNING:  USE AT YOUR OWN RISK.
  The authors accept no responsibility for
  the accuracy, appropriateness or fitness for
  use of any of this material.

Abstract:  This is a Modula-3 rendition of a collection
           of numerical analysis routines.         

12/13/95   Harry George      Initial version
1/22/96    Harry George      Change to m3na project
2/17/96    Harry George      Convert to separate Real* modules
*)
FROM xUtils IMPORT Error;

(*=================*)
TYPE
  T = LONGREAL;  (*IEEE 64-bit real*)

(*============================*)
(* Really special functions   *)
(*============================*)
PROCEDURE sgn(a:T):T;
(*IF a >=0.0 THEN RETURN 1.0 ELSE RETURN -1.0; END:*)

PROCEDURE factorial(n:CARDINAL):T;
(*return n! as a real*)

PROCEDURE ln_factorial(n:CARDINAL):T;
(*returns ln(n!) as a real*)

PROCEDURE gamma(z:T):T;
(*returns gamma(z)*)

PROCEDURE ln_gamma(z:T):T;
(*returns ln(gamma(z))*)


PROCEDURE binomial(n,k:CARDINAL):T RAISES {Error};
(*returns binomial coefficient for "n over k"*)

PROCEDURE gamma_p(a,x:T):T RAISES {Error};
(*returns incomplete gamma P(a,x)=gamma(a,x)/Gamma(a)*)

PROCEDURE gamma_q(a,x:T):T RAISES {Error};
(*returns incomplete gamma Q(a,x)=Gamma(a,x)/Gamma(a)*)
(*also, Q(a,x)=1-P(a,x) *)

(*Notes for in-lines:
|1. Cumulative Poisson Probability:
|   Px(<k)=probability that the number of events will be
|   between 0 and k-1 inclusive, given mean=x.
|     Px(<k)=gamma_q(k,x)
|2. Chi-Square Probability:
|   P(X2|df)=probability that observed chi-square should be
|   less than X2, given df degrees of freedom. 
|     P(X2|df)=gamma_p(df/2.0,X2/2.0); P(0|df)=0, P(inf|df)=1
|   Complementary form:
|     Q(X2|df)=gamma_q(df/2.0,X2/2.0); Q(0|df)=1, Q(inf|df)=0
*)

PROCEDURE erf(x:T):T RAISES {Error};
(*returns error function of x*)

PROCEDURE erfc(x:T):T RAISES {Error};
(*returns 1-erf(x) *)

PROCEDURE beta(z,w:T):T;
(*returns gamma(z)*gamma(w)/gamma(z+w)*)

PROCEDURE betai(a,b,x:T):T RAISES {Error};
(*returns incomplete beta Ix(a,b) *)
(*Notes for in-lines:
|1. Student's t-test distribution for df degrees of freedom is
|     A(t|df) = 1.0-betai(df/2,1/2,df/(df+t^2))
|   In other words, big A means t should probably be smaller
|2. F-test distribution for df1 and df2 degrees of freedom is
|     A(F|df1,df2) = betai(df1/2,df2/2,df2/(df2+df1*F))
|    
|3. Cumulative binomial probability for event which has
|   probability p of occurring in each trial,
|   having the event occur k or moe times in n trials is
|     P(= betai(k,n-k+1,p)
*)


(*========================*)
END SpecialFunction.
