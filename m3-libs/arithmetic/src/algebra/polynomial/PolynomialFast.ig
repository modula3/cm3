GENERIC INTERFACE PolynomialFast(R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to Polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

FROM xUtils IMPORT Error;
(*==========================*)

TYPE
  (*interpretation is: a[0] + a[1]*x + a[2]* x^2...a[n]*x^n *)
  (*text form is: T4{a0,a1,a2,a3} *)

  TBody = ARRAY OF R.T;
  T = BRANDED "Polynomial" REF TBody;

(*
CONST
  Zero    =  TBody{R.Zero};
  One     =  TBody{R.One};
*)

VAR
  Zero    : T;
  One     : T;

PROCEDURE New(n:CARDINAL):T;    (*make a poly for a0..an*)
PROCEDURE Copy(p:T):T;       (*copy p to a New poly*)
(*
PROCEDURE Zero(p:T);          (*set p to zeros*)
PROCEDURE One (p:T);          (*set p to 1*)
*)

PROCEDURE Eval(p:T;           (*eval this polynomial*)
               x:R.T          (*at this point*)
               ):R.T;
PROCEDURE Add(p1,p2:T):T;  (*return p1+p2*)
PROCEDURE Sub(p1,p2:T):T;  (*return p1-p2*)
PROCEDURE Equal(p1,p2:T):BOOLEAN;  (*return p1=p2*)

PROCEDURE Mul(p1,p2:T):T;  (*return p1*p2*)
PROCEDURE Div(p1,p2:T):T RAISES {Error};  (*return p1/p2 if possible*)
PROCEDURE DivMod(p1,p2:T;        (*compute p1/p2 *) 
              VAR r:T):T;     (*giving quotient with remainder r*)
(*
PROCEDURE deflate(p:T;        (*divide this polynomial*)
                  c:R.T;      (* by (x-c) *)
                  VAR rem:R.T);(*leaving remainder -- possibly 0*)
*)
PROCEDURE Derive(p:T;           (*differentiate polynomial*)
                 ):T;
PROCEDURE EvalDerivate(p:T;          (*Eval this polynomial*)
                x:R.T;               (*for this argument*)
           VAR pd:ARRAY OF R.T;      (*returning p(x), p'(x)...*)
               nd:CARDINAL           (*for up to nd EvalDerivateatives*)
                ) RAISES {Error};
          (*raises:
               Err.bad_size if nd>NUMBER(pd)+1
          *)
(*==========================*)
END PolynomialFast.
