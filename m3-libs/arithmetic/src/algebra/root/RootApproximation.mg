GENERIC MODULE RootApproximation(CRt,CT,C,RRt,RT,R);
(*Copyright (c) 1996, Harry George

Abstract: Implementation of Root.

1/28/96    Harry George    Initial version, from earlier work
2/17/96    Harry George    Converted from OO to ADT format
*)
FROM xUtils IMPORT Error,Err;

CONST Module = "RootApproximation.";
(*==========================*)
(*=====================*)
(* Quadratics          *)
(*=====================*)

(*---------------------*)
PROCEDURE QuadraticReal (a,b,c:R.T;             (*coefs*)
                    VAR alpha,beta:C.T; (*alpha +/- beta format*)
                    VAR x1,x2:C.T)=     (*root format*)
(*Given a*x^2+b*x+c=0, solve for x.*)
VAR
  disc,q,q1,q2:R.T;
BEGIN
  disc:=b*b-FLOAT(4.0,R.T)*a*c;
  IF disc<R.Zero THEN
    (*use the complex version*)
    QuadraticComplex(
             C.T{re:=a,im:=R.Zero},
             C.T{re:=b,im:=R.Zero},
             C.T{re:=c,im:=R.Zero},
             alpha,beta,x1,x2);
    RETURN;
  END;

  q:=-RT.Half*(b+RT.Sgn(b)*RT.SqRt(disc));
  q1:=q/a;
  q2:=c/q;
  x1   :=C.T{re:=q1,im:=R.Zero};
  x2   :=C.T{re:=q2,im:=R.Zero};
  alpha:=C.T{re:=RT.Half*(q1+q2),im:=R.Zero};
  beta :=C.T{re:=RT.Half*(q1-q2),im:=R.Zero};
END QuadraticReal;
(*---------------------*)
PROCEDURE QuadraticComplex (a,b,c:C.T;          (*coefs*)
                    VAR alpha,beta:C.T; (*alpha +/- beta format*)
                    VAR x1,x2:C.T)=     (*results*)
(*Given a*x^2+b*x+c=0, solve for x.*)
CONST
  c4 =C.T{re:=FLOAT( 4.0,R.T),im:=R.Zero};
  c05=C.T{re:=FLOAT(-0.5,R.T),im:=R.Zero};
VAR
  disc,disc_sqrt,q:C.T;
BEGIN
  disc:=C.Sub(C.Mul(b,b),C.Mul(c4,C.Mul(a,c)));
  disc_sqrt:=CT.SqRt(disc);

  (*---set sign of sqrt via NR92 eqn 5.6.6---*)
  IF C.Mul(C.Conj(b),disc_sqrt).re<R.Zero THEN
    disc_sqrt.re:=-disc_sqrt.re;
  END;

  (*---calculate per NR92 eqn 5.6.4, 5.6.5.---*)
  q:=C.Mul(c05,C.Add(b,disc_sqrt));
  x1:=C.Div(q,a);
  x2:=C.Div(c,q);
  alpha:=C.Scale(C.Add(x1,x2),RT.Half);
  beta :=C.Scale(C.Sub(x1,x2),RT.Half);
END QuadraticComplex;

(*==========================*)
BEGIN
END RootApproximation.
