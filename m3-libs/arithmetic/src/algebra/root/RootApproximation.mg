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
PROCEDURE QuadraticReal   (READONLY x:RealPolynomial2;        (*coefs*)
                           ):RootArray2=
(*Given a*t^2+b*t+c=0, solve for t.*)
VAR
  a:=x[2];
  b:=x[1];
  c:=x[0];
  disc,q,q1,q2:R.T;
BEGIN
  disc:=b*b-FLOAT(4.0,R.T)*a*c;
  IF disc<=R.Zero THEN
    q1:=-b/a*RT.Half;
    q2:=RT.SqRt(-disc)/a*RT.Half;
    RETURN RootArray2{
             C.T{re:=q1,im:= q2},
             C.T{re:=q1,im:=-q2}
           };
  ELSE
    (*avoid cancelation*)
    IF b<R.Zero THEN
      q:=RT.Half*(-b+RT.SqRt(disc));
    ELSE
      q:=RT.Half*(-b-RT.SqRt(disc));
    END;
    (*fails because Sgn(0)=0 :
    q:=-RT.Half*(b+RT.Sgn(b)*RT.SqRt(disc));
    *)
    q1:=q/a;
    q2:=c/q;
    RETURN RootArray2{
             C.T{re:=q1,im:=R.Zero},
             C.T{re:=q2,im:=R.Zero}
           };
  END;
END QuadraticReal;
(*---------------------*)
PROCEDURE QuadraticComplex(READONLY x:ComplexPolynomial2;     (*coefs*)
                           ):RootArray2=
(*Given a*t^2+b*t+c=0, solve for t.*)
CONST
  c4=FLOAT(4.0,R.T);
VAR
  a:=x[2];
  b:=x[1];
  c:=x[0];
  disc,disc_sqrt,q:C.T;
BEGIN
  disc:=C.Sub(C.Mul(b,b),C.Scale(C.Mul(a,c),c4));
  disc_sqrt:=CT.SqRt(disc);

  (*---set sign of sqrt via NR92 eqn 5.6.6---*)
  IF C.Mul(C.Conj(b),disc_sqrt).re>R.Zero THEN
    disc_sqrt:=C.Neg(disc_sqrt);
  END;

  (*---calculate per NR92 eqn 5.6.4, 5.6.5.---*)
  q:=C.Scale(C.Sub(disc_sqrt,b),RT.Half);
  RETURN RootArray2{C.Div(q,a),C.Div(c,q)};
END QuadraticComplex;

(*==========================*)
BEGIN
END RootApproximation.
