GENERIC INTERFACE RootApproximation(CRt,C,RRt,R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to root finders

1/28/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error;
(*==========================*)

(*=====================*)
(* Quadratics          *)
(*=====================*)

TYPE
  RealPolynomial2    = ARRAY [0..2] OF R.T;
  ComplexPolynomial2 = ARRAY [0..2] OF C.T;
  RootArray2         = ARRAY [0..1] OF C.T;

(*------------------*)
PROCEDURE QuadraticReal   (READONLY x:RealPolynomial2;        (*coefs*)
                           ):RootArray2;
(*Given a*x^2+b*x+c=0, solve for x.*)
(*------------------*)
PROCEDURE QuadraticComplex(READONLY x:ComplexPolynomial2;     (*coefs*)
                           ):RootArray2;
(*Given a*x^2+b*x+c=0, solve for x.*)


(*==========================*)
END RootApproximation.
