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
(*------------------*)
PROCEDURE QuadraticReal (a,b,c:R.T;             (*coefs*)
                    VAR alpha,beta:C.T; (*alpha +/- beta format*)
                    VAR x1,x2:C.T);     (*root format*)
(*Given a*x^2+b*x+c=0, solve for x.*)
(*------------------*)
PROCEDURE QuadraticComplex(a,b,c:C.T;          (*coefs*)
                   VAR alpha,beta:C.T; (*alpha +/- beta format*)
                   VAR x1,x2:C.T);     (*results*)
(*Given a*x^2+b*x+c=0, solve for x.*)


(*==========================*)
END RootApproximation.
