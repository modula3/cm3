GENERIC INTERFACE Functional(R, V, FD);
(* Copyright (c) 1996, m3na project

   Abstract:

   <describe> *)

IMPORT NADefinitions AS NA;

(*==========================*)

TYPE
  Func = PROCEDURE (x: V.T): R.T;
  (*evaluate functional for vector x*)
  FuncDeriv2 = PROCEDURE (x: V.T): FD.T;
(*evaluate functional, its first derivative (gradient) and its second
   derivative (Jacobian of the gradient)*)


PROCEDURE EvalCentralDiff2 (f: Func; x, dx: V.T): FD.T;
(*compute central differences, this can be used as approximation for
   derivatives; accesses 'evaluate' method*)

PROCEDURE FindStationaryPoint (f      : FuncDeriv2;
                               x      : V.T;
                               tol    : R.T;
                               maxiter: CARDINAL    ): V.T
  RAISES {NA.Error};
(*find candidates for extrema and saddle points by Newton's iteration*)

(*==========================*)
END Functional.
