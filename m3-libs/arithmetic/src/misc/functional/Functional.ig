GENERIC INTERFACE Functional(R, V, M, FD);
(* Copyright (c) 1996, m3na project

   Abstract:

   <describe> *)

(*==========================*)

TYPE
  T <: Public;
  Public =
    OBJECT
    METHODS
      (*abstract declarations that must be implemented by sub-classes*)
      evaluate (x: V.T): R.T;
      (*evaluate functional for vector x*)
      evaluateDeriv2 (x: V.T): FD.T;
      (*evaluate functional, its first derivative (gradient) and its second
         derivative (Jacobian of the gradient)*)

      (*these methods are implemented and access the abstract methods*)
      evaluateCentralDiff2 (x, dx: V.T): FD.T;
      (*compute central differences, this can be used as approximation for
         derivatives; accesses 'evaluate' method*)
      findStationaryPoint (x: V.T): FD.T;
      (*find candidates for extrema and saddle points by Newton's
         iteration*)
    END;

(*==========================*)
END Functional.
