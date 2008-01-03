GENERIC INTERFACE RefinableSmooth(R, V, M, Eigen, S);

IMPORT Arithmetic;

PROCEDURE SobolevNonSmooth (x: S.T; ): R.T RAISES {Arithmetic.Error};
(* the fractional Sobolev smoothness for refinable functions with masks
   that do not contain smoothness factors 1/2*(1,1); the mask sum must be 1 *)

PROCEDURE Eigenvalues (mask: S.T; ): Eigen.EV RAISES {Arithmetic.Error};

PROCEDURE SpectralRadius (x: S.T; ): R.T RAISES {Arithmetic.Error};
(* spectral radius of the transition matrix *)

PROCEDURE ComputeSSE (READONLY x: ARRAY [0 .. 2] OF R.T; ): R.T;
PROCEDURE ComputeDSSE (READONLY x: ARRAY [0 .. 2] OF R.T; ): V.T;
PROCEDURE ComputeDDSSE (READONLY x: ARRAY [0 .. 2] OF R.T; ): M.T;
PROCEDURE SquareSmoothEstimate (x: S.T; ): R.T;
(* Compute the Square smoothness estimate and its derivatives.  See Henning
   Thielemann: "Bounds for smoothness of refinable functions" *)

PROCEDURE BSpline (x: S.T; ): R.T RAISES {Arithmetic.Error};
(* distances of the transition eigenvalues to those of b-spline mask,
   namely 1/2, 1/4, 1/8, ... *)

PROCEDURE Binomial (x: S.T; ): R.T;
(* negative scalar product with the b-spline mask, the more x is similar to
   a b-spline mask the more negative (i.e.  smaller) the computed value *)

PROCEDURE Frobenius (x: S.T; ): R.T;
(* Frobenius norm of the transition matrix *)

PROCEDURE SimplifiedFrobenius (x: S.T; ): R.T;
(* Square of the Euclidean norm of the autocorrelated 'x'.  This was
   derived as an approximation to the Frobenius norm. *)

PROCEDURE SumNorm (x: S.T; ): R.T;
(* Sum norm of the autocorrelated 'x', which is equivalent to the sum norm
   of the transition matrix. *)

END RefinableSmooth.
