GENERIC INTERFACE RefinableSmooth(R, V, M, Eigen, S);

IMPORT NADefinitions AS NA;

PROCEDURE ComputeSSE (READONLY x: ARRAY [0 .. 2] OF R.T): R.T;
PROCEDURE ComputeDSSE (READONLY x: ARRAY [0 .. 2] OF R.T): V.T;
PROCEDURE ComputeDDSSE (READONLY x: ARRAY [0 .. 2] OF R.T): M.T;
PROCEDURE SquareSmoothEstimate (x: S.T): R.T;

PROCEDURE Eigenvalues (mask: S.T): Eigen.EV RAISES {NA.Error};
PROCEDURE SpecRad (x: S.T): R.T RAISES {NA.Error};
PROCEDURE BSpline (x: S.T): R.T RAISES {NA.Error};
PROCEDURE Binomial (x: S.T): R.T;
PROCEDURE Frobenius (x: S.T): R.T;
PROCEDURE SimpFrobenius (x: S.T): R.T;
PROCEDURE SumNorm (x: S.T): R.T;

END RefinableSmooth.
