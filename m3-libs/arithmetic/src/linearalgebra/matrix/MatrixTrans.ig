GENERIC INTERFACE MatrixTrans(R, M);
(*Copyright (c) 1996, m3na project

   Abstract: Matrix math *)

FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE T = M.T;

PROCEDURE Norm1 (x: T): R.T;     (*Column sum norm*)
PROCEDURE Norm2 (x: T): R.T RAISES {Error}; (*Spectral norm*)
PROCEDURE Norm2Sqr (x: T): R.T
  RAISES {Error};                (*Square of the spectral norm*)
PROCEDURE NormFrob (x: T): R.T;  (*Frobenius norm*)
PROCEDURE NormFrobSqr (x: T): R.T; (*Square of the Frobenius norm*)
PROCEDURE NormInf (x: T): R.T;   (*Row sum norm*)

(*==========================*)
END MatrixTrans.
