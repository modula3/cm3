GENERIC INTERFACE MatrixTrans(M,R);
(*Copyright (c) 1996, m3na project

Abstract: Matrix math
*)

FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE
  T = M.T;

PROCEDURE Norm1(x:T):R.T;                      (*Column sum norm*)
PROCEDURE Norm2(x:T):R.T RAISES {Error};       (*Spectral norm*)
PROCEDURE NormInf(x:T):R.T;                    (*Row sum norm*)

(*==========================*)
END MatrixTrans.
