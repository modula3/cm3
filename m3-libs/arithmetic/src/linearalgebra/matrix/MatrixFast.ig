GENERIC INTERFACE MatrixFast(R, V);
(*Copyright (c) 1996, m3na project*)
FROM NADefinitions IMPORT Error;
(*==========================*)
TYPE
  TRow = ARRAY OF R.T;
  TBody = ARRAY OF TRow;
  T = REF TBody;

PROCEDURE Add (x, y: T): T RAISES {Error}; (*x + y*)
PROCEDURE Sub (x, y: T): T RAISES {Error}; (*x - y*)
PROCEDURE IsZero (x: T): BOOLEAN;
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Error}; (*return v1=v2*)

PROCEDURE Scale (x: T; y: R.T): T; (*x:=x*factor*)
PROCEDURE Mul (x, y: T): T RAISES {Error}; (*x * y*)
PROCEDURE MulV (x: T; y: V.T): V.T RAISES {Error}; (*A * b*)
PROCEDURE MulTV (x: T; y: V.T): V.T RAISES {Error}; (*A^T * b or b^T*A *)
PROCEDURE Transpose (x: T): T;   (*x^T*)
CONST Adjoint = Transpose;
PROCEDURE MulMMA (x: T): T;      (*x*x^**)
PROCEDURE MulMAM (x: T): T;      (*x^**x*)

PROCEDURE Trace (x: T): R.T;     (*sum of the diagonal elements*)
(*PROCEDURE Determinant(x:T):R.T;*)

(*==========================*)
END MatrixFast.
