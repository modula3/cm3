GENERIC INTERFACE MatrixBasic(R, V);
(**Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
**)
FROM NADefinitions IMPORT Error;
(*==========================*)
(*-----------------*)

CONST Brand = R.Brand & "Matrix";

TYPE
  (**
     text form: "M2x6{
                 V6{a00,a01,a02,a03,a04,a05},
                 V6{a10,a11,a12,a13,a14,a15}
                 }"
  **)
  TRow = ARRAY OF R.T;
  TBody = ARRAY OF TRow;
  T = REF TBody;

  TMRow = ARRAY OF T;
  TMBody = ARRAY OF TMRow;       (*matrix of matrices*)

PROCEDURE New (m, n: CARDINAL): T; (*make New mxn matrix*)
PROCEDURE FromArray (READONLY x: TBody): T;
PROCEDURE RowFromArray (READONLY x: V.TBody): T;
PROCEDURE ColumnFromArray (READONLY x: V.TBody): T;
PROCEDURE DiagonalFromArray (READONLY x: V.TBody): T;
<*INLINE*>
PROCEDURE RowFromVector (x: V.T): T;
<*INLINE*>
PROCEDURE ColumnFromVector (x: V.T): T;
<*INLINE*>
PROCEDURE DiagonalFromVector (x: V.T): T;
PROCEDURE FromMatrixArray (READONLY x: TMBody): T RAISES {Error};
PROCEDURE FromScalar (x: R.T): T;
PROCEDURE Copy (x: T): T;

PROCEDURE NewZero (m, n: CARDINAL): T; (*create zero matrix*)
PROCEDURE NewOne (n: CARDINAL): T; (*create identity matrix*)
PROCEDURE Cyclic (x: V.T; size: CARDINAL; shift: INTEGER := 1):
  T;                             (*each row is 'x' shifted by 'shift' to
                                    the right compared to the row above*)

PROCEDURE Add (x, y: T): T RAISES {Error}; (*x + y*)
PROCEDURE Sub (x, y: T): T RAISES {Error}; (*x - y*)
PROCEDURE IsZero (x: T): BOOLEAN;
PROCEDURE Equal (x, y: T): BOOLEAN RAISES {Error}; (*return v1=v2*)

PROCEDURE Scale (x: T; y: R.T): T; (*x:=x*factor*)
PROCEDURE Mul (x, y: T): T RAISES {Error}; (*x * y*)
PROCEDURE MulV (A: T; b: V.T): V.T RAISES {Error}; (*A * b*)
PROCEDURE MulTV (A: T; b: V.T): V.T RAISES {Error}; (*A^T * b or b^T*A *)
PROCEDURE Transpose (x: T): T;   (*x^T*)
PROCEDURE Adjoint (x: T): T;     (*x^**)
PROCEDURE MMA (x: T): T;         (*x*x^**)
PROCEDURE MAM (x: T): T;         (*x^**x*)

PROCEDURE Trace (x: T): R.T;     (*sum of the diagonal elements*)
(*PROCEDURE Determinant(x:T):R.T;*)

PROCEDURE GetRow (x: T; k: CARDINAL): V.T;
PROCEDURE GetColumn (x: T; k: CARDINAL): V.T;

TYPE
  ApplyFtn = PROCEDURE (x: R.T);
  MapFtn = PROCEDURE (x: R.T): R.T;
  ReduceFtn = PROCEDURE (x, y: R.T): R.T;

PROCEDURE Apply (x: T; f: ApplyFtn);
PROCEDURE Map (x: T; f: MapFtn): T;
PROCEDURE ReduceRows (x: T; f: ReduceFtn; READONLY init: V.TBody): V.T;
PROCEDURE ReduceColumns (x: T; f: ReduceFtn; READONLY init: V.TBody): V.T;

(*==========================*)
END MatrixBasic.
