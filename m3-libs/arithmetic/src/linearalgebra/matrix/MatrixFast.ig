GENERIC INTERFACE MatrixFast(R,V,MB);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
FROM NADefinitions IMPORT Error;
(*==========================*)
(*-----------------*)
TYPE
(*
|   text form: "M2x6{
                V6{a00,a01,a02,a03,a04,a05},
|               V6{a10,a11,a12,a13,a14,a15}
|               }"
*)
  TRow  = ARRAY OF R.T;
  TBody = ARRAY OF TRow;
  T     = REF TBody;

CONST
  New       = MB.New;
  FromArray = MB.FromArray;
  Copy      = MB.Copy;
  NewZero   = MB.NewZero;
  NewOne    = MB.NewOne;
  Cyclic    = MB.Cyclic;

(*
PROCEDURE Zero(x:T);                              (*set to zeros*)
PROCEDURE One (x:T) RAISES {Error};               (*set to identity*)
*)

PROCEDURE Add(x,y:T):T RAISES {Error};   (*x + y*)
PROCEDURE Sub(x,y:T):T RAISES {Error};   (*x - y*)
PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error};  (*return v1=v2*)

PROCEDURE Scale(x:T; y:R.T):T;            (*x:=x*factor*)
PROCEDURE Mul(x,y:T):T RAISES {Error};   (*x * y*)
PROCEDURE MulV(A:T;b:V.T):V.T RAISES {Error};  (*A * b*)
PROCEDURE MulTV(A:T;b:V.T):V.T RAISES {Error};  (*A^T * b or b^T*A *)
PROCEDURE Transpose(x:T):T;                  (*x^T*)
CONST Adjoint = Transpose;
PROCEDURE MMA(x:T):T;  (*x*x^**)
PROCEDURE MAM(x:T):T;  (*x^**x*)

PROCEDURE Trace(x:T):R.T;    (*sum of the diagonal elements*)
(*PROCEDURE Determinant(x:T):R.T;*)

CONST
  GetRow    = MB.GetRow;
  GetColumn = MB.GetColumn;

TYPE
  ApplyFtn  = MB.ApplyFtn;
  MapFtn    = MB.MapFtn;
  ReduceFtn = MB.ReduceFtn;

CONST
  Apply         = MB.Apply;
  Map           = MB.Map;
  ReduceRows    = MB.ReduceRows;
  ReduceColumns = MB.ReduceColumns;


(*==========================*)
END MatrixFast.
