GENERIC INTERFACE MatrixBasic(V,R);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
FROM xUtils IMPORT Error;
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

PROCEDURE New(m,n:CARDINAL):T; (*make New mxn matrix*)
PROCEDURE Copy(x:T):T;

PROCEDURE NewZero(m,n:CARDINAL):T;               (*create zero matrix*)
PROCEDURE NewOne (n  :CARDINAL):T;               (*create identity matrix*)

PROCEDURE Add(x,y:T):T RAISES {Error};   (*x + y*)
PROCEDURE Sub(x,y:T):T RAISES {Error};   (*x - y*)
PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error};  (*return v1=v2*)

PROCEDURE Scale(x:T; y:R.T):T;            (*x:=x*factor*)
PROCEDURE Mul(x,y:T):T RAISES {Error};   (*x * y*)
PROCEDURE MulV(A:T;b:V.T):V.T RAISES {Error};  (*A * b*)
PROCEDURE Transpose(x:T):T;                  (*x^T*)
PROCEDURE Adjungate(x:T):T;                  (*x^**)

PROCEDURE Trace(x:T):R.T;    (*sum of the diagonal elements*)
(*PROCEDURE Determinant(x:T):R.T;*)

(*==========================*)
END MatrixBasic.
