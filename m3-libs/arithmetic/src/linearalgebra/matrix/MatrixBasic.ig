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
  T  = REF ARRAY OF ARRAY OF R.T;

PROCEDURE New(m,n:CARDINAL):T; (*make New mxn matrix*)
PROCEDURE Copy(mat:T):T;

(*
PROCEDURE Zero(mat:T);                              (*set to zeros*)
PROCEDURE One (mat:T) RAISES {Error};               (*set to identity*)
*)

PROCEDURE Add(mat1,mat2:T):T RAISES {Error};   (*mat1 + mat2*)
PROCEDURE Sub(mat1,mat2:T):T RAISES {Error};   (*mat1 - mat2*)
PROCEDURE Equal(mat1,mat2:T):BOOLEAN RAISES {Error};  (*return v1=v2*)

PROCEDURE Mul(mat1,mat2:T):T RAISES {Error};   (*mat1 * mat2*)
PROCEDURE MulV(A:T;b:V.T):V.T RAISES {Error};  (*A * b*)
PROCEDURE Transpose(mat:T):T;                  (*mat^T*)
PROCEDURE Adjungate(mat:T):T;                  (*mat^**)
(*==========================*)
END MatrixBasic.
