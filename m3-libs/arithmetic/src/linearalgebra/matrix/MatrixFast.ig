GENERIC INTERFACE MatrixFast(V,R);
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
PROCEDURE Copy(x:T):T;

PROCEDURE NewZero(m,n:CARDINAL):T;               (*create zero matrix*)
PROCEDURE NewOne (n  :CARDINAL):T;               (*create identity matrix*)

(*
PROCEDURE Zero(x:T);                              (*set to zeros*)
PROCEDURE One (x:T) RAISES {Error};               (*set to identity*)
*)

PROCEDURE Add(x,y:T):T RAISES {Error};   (*x + y*)
PROCEDURE Sub(x,y:T):T RAISES {Error};   (*x - y*)
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error};  (*return v1=v2*)

PROCEDURE Mul(x,y:T):T RAISES {Error};   (*x * y*)
PROCEDURE MulV(A:T;b:V.T):V.T RAISES {Error};  (*A * b*)
PROCEDURE Transpose(x:T):T;                  (*x^T*)
CONST Adjungate = Transpose;
(*==========================*)
END MatrixFast.
