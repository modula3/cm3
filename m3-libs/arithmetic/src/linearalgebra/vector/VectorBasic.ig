GENERIC INTERFACE VectorBasic(R);
(*Copyright (c) 1996, m3na project

Abstract: Vector math

2/17/96  Harry George    Convert from Objects to ADT's
*)
FROM NADefinitions IMPORT Error;
(*==========================*)
TYPE
  (*text form: "V6{a0,a1,a2,a3,a4,a5}"*)
  TBody = ARRAY OF R.T;
  T     = REF TBody;

PROCEDURE New(n:CARDINAL):T; (*make new vector with n components T*)
PROCEDURE Copy(x:T):T;

(*
PROCEDURE Zero(x:T);                   (*set to zero*)
    (*NOTE: you should make unit vectors as needed*)
*)

PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error};  (*return x=y*)

PROCEDURE Add(x,y:T):T RAISES {Error};   (*x+y*)
PROCEDURE Sub(x,y:T):T RAISES {Error};   (*x-y*)
PROCEDURE Neg(x:T):T;    (*return -x *)

PROCEDURE Scale(x:T; y:R.T):T;            (*x:=x*factor*)
PROCEDURE Inner(x,y:T):R.T RAISES {Error};   (*<x,y>*)
(* should be generalized to finding an orthonormal basis
   of the space orthogonal to a given set of vectors
   one way to do this:
     let the matrix have size (n,m) with less columns than rows (m<n)
     clip the matrix to size (m+1,m) and create a column vector orthogonal to it
     the j-th component is computed by the determinant of the limitted matrix
     with the j-th row removed
     now iterate to the matrix of size (m+2,m+1) and so on
PROCEDURE Cross(x,y:T):T RAISES {Error}; (*x x y*)
*)
(*==========================*)
END VectorBasic.
