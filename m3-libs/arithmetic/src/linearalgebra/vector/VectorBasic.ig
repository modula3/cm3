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
PROCEDURE Cross(x,y:T):T RAISES {Error}; (*x x y*)
*)

TYPE
  ApplyFtn  = PROCEDURE (x:R.T);
  MapFtn    = PROCEDURE (x:R.T):R.T;
  ReduceFtn = PROCEDURE (x,y:R.T):R.T;

PROCEDURE Apply(x:T;f:ApplyFtn);
PROCEDURE Map(x:T;f:MapFtn):T;
PROCEDURE Reduce(x:T;f:ReduceFtn;init:R.T):R.T;

(*==========================*)
END VectorBasic.
