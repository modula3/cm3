GENERIC INTERFACE VectorFast(R);
(*Copyright (c) 1996, m3na project

Abstract: Vector math

2/17/96  Harry George    Convert from Objects to ADT's
*)
FROM xUtils IMPORT Error;
(*==========================*)
TYPE
  (*text form: "V6{a0,a1,a2,a3,a4,a5}"*)
  T = REF ARRAY OF R.T;

PROCEDURE New(n:CARDINAL):T; (*make new vector with n components T*)
PROCEDURE Copy(x:T):T;

(*
PROCEDURE Zero(x:T);                   (*set to zero*)
    (*NOTE: you should make unit vectors as needed*)
*)

PROCEDURE Add(x,y:T):T RAISES {Error};   (*x+y*)
PROCEDURE Sub(x,y:T):T RAISES {Error};   (*x-y*)
PROCEDURE IsZero(x:T):BOOLEAN;
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error};  (*return x=y*)

PROCEDURE Scale(x:T; y:R.T):T;            (*x:=x*factor*)
PROCEDURE Inner(x,y:T):R.T RAISES {Error};   (*<x,y>*)
(*
PROCEDURE Cross(x,y:T):T RAISES {Error}; (*x x y*)
*)
(*==========================*)
END VectorFast.
