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
PROCEDURE Copy(v:T):T;

(*
PROCEDURE Zero(v:T);                   (*set to zero*)
    (*NOTE: you should make unit vectors as needed*)
*)

PROCEDURE Add(v1,v2:T):T RAISES {Error};   (*v1+v2*)
PROCEDURE Sub(v1,v2:T):T RAISES {Error};   (*v1-v2*)
PROCEDURE Equal(v1,v2:T):BOOLEAN;  (*return v1=v2*)

PROCEDURE Scale(v:T; factor:R.T);            (*v1:=v1*factor*)
PROCEDURE Inner(v1,v2:T):R.T RAISES {Error};   (*<v1,v2>*)
<*UNUSED*>
PROCEDURE Cross(v1,v2:T):T RAISES {Error}; (*v1 x v2*)       
(*==========================*)
END VectorFast.
