GENERIC INTERFACE VectorTrans(V,R);
(*Copyright (c) 1996, m3na project

Abstract: Vector math

2/17/96  Harry George    Convert from Objects to ADT's
*)
(*==========================*)

TYPE
  T = V.T;

PROCEDURE Norm1(v:T):R.T;                      (*Sum norm*)
PROCEDURE Norm2(v:T):R.T;                      (*Euclidean norm*)
PROCEDURE NormInf(v:T):R.T;                    (*Maximum norm*)

(*==========================*)
END VectorTrans.
