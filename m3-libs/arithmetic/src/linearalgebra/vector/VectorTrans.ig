GENERIC INTERFACE VectorTrans(R,V);
(*Copyright (c) 1996, m3na project

Abstract: Vector math

2/17/96  Harry George    Convert from Objects to ADT's
*)
(*==========================*)

TYPE
  T = V.T;

PROCEDURE Norm1(x:T):R.T;                      (*Sum norm*)
PROCEDURE Norm2(x:T):R.T;                      (*Euclidean norm*)
PROCEDURE NormInf(x:T):R.T;                    (*Maximum norm*)

(*==========================*)
END VectorTrans.
