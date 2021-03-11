INTERFACE M2_F_dp;
IMPORT LRMatrix2 AS M2;

(* single-precision Fortran bindings for Matrix 2 *)

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V);
PROCEDURE MulMC(READONLY a : M2.M; READONLY b : M2.M; VAR res : M2.V);
PROCEDURE MulMVC(READONLY a : M2.M; READONLY b : M2.V; VAR res : M2.M);
PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M);

PROCEDURE IndexedDot(READONLY v : M2.V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : M2.V) : M2.Base;

PROCEDURE Delta(READONLY v : M2.V; VAR d : M2.V);

END M2_F_dp.
