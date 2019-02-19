(* $Id$ *)

GENERIC INTERFACE Matrix2(Elem);
IMPORT Matrix;

CONST Brand = "Matrix2(" & Elem.Brand & ")";

TYPE Base = Elem.T;

TYPE M = ARRAY OF ARRAY OF Elem.T;
TYPE V = ARRAY OF          Elem.T;

CONST Format = Elem.Format;
CONST Rand   = Elem.Rand;

PROCEDURE Copy(READONLY a : M; VAR b : M);
PROCEDURE Zero(VAR a : M);
PROCEDURE MakeUnit(VAR a : M);

PROCEDURE MulSM(s : Base; READONLY a : M; VAR res : M);
PROCEDURE MulSV(s : Base; READONLY a : V; VAR res : V);
PROCEDURE AddSV(s : Base; READONLY a : V; VAR res : V); (* add s to each elem *)
PROCEDURE MulMM(READONLY a, b : M; VAR res : M);

PROCEDURE MulMV(READONLY a : M; READONLY v : V; VAR res : V);

PROCEDURE MulMC(READONLY a : M; READONLY b : M; VAR res : V);
  (* multiplies matrix a by column 0 of single-column matrix b and 
     puts result in res *)

PROCEDURE MulMC2(READONLY a : M; READONLY b : M; VAR res, res2 : V);
  (* multiplies matrix a by column 0 of single-column matrix b and 
     puts results in res and res2
     res2 contains sum of squares *)

PROCEDURE MulMVC(READONLY a : M; READONLY b : V; VAR res : M);
  (* multiplies matrix a by column vector b and 
     puts result in column 0 of single-column matrix res *)

PROCEDURE MeanM(READONLY m : M) : Elem.T;
PROCEDURE MeanSqM(READONLY m : M) : Elem.T;
PROCEDURE DevSqM(READONLY m : M) : Elem.T; (* sum of deviations from mean *)
PROCEDURE SumM(READONLY m : M) : Elem.T;
PROCEDURE SumSqM(READONLY m : M) : Elem.T;
PROCEDURE SumDiffSqM(READONLY m, n : M) : Elem.T; (* sum of sq. differences *)

PROCEDURE MeanV(READONLY v : V) : Elem.T;
PROCEDURE MeanSqV(READONLY v : V) : Elem.T;
PROCEDURE DevSqV(READONLY v : V) : Elem.T; (* sum of deviations from mean *)
PROCEDURE SumV(READONLY v : V) : Elem.T;
PROCEDURE SumSqV(READONLY v : V) : Elem.T;
PROCEDURE SumDiffSqV(READONLY m, n : V) : Elem.T; (* sum of sq. differences *)

PROCEDURE NewM(dims : Matrix.Dim) : REF M;

PROCEDURE GetDim(READONLY m : M) : Dim;

PROCEDURE NewV(elems : CARDINAL) : REF V;

TYPE
  Dim = RECORD
    rows,cols : INTEGER;
  END;

PROCEDURE FormatM(READONLY m : M) : TEXT;
PROCEDURE FormatV(READONLY v : V) : TEXT;

PROCEDURE IndexedDot(READONLY v : V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : V) : Elem.T;
  (* FOR c := 0 TO n DO 
       sum := sum + v[idx[c]]*w[c]
     END *)

PROCEDURE Delta(READONLY v : V; VAR d : V);
  (* produces the v[i+1]-v[i] in d[i] forall i *)

(**********************************************************************)

PROCEDURE MulTransposeMM(READONLY a,b : M; VAR c : M);
  (* returns aTb in c *)

PROCEDURE AddToDiagonal(VAR m : M; a : Base);
  (* increase diagonal elements in-place *)

PROCEDURE Det(READONLY m : M) : Base;

PROCEDURE ExtractRowAsVector(READONLY m : M; r : CARDINAL; VAR res : V);
PROCEDURE ExtractColAsVector(READONLY m : M; c : CARDINAL; VAR res : V);

PROCEDURE SetCol(VAR m : M; c : CARDINAL; READONLY col : V);

PROCEDURE SubV(READONLY a, b : V; VAR c : V);
PROCEDURE AddV(READONLY a, b : V; VAR c : V);

PROCEDURE SubM(READONLY a, b : M; VAR c : M);
PROCEDURE AddM(READONLY a, b : M; VAR c : M);

PROCEDURE LinearCombination(aw : Elem.T; READONLY a : M;
                            bw : Elem.T; READONLY b : M;
                            VAR c : M);
  (* c <- aw * a + bw * b *)

PROCEDURE LinearCombinationV(aw : Elem.T; READONLY a : V;
                             bw : Elem.T; READONLY b : V;
                             VAR c : V);
  (* c <- aw * a + bw * b *)

END Matrix2.

