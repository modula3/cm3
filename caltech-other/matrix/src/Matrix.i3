(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE Matrix;
IMPORT LRVector;

(* THIS INTERFACE HAS BEEN MOSTLY SUPERSEDED BY Matrix2.ig *)

(* DO NOT USE IT FOR NEW CODE---IF SOMETHING IS MISSING FROM
   Matrix2, IT IS BETTER TO EXTEND Matrix2 INSTEAD OF LINKING
   IN STUFF FROM HERE!!!!! *)

EXCEPTION
  DimensionMismatch;
  NotSquare;
  Singular(CARDINAL);  (* arg is index of bad func. *)

TYPE 
  S = ARRAY OF ARRAY OF LONGREAL;
  T = REF S;
  Vector = LRVector.T;
  Dim = RECORD
    rows,cols : INTEGER;
  END;
    

(* make matrix from vector *)
PROCEDURE ColVector(vector : Vector) : T;
PROCEDURE RowVector(vector : Vector) : T;

(* standard matrix ops *)
PROCEDURE Add(a,b : T): T            RAISES { DimensionMismatch };
PROCEDURE Sub(a,b : T): T            RAISES { DimensionMismatch };
PROCEDURE Mul(a,b : T): T            RAISES { DimensionMismatch };

PROCEDURE MulD(a,b,c : T)            RAISES { DimensionMismatch };
  (* returns ab in c *)

PROCEDURE MulTranspose(a,b : T): T            RAISES { DimensionMismatch };
  (* returns aTb *)

PROCEDURE MulTransposeD(a,b,c : T)            RAISES { DimensionMismatch };
  (* returns aTb in c *)

PROCEDURE Scale(a : LONGREAL; m : T) : T;
PROCEDURE Det(a : T): LONGREAL       RAISES { NotSquare };
PROCEDURE Trace(a : T): LONGREAL       RAISES { NotSquare };

(* return a unit matrix of the specified size *)
PROCEDURE Unit(dim : Dim) : T RAISES { NotSquare } ;
(* a zero matrix *)
PROCEDURE Zero(dim : Dim) : T;
(* just a matrix (uninitialized) *)
PROCEDURE New(dim : Dim) : T;

(* get dimensions of existing matrix *)
(* if rows is zero, cols is meaningless *)
PROCEDURE GetDim(m : T) : Dim;

(* format a matrix for printing *)
PROCEDURE Format(m : T) : TEXT;
PROCEDURE FormatVector(v : Vector) : TEXT;

PROCEDURE Transpose(a : T): T;

(* Decompose an LU matrix *)
PROCEDURE U(m : T) : T;
PROCEDURE L(m : T) : T;

(* Zap small entries *)
PROCEDURE Zap(m : T; threshold : LONGREAL ) : T;

(* extract row, col *)
PROCEDURE ExtractRowAsVectorD(m : T; r : CARDINAL; res : Vector);
PROCEDURE ExtractRowAsVector(m : T; r : CARDINAL) : Vector;
PROCEDURE ExtractColAsVector(m : T; c : CARDINAL) : Vector;

PROCEDURE ExtractRow(m : T; r : CARDINAL) : T;
PROCEDURE ExtractCol(m : T; c : CARDINAL) : T;

PROCEDURE SetCol(m : T; c : CARDINAL; col : Vector);

(* mean and variance of all the matrix elements... *)
PROCEDURE Mean(m : T) : LONGREAL;
PROCEDURE MeanSq(m : T) : LONGREAL;
PROCEDURE DevSq(m : T) : LONGREAL; (* sum of deviations from mean *)
PROCEDURE SumSq(m : T) : LONGREAL;
PROCEDURE SumDiffSq(m, n : T) : LONGREAL; (* sum of sq. differences *)

PROCEDURE CopyIn(from, to : T);

PROCEDURE Measure(colVector : T; squareMatrix : T) : LONGREAL RAISES { DimensionMismatch };

(* compute colVector^T x squareMatrix x colVector *)

(* DESTRUCTIVE OPS *)

PROCEDURE AddToDiagonal(m : T; a : LONGREAL) RAISES { NotSquare };
(* add a to each diagonal element, checks that m is square *)

END Matrix.





