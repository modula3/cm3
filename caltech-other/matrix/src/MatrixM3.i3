(* $Id$ *)

INTERFACE MatrixM3;
FROM Matrix IMPORT T, DimensionMismatch;

PROCEDURE MulD(a,b, prod : T)            RAISES { DimensionMismatch };

END MatrixM3.
