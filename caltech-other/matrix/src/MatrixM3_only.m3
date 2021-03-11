(* $Id$ *)

MODULE MatrixM3_only EXPORTS Matrix;
IMPORT MatrixM3;
IMPORT UseFortran, Process;

PROCEDURE MulD(a, b, prod : T) RAISES { DimensionMismatch } =
  BEGIN MatrixM3.MulD(a,b,prod) END MulD;

BEGIN 
  IF UseFortran.True() THEN
    Process.Crash("FORTRANMATH not supported on this architecture."&    
                  "  Please ensure that environment variable is not defined.")
  END
END MatrixM3_only.
