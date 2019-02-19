(* $Id$ *)

MODULE MatrixM3;
FROM Matrix IMPORT T, GetDim, DimensionMismatch;

PROCEDURE MulD(a,b, prod : T)            RAISES { DimensionMismatch } =
  VAR
    aDim := GetDim(a);
    bDim := GetDim(b);
  BEGIN
    IF GetDim(a).cols # GetDim(b).rows THEN RAISE DimensionMismatch END;

    IF GetDim(prod).rows # GetDim(a).rows OR
      GetDim(prod).cols # GetDim(b).cols THEN
      RAISE DimensionMismatch
    END;

    FOR row:= 0 TO aDim.rows - 1 DO
      FOR col:= 0 TO bDim.cols - 1 DO
        VAR
          element := 0.0d0;
        BEGIN
          FOR term := 0 TO aDim.cols - 1 DO
            element := element + a[row,term] * b[term,col];
          END;
          prod[row,col] := element;
        END;
      END;
    END
  END MulD;

BEGIN END MatrixM3.
