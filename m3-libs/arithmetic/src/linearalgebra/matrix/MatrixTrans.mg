GENERIC MODULE MatrixTrans(R, RT, CT, CVR, M, Eigen);
(*Arithmetic for Modula-3, see doc for details *)

FROM Arithmetic IMPORT Error;

<* UNUSED *>
CONST
  Module = "MatrixTrans.";

PROCEDURE Norm1 (x: T): R.T =
  VAR max := R.Zero;
  BEGIN
    FOR j := FIRST(x[0]) TO LAST(x[0]) DO
      VAR sum := R.Zero;
      BEGIN
        FOR i := FIRST(x^) TO LAST(x^) DO
          sum := R.Add(sum, CT.Abs(x[i, j]));
        END;
        max := MAX(max, sum);
      END;
    END;
    RETURN max;
  END Norm1;

PROCEDURE Norm2 (x: T): R.T RAISES {Error} =
  BEGIN
    RETURN RT.SqRt(Norm2Sqr(x));
  END Norm2;

PROCEDURE Norm2Sqr (x: T): R.T RAISES {Error} =
  VAR xax: T;
  BEGIN
    IF NUMBER(x^) < NUMBER(x[0]) THEN
      xax := M.MulMMA(x);
    ELSE
      xax := M.MulMAM(x);
    END;
    RETURN Eigen.SquareMethod(xax).value;
  END Norm2Sqr;

PROCEDURE NormFrob (x: T): R.T =
  BEGIN
    RETURN RT.SqRt(NormFrobSqr(x));
  END NormFrob;

PROCEDURE NormFrobSqr (x: T): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      sum := R.Add(sum, CVR.Inner(x[i], x[i]));
    END;
    RETURN sum;
  END NormFrobSqr;

PROCEDURE NormInf (x: T): R.T =
  VAR max := R.Zero;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      VAR sum := R.Zero;
      BEGIN
        FOR j := FIRST(x[0]) TO LAST(x[0]) DO
          sum := R.Add(sum, CT.Abs(x[i, j]));
        END;
        max := MAX(max, sum);
      END;
    END;
    RETURN max;
  END NormInf;


BEGIN
END MatrixTrans.
