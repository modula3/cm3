GENERIC MODULE VectorTrans(R, RT, CT);
(*Copyright (c) 1996, m3na project *)

<*UNUSED*>
CONST Module = "VectorTrans.";

(*-----------------*)
PROCEDURE Norm1 (x: T): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO sum := R.Add(sum, CT.Abs(x[i])); END;
    RETURN sum;
  END Norm1;

(*-----------------*)
PROCEDURE Norm2 (x: T): R.T =
  VAR sum := R.Zero;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      sum := R.Add(sum, CT.AbsSqr(x[i]));
    END;
    RETURN RT.SqRt(sum);
  END Norm2;

(*-----------------*)
PROCEDURE NormInf (x: T): R.T =
  VAR max := R.Zero;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      VAR abs := CT.Abs(x[i]);
      BEGIN
        IF R.Compare(max, abs) < 0 THEN max := abs; END;
      END;
    END;
    RETURN max;
  END NormInf;


(*-----------------*)
BEGIN
END VectorTrans.
