GENERIC MODULE VectorComplex(V, CV);
(* Arithmetic for Modula-3, see doc for details *)

<* UNUSED *>
CONST
  Module = "VectorComplex.";


PROCEDURE Split (READONLY x: CV.TBody; ): T =
  VAR y := T{NEW(V.T, NUMBER(x)), NEW(V.T, NUMBER(x))};
  BEGIN
    FOR k := FIRST(x) TO LAST(x) DO
      y.re[k] := x[k].re;
      y.im[k] := x[k].im;
    END;
    RETURN y;
  END Split;

PROCEDURE Merge (READONLY re, im: V.TBody; ): CV.T =
  VAR y := NEW(CV.T, NUMBER(re));
  BEGIN
    <* ASSERT NUMBER(re) = NUMBER(im) *>
    FOR k := FIRST(y^) TO LAST(y^) DO
      y[k].re := re[k];
      y[k].im := im[k];
    END;
    RETURN y;
  END Merge;


BEGIN
END VectorComplex.
