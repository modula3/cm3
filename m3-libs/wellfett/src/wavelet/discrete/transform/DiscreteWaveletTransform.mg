GENERIC MODULE DiscreteWaveletTransform(S, SV, SVR, SM, FB);

IMPORT Fmt;

PROCEDURE SingleFromSignal (         x      : S.T;
                            READONLY y      : FB.TBody;
                                     scaling: S.ScalingType; ): SV.T =
  VAR z := NEW(SV.T, NUMBER(y));
  BEGIN
    FOR i := FIRST(y) TO LAST(y) DO
      z[i] := x.convolveDown(y[i], scaling);
    END;
    RETURN z;
  END SingleFromSignal;

PROCEDURE SingleToSignal (READONLY x      : SV.TBody;
                          READONLY y      : FB.TBody;
                                   scaling: S.ScalingType; ): S.T =
  VAR z := S.Zero;
  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(y),
                "The number of channels (" & Fmt.Int(NUMBER(x))
                  & ") is different from the number of filters ("
                  & Fmt.Int(NUMBER(y)) & ")." *>
    FOR i := FIRST(x) TO LAST(x) DO
      z := z.superpose(y[i].upConvolve(x[i], scaling));
    END;
    RETURN z;
  END SingleToSignal;

<* INLINE *>
PROCEDURE SingleShiftInvariantFromSignal (         x     : S.T;
                                          READONLY filter: FB.TBody; ):
  SV.T =
  BEGIN
    (* flip argument order *)
    RETURN SVR.Scale(filter, x);
  END SingleShiftInvariantFromSignal;

PROCEDURE FromSignal (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ):
  T =
  VAR z := T{low := NIL, high := NEW(SM.T, numLevels, NUMBER(y) - 1)};
  BEGIN
    FOR i := 0 TO numLevels - 1 DO
      WITH lev = SingleFromSignal(x, y, NUMBER(y))^ DO
        z.high[i] := SUBARRAY(lev, 1, NUMBER(y) - 1);
        x := lev[0];
      END;
    END;
    RETURN z;
  END FromSignal;


BEGIN
END DiscreteWaveletTransform.
