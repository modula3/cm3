GENERIC MODULE BSplineWavelet(R, S, IntPow, DB);

CONST Half = FLOAT(0.50D0, R.T);

PROCEDURE GeneratorMask (n: CARDINAL; base: [1 .. LAST(CARDINAL)]): S.T =
  VAR
    maskArr := NEW(REF ARRAY OF R.T, base);
    coef    := R.One / R.FromInteger(base);
  BEGIN
    FOR j := 0 TO LAST(maskArr^) DO maskArr[j] := coef; END;
    RETURN IntPow.MulPower(S.One, NEW(S.T).fromArray(maskArr^), n);
  END GeneratorMask;

PROCEDURE WaveletMask (n, m: CARDINAL): S.T RAISES {DifferentParity} =
  BEGIN
    IF (m + n) MOD 2 # 0 THEN RAISE DifferentParity; END;
    VAR mask := DB.FilterPureAbsSqr((m + n) DIV 2);
    BEGIN
      mask := IntPow.MulPower(
                mask, NEW(S.T).fromArray(ARRAY OF R.T{Half, Half}), m);
      RETURN mask.translate(1).alternate();
    END;
  END WaveletMask;

BEGIN
END BSplineWavelet.
