GENERIC MODULE BSplineWavelet(R, S, IntPow, DB);

PROCEDURE GeneratorMask (n: CARDINAL; base: [1 .. LAST(CARDINAL)]): S.T =
  VAR
    coef := R.One / R.FromInteger(base);
    atom := NEW(S.T).fromVector(V.NewUniform(coef));
  BEGIN
    RETURN IntPow.MulPower(S.One, atom, n);
  END GeneratorMask;

PROCEDURE WaveletMask (n, m: CARDINAL): S.T RAISES {DifferentParity} =
  VAR mask := WaveletMaskNoVan(n, m);
  BEGIN
    mask := IntPow.MulPower(
              mask, NEW(S.T).fromArray(ARRAY OF R.T{R.Half, -R.Half}), m);
    RETURN mask.translate(1 - (m + n) DIV 2);
  END WaveletMask;

PROCEDURE WaveletMaskNoVan (n, m: CARDINAL): S.T RAISES {DifferentParity} =
  BEGIN
    IF (m + n) MOD 2 # 0 THEN RAISE DifferentParity; END;
    RETURN DB.FilterPureAbsSqr((m + n) DIV 2).alternate();
  END WaveletMaskNoVan;

BEGIN
END BSplineWavelet.
