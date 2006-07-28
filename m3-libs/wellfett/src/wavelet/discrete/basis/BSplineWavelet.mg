GENERIC MODULE BSplineWavelet(R, V, S, IntPow, DB);

IMPORT Arithmetic;

PROCEDURE GeneratorMask (n: CARDINAL; base: [1 .. LAST(CARDINAL)]): S.T =
  VAR
    coef := R.One / R.FromInteger(base);
    atom := NEW(S.T).fromVector(V.NewUniform(base, coef));
  <* FATAL Arithmetic.Error *>   (* MulPower can't fail for signals *)
  BEGIN
    RETURN IntPow.MulPower(S.One, atom, n);
  END GeneratorMask;

PROCEDURE WaveletMask (n, m: CARDINAL): S.T =
  VAR
    mn2  := (m + n) DIV 2;
    mask := WaveletMaskNoVan(n, m);
  <* FATAL Arithmetic.Error *>   (* MulPower can't fail for signals *)
  BEGIN
    mask := IntPow.MulPower(
              mask, NEW(S.T).fromArray(ARRAY OF R.T{R.Half, -R.Half}), m);
    mask := mask.translate(1 - mn2);
    IF mn2 MOD 2 = 0 THEN RETURN mask; ELSE RETURN mask.negate(); END;
  END WaveletMask;

PROCEDURE WaveletMaskNoVan (n, m: CARDINAL): S.T =
  BEGIN
    <* ASSERT (m + n) MOD 2 = 0, "n and m must have the same parity." *>
    RETURN DB.FilterPureAbsSqr((m + n) DIV 2).alternate();
  END WaveletMaskNoVan;

BEGIN
END BSplineWavelet.
