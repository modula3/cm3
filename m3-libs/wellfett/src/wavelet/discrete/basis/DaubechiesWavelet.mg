GENERIC MODULE DaubechiesWavelet(R, RT, S, IntPow);

IMPORT Arithmetic AS Arith;

CONST
  Half    = R.Half;
  Quarter = Half * Half;

PROCEDURE Filter (n: CARDINAL): S.T =
  VAR
    sum := FilterPure(n);
    fac := NEW(S.T).fromArray(ARRAY OF R.T{Half, Half}, 0);
  <* FATAL Arith.Error *>        (* Power can't fail for signals *)
  BEGIN
    RETURN IntPow.MulPower(sum, fac, n);
  END Filter;

PROCEDURE FilterPure (n: CARDINAL): S.T =
  BEGIN
    CASE n OF
    | 0, 1 => RETURN S.One;
    | 2 =>
        WITH w = RT.Pi / FLOAT(12, R.T) DO
          RETURN NEW(S.T).fromArray(
                   ARRAY OF
                     R.T{RT.SqRtTwo * RT.Cos(w), -RT.SqRtTwo * RT.Sin(w)});
        END;
    | 4 =>
        RETURN NEW(S.T).fromArray(
                 ARRAY OF
                   R.T{FLOAT(2.3037781330694596D-1, R.T),
                       FLOAT(-2.066646826819176D-1, R.T),
                       FLOAT(7.52726188069721D-2, R.T),
                       FLOAT(-1.05974017849973D-2, R.T)}).scale(
                 FLOAT(8, R.T) * RT.SqRtTwo);
    ELSE
      <* ASSERT FALSE, "Sorry this order is not supported." *>
    END;
  END FilterPure;

PROCEDURE FilterAbsSqr (n: CARDINAL): S.T =
  VAR
    sum := FilterPureAbsSqr(n);
    fac := NEW(S.T).fromArray(ARRAY OF R.T{Quarter, Half, Quarter}, -1);
  <* FATAL Arith.Error *>        (* Power can't fail for signals *)
  BEGIN
    RETURN IntPow.MulPower(sum, fac, n);
  END FilterAbsSqr;

PROCEDURE FilterPureAbsSqr (n: CARDINAL): S.T =
  VAR
    sum: S.T;
    fac  := NEW(S.T).fromArray(ARRAY OF R.T{-Quarter, Half, -Quarter}, -1);
    coef := NEW(REF ARRAY OF R.T, n);
    binom: R.T := R.One;
  BEGIN
    IF n = 0 THEN
      RETURN NEW(S.T).fromArray(ARRAY OF R.T{R.Zero});
    ELSE
      FOR k := 1 TO n DO
        coef[k - 1] := binom;
        binom := binom * FLOAT(n - 1 + k, R.T) / FLOAT(k, R.T);
      END;
      (* use Horner's scheme for more numerical stability *)
      sum := NEW(S.T).fromArray(ARRAY OF R.T{coef[n - 1]});
      FOR k := n - 2 TO 0 BY -1 DO
        sum := sum.convolve(fac);
        sum := sum.superpose(NEW(S.T).fromArray(ARRAY OF R.T{coef[k]}));
      END;
    END;
    RETURN sum;
  END FilterPureAbsSqr;

BEGIN
END DaubechiesWavelet.
