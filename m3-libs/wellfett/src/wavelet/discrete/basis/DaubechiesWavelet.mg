GENERIC MODULE DaubechiesWavelet(R,S);

CONST
  Half    = FLOAT(0.50D0, R.T);
  Quarter = FLOAT(0.25D0, R.T);

PROCEDURE FilterAbsSqr(n : CARDINAL) : S.T =
  VAR
    sum := FilterPureAbsSqr(n);
    fac := NEW(S.T).fromArray(ARRAY OF R.T{Quarter,Half,Quarter}, -1);
  BEGIN
    FOR k:=1 TO n DO
      sum := sum.convolve(fac);
    END;
    RETURN sum;
  END FilterAbsSqr;

PROCEDURE FilterPureAbsSqr(n : CARDINAL) : S.T =
  VAR
    sum :  S.T;
    fac := NEW(S.T).fromArray(ARRAY OF R.T{-Quarter,Half,-Quarter}, -1);
    coef  := NEW(REF ARRAY OF R.T,n);
    binom :  R.T := R.One;
  BEGIN
    IF n=0 THEN
      RETURN NEW(S.T).fromArray(ARRAY OF R.T{R.Zero});
    ELSE
      FOR k:=1 TO n DO
	coef[k-1] := binom;
	binom := binom*FLOAT(n-1+k,R.T)/FLOAT(k,R.T);
      END;
      (*use Horner's scheme for more numerical stability*)
      sum := NEW(S.T).fromArray(ARRAY OF R.T{coef[n-1]});
      FOR k:=n-2 TO 0 BY -1 DO
	sum := sum.convolve(fac);
	sum := sum.superpose(NEW(S.T).fromArray(ARRAY OF R.T{coef[k]}));
      END;
    END;
    RETURN sum;
  END FilterPureAbsSqr;

BEGIN
END DaubechiesWavelet.
