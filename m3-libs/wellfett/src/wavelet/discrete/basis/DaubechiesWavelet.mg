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
    pow := NEW(S.T).fromArray(ARRAY OF R.T{R.One});
		sum := NEW(S.T).init(0,1);
		binom : R.T := R.One;
    fac := NEW(S.T).fromArray(ARRAY OF R.T{-Quarter,Half,-Quarter}, -1);
  BEGIN
    FOR k:=1 TO n DO
      sum := sum.superpose(pow.scale(binom));
      pow := pow.convolve(fac);
      binom := binom*FLOAT(n-1+k,R.T)/FLOAT(k,R.T);
    END;
    RETURN sum;
  END FilterPureAbsSqr;

BEGIN
END DaubechiesWavelet.
