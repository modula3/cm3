GENERIC MODULE DaubechiesWavelet(S,R);

PROCEDURE FilterAbsSqr(n : CARDINAL) : S.T =
  CONST
    Half    = FLOAT(0.50D0, R.T);
    Quarter = FLOAT(0.25D0, R.T);
  VAR
    fac := NEW(S.T);
    pow,
		scaledpow,
		sum : S.T;
		binom : R.T := R.One;
  BEGIN
    fac.fromArray(ARRAY OF R.T{Quarter,Half,Quarter}, -1);
    pow.fromArray(ARRAY OF R.T{R.One});
		FOR k:=1 TO n DO
			scaledpow := pow.copy();
		  scaledpow.scale(binom);
			sum := sum.superpose(scaledpow);
			pow := pow.convolve(fac);
      binom := binom*FLOAT(n-1+k,R.T)/FLOAT(k,R.T);
		END;
		RETURN pow;
  END FilterAbsSqr;


BEGIN
END DaubechiesWavelet.
