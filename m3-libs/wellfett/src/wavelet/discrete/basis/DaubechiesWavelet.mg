GENERIC MODULE DaubechiesWavelet(S,R);

PROCEDURE FilterAbsSqr(n : CARDINAL) : S.T =
  CONST
    Half    = FLOAT(0.50D0, R.T);
    Quarter = FLOAT(0.25D0, R.T);
  VAR
    pow := NEW(S.T).fromArray(ARRAY OF R.T{R.One});
		sum := NEW(S.T).init(0,1);
		binom : R.T := R.One;
  BEGIN
    VAR
      fac := NEW(S.T).fromArray(ARRAY OF R.T{-Quarter,Half,-Quarter}, -1);
    BEGIN
		  FOR k:=1 TO n DO
			  sum := sum.superpose(pow.scale(binom));
			  pow := pow.convolve(fac);
        binom := binom*FLOAT(n-1+k,R.T)/FLOAT(k,R.T);
		  END;
		END;
    VAR
      fac := NEW(S.T).fromArray(ARRAY OF R.T{Quarter,Half,Quarter}, -1);
    BEGIN
      FOR k:=1 TO n DO
			  sum := sum.convolve(fac);
      END;
    END;
		RETURN sum;
  END FilterAbsSqr;


BEGIN
END DaubechiesWavelet.
