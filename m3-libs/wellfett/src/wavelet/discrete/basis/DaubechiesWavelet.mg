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
    summand := NEW(REF ARRAY OF S.T, n);
  BEGIN
    FOR k:=1 TO n DO
      summand[k-1] := pow.scale(binom);
      pow := pow.convolve(fac);
      binom := binom*FLOAT(n-1+k,R.T)/FLOAT(k,R.T);
    END;
    FOR k:=n-1 TO 0 BY -1 DO
      sum := sum.superpose(summand[k]);
    END;
    RETURN sum;
  END FilterPureAbsSqr;

BEGIN
END DaubechiesWavelet.
