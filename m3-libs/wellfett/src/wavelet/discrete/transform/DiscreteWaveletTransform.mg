GENERIC MODULE DiscreteWaveletTransform( S, VS, MS);

IMPORT NADefinitions AS NA;

PROCEDURE FilterBankToPolyphase (READONLY x: VS.TBody; ): MS.T =
  VAR z := NEW(MS.T, NUMBER(x), NUMBER(x));
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := x[i].sliceRev(NUMBER(x))^;
    END;
    RETURN z;
  END FilterBankToPolyphase;

PROCEDURE PolyphaseToFilterBank (READONLY x: MS.TBody; ): VS.T
  RAISES {NA.Error} =
  VAR z := NEW(VS.T, NUMBER(x));
  BEGIN
    IF NUMBER(x) # NUMBER(x[0]) THEN RAISE NA.Error(NA.Err.bad_size); END;
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := NEW(S.T).interleaveRev(x[0]);
    END;
    RETURN z;
  END PolyphaseToFilterBank;

PROCEDURE FilterBankTransform (x: S.T; READONLY y: VS.TBody; ): VS.T =
  VAR z := NEW(VS.T, NUMBER(y));
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := x.convolveDown(y[i], NUMBER(y));
    END;
    RETURN z;
  END FilterBankTransform;

BEGIN
END DiscreteWaveletTransform.
