MODULE ProcessExponential;

IMPORT Signal, Math;

PROCEDURE Do (length: CARDINAL; halflife: LONGREAL; ): Signal.RefArray =
  VAR z := NEW(Signal.RefArray, length);

  BEGIN
    FOR j := FIRST(z^) TO LAST(z^) DO
      z[j] := Math.pow(0.5D0, FLOAT(j, LONGREAL) / halflife);
    END;

    RETURN z;

  END Do;


PROCEDURE Init (SELF: T; halflife: LONGREAL; ): T =
  BEGIN
    SELF.factor := Math.pow(0.5D0, 1.0D0 / halflife);
    SELF.elongation := 1.0D0;
    RETURN SELF;
  END Init;

PROCEDURE Get (SELF: T; ): LONGREAL =
  VAR x := SELF.elongation;
  BEGIN
    SELF.elongation := SELF.elongation * SELF.factor;
    RETURN x;
  END Get;

BEGIN

END ProcessExponential.
