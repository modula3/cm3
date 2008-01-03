GENERIC MODULE ScaledSignal(R);

PROCEDURE First (READONLY x: T; ): R.T =
  BEGIN
    RETURN FLOAT(x.signal.getFirst(), R.T) * x.resolution;
  END First;

PROCEDURE Last (READONLY x: T; ): R.T =
  BEGIN
    RETURN FLOAT(x.signal.getLast(), R.T) * x.resolution;
  END Last;

BEGIN
END ScaledSignal.
