MODULE WaveTriangle;

PROCEDURE Wave (x: LONGREAL; ): LONGREAL =
  BEGIN
    IF x < 0.25D0 THEN
      RETURN 4.0D0 * x;
    ELSIF x < 0.75D0 THEN
      RETURN 2.0D0 - 4.0D0 * x;
    ELSE
      RETURN 4.0D0 * x - 4.0D0;
    END;
  END Wave;

<* UNUSED *>
PROCEDURE WaveAlt (x: LONGREAL; ): LONGREAL =
  BEGIN
    IF x < 0.5D0 THEN
      RETURN 4.0D0 * x - 1.0D0;
    ELSE
      RETURN 3.0D0 - 4.0D0 * x;
    END;
  END WaveAlt;

BEGIN
END WaveTriangle.
