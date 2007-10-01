MODULE WaveSine;

IMPORT Math;

PROCEDURE Wave (x: LONGREAL; ): LONGREAL =
  BEGIN
    RETURN Math.sin(x * Math.Pi * 2.0D0);
  END Wave;

BEGIN
END WaveSine.
