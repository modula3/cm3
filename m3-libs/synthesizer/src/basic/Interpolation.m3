MODULE Interpolation;

PROCEDURE Linear (READONLY x: ARRAY [0 .. 1] OF LONGREAL; t: LONGREAL; ):
  LONGREAL =
  BEGIN
    RETURN x[0] + (x[1] - x[0]) * t;
  END Linear;

PROCEDURE Cubic (READONLY x: ARRAY [-1 .. 2] OF LONGREAL; t: LONGREAL; ):
  LONGREAL =
  BEGIN
    WITH lipm12 = Linear(ARRAY [0 .. 1] OF LONGREAL{x[-1], x[2]}, t),
         lip01  = Linear(ARRAY [0 .. 1] OF LONGREAL{x[0], x[1]}, t)   DO
      RETURN lip01 + t * (t - 1.0D0) / 2.0D0
                       * (x[0] + x[1] + lipm12 - 3.0D0 * lip01);
    END;
  END Cubic;


BEGIN
END Interpolation.
