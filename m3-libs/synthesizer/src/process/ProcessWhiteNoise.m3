MODULE ProcessWhiteNoise;

IMPORT Random;


REVEAL
  T = Public BRANDED OBJECT
        amplitude: LONGREAL;
        random   : Random.T;
      OVERRIDES
        init := Init;
        get  := Get;
      END;


PROCEDURE Init (SELF: T; amplitude: LONGREAL; ): T =
  BEGIN
    SELF.amplitude := amplitude;
    SELF.random := NEW(Random.Default).init();

    RETURN SELF;
  END Init;


PROCEDURE Get (SELF: T; ): LONGREAL =
  BEGIN
    RETURN SELF.random.longreal(-SELF.amplitude, SELF.amplitude);
  END Get;

BEGIN
END ProcessWhiteNoise.
