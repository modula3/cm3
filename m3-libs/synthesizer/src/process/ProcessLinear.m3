MODULE ProcessLinear;


REVEAL
  T = Public BRANDED OBJECT
        y, inc: LONGREAL;
      OVERRIDES
        init := Init;
        get  := Get;
      END;


PROCEDURE Init (SELF: T; start, increment: LONGREAL; ): T =
  BEGIN
    SELF.y := start;
    SELF.inc := increment;

    RETURN SELF;
  END Init;


PROCEDURE Get (SELF: T; ): LONGREAL =
  VAR y := SELF.y;
  BEGIN
    SELF.y := SELF.y + SELF.inc;
    RETURN y;
  END Get;

BEGIN
END ProcessLinear.
