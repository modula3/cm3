MODULE ProcessControlConstant;

IMPORT SignalControl;

PROCEDURE Do (length: CARDINAL; value: LONGREAL; ):
  SignalControl.RefArray =
  VAR x := NEW(SignalControl.RefArray, length);
  BEGIN
    FOR j := FIRST(x^) TO LAST(x^) DO x[j] := value; END;
    RETURN x;
  END Do;

REVEAL
  T = Public BRANDED OBJECT
        value: LONGREAL;
      OVERRIDES
        init := Init;
        get  := Get;
      END;

PROCEDURE Init (SELF: T; value: LONGREAL; ): T =
  BEGIN
    SELF.value := value;
    RETURN SELF;
  END Init;

PROCEDURE Get (SELF: T; ): LONGREAL =
  BEGIN
    RETURN SELF.value;
  END Get;

BEGIN
END ProcessControlConstant.
