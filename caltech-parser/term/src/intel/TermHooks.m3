MODULE TermHooks;

REVEAL
  CharGetter = CharGetter BRANDED OBJECT
  OVERRIDES
    get                 := Get;
  END;

PROCEDURE Get(self: CharGetter): CHAR =
  BEGIN
  END Get;

PROCEDURE SetCharInput(c: CharGetter) =
  BEGIN
  END SetCharInput;

BEGIN
END TermHooks.
