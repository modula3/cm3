GENERIC MODULE CompositeUnit(UDB);

IMPORT PhysicalUnit AS U;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN=
  BEGIN
    RETURN U.Equal(k1.uu.unit,k2.uu.unit) AND
           k1.exp = k2.exp;
  END Equal;

BEGIN
END CompositeUnit.
