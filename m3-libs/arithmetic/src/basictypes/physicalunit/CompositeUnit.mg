GENERIC MODULE CompositeUnit();

IMPORT PhysicalUnit AS U;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN=
  BEGIN
    RETURN U.Equal(k1.uu.head.unit,k2.uu.head.unit) AND
           k1.exp = k2.exp;
  END Equal;

BEGIN
END CompositeUnit.
