GENERIC MODULE UsualUnit();

IMPORT PhysicalUnit AS U;

PROCEDURE Equal (READONLY k1, k2: T; ): BOOLEAN =
  BEGIN
    RETURN U.Equal(k1.unit, k2.unit);
  END Equal;

BEGIN
END UsualUnit.
