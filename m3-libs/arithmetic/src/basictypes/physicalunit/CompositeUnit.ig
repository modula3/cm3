GENERIC INTERFACE CompositeUnit();

IMPORT PhysicalUnit AS U;

CONST
  Brand = "CompositeUnit";

TYPE
  UsualUnit<:REFANY;
  T =
    RECORD
      uu   : UsualUnit;
      exp  : U.ExpType;
    END;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN;


END CompositeUnit.
