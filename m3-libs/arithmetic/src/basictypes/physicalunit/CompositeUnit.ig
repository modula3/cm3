GENERIC INTERFACE CompositeUnit(UUList);

IMPORT PhysicalUnit AS U;

CONST
  Brand = "CompositeUnit";

TYPE
  T =
    RECORD
      uu   : UUList.T;   (*only the single entry is meant*)
      exp  : U.ExpType;
    END;

PROCEDURE Equal(READONLY k1, k2: T): BOOLEAN;


END CompositeUnit.
