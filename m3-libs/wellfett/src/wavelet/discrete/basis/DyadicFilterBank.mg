GENERIC MODULE DyadicFilterBank();

PROCEDURE PrimalToDual (READONLY x: T): T =
  BEGIN
    RETURN T{x[1].translate(1).alternate(), x[0].alternate().translate(1)};
  END PrimalToDual;

PROCEDURE DualToPrimal (READONLY x: T): T =
  BEGIN
    RETURN
      T{x[1].translate(-1).alternate(), x[0].alternate().translate(-1)};
  END DualToPrimal;

BEGIN
END DyadicFilterBank.
