GENERIC MODULE DyadicFilterBank();

PROCEDURE GetComplement (READONLY x: T): T =
  BEGIN
    RETURN T{x[1].translate(-1).adjoint().alternate(),
             x[0].alternate().adjoint().translate(1)};
  END GetComplement;

BEGIN
END DyadicFilterBank.
