GENERIC MODULE DyadicFilterBank(S);

PROCEDURE Complement (READONLY x: T): T =
  BEGIN
    RETURN T{OrthogonalGenerator(x[1]), OrthogonalWavelet(x[0])};
  END Complement;

PROCEDURE OrthogonalWavelet (x: S.T): S.T =
  BEGIN
    RETURN x.alternate().adjoint().translate(1);
  END OrthogonalWavelet;

PROCEDURE OrthogonalGenerator (x: S.T): S.T =
  BEGIN
    RETURN x.translate(-1).adjoint().alternate();
  END OrthogonalGenerator;

BEGIN
END DyadicFilterBank.
