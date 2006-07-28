GENERIC MODULE DyadicFilterBank(S);

CONST
  translationPrimalToDual = 1;
  translationDualToPrimal = -1;

<* INLINE *>
PROCEDURE CrossLowPassToHighPass
  (READONLY x: S.T; translation: [-1 .. 1]; ): S.T =
  BEGIN
    RETURN x.alternate().translate(translation);
  END CrossLowPassToHighPass;

<* INLINE *>
PROCEDURE CrossHighPassToLowPass
  (READONLY x: S.T; translation: [-1 .. 1]; ): S.T =
  BEGIN
    RETURN x.translate(translation).alternate();
  END CrossHighPassToLowPass;

PROCEDURE PrimalLowPassToDualHighPass (x: S.T; ): S.T =
  BEGIN
    RETURN x.alternate().translate(translationPrimalToDual);
  END PrimalLowPassToDualHighPass;

PROCEDURE PrimalHighPassToDualLowPass (x: S.T; ): S.T =
  BEGIN
    RETURN x.translate(translationPrimalToDual).alternate();
  END PrimalHighPassToDualLowPass;

PROCEDURE DualLowPassToPrimalHighPass (x: S.T; ): S.T =
  BEGIN
    RETURN x.alternate().translate(translationDualToPrimal);
  END DualLowPassToPrimalHighPass;

PROCEDURE DualHighPassToPrimalLowPass (x: S.T; ): S.T =
  BEGIN
    RETURN x.translate(translationDualToPrimal).alternate();
  END DualHighPassToPrimalLowPass;




<* INLINE *>
PROCEDURE BiorthogonalComplement
  (READONLY x: TBody; translation: [-1 .. 1]; ): TBody =
  BEGIN
    RETURN TBody{CrossHighPassToLowPass(x[1], translation),
                 CrossLowPassToHighPass(x[0], translation)};
  END BiorthogonalComplement;


PROCEDURE PrimalToDual (READONLY x: TBody; ): TBody =
  BEGIN
    RETURN BiorthogonalComplement(x, translationPrimalToDual);
  END PrimalToDual;

PROCEDURE DualToPrimal (READONLY x: TBody; ): TBody =
  BEGIN
    RETURN BiorthogonalComplement(x, translationDualToPrimal);
  END DualToPrimal;


PROCEDURE OrthogonalLowPassToHighPass (x: S.T): S.T =
  BEGIN
    RETURN x.alternate().adjoint().translate(1);
  END OrthogonalLowPassToHighPass;

PROCEDURE OrthogonalHighPassToLowPass (x: S.T): S.T =
  BEGIN
    RETURN x.translate(-1).adjoint().alternate();
  END OrthogonalHighPassToLowPass;


<* INLINE *>
PROCEDURE OrthogonalLowPassToHighPassGeneric
  (x: S.T; translation: [-1 .. 1]; ): S.T =
  BEGIN
    RETURN x.alternate().adjoint().translate(-translation);
  END OrthogonalLowPassToHighPassGeneric;

<* INLINE *>
PROCEDURE OrthogonalHighPassToLowPassGeneric
  (x: S.T; translation: [-1 .. 1]; ): S.T =
  BEGIN
    RETURN x.translate(translation).adjoint().alternate();
  END OrthogonalHighPassToLowPassGeneric;


<* UNUSED *>
PROCEDURE OrthogonalPrimalLowPassToHighPass (x: S.T; ): S.T =
  BEGIN
    RETURN OrthogonalLowPassToHighPassGeneric(x, 1);
  END OrthogonalPrimalLowPassToHighPass;

<* UNUSED *>
PROCEDURE OrthogonalPrimalHighPassToLowPass (x: S.T; ): S.T =
  BEGIN
    RETURN OrthogonalHighPassToLowPassGeneric(x, 1);
  END OrthogonalPrimalHighPassToLowPass;

<* UNUSED *>
PROCEDURE OrthogonalDualLowPassToHighPass (x: S.T; ): S.T =
  BEGIN
    RETURN OrthogonalLowPassToHighPassGeneric(x, -1);
  END OrthogonalDualLowPassToHighPass;

<* UNUSED *>
PROCEDURE OrthogonalDualHighPassToLowPass (x: S.T; ): S.T =
  BEGIN
    RETURN OrthogonalHighPassToLowPassGeneric(x, -1);
  END OrthogonalDualHighPassToLowPass;

BEGIN
END DyadicFilterBank.
