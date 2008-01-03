GENERIC MODULE WaveletMatchGradientLift(R, V, M, S, RefnSm, FnD);


PROCEDURE DeriveDist
  (normalMat: M.T; targetCor: V.T; targetNormSqr: R.T; s: S.T; ): FnD.T =
  VAR
    normals := M.MulV(normalMat, s.getData());
    dist := V.Inner(s.getData(), V.Sub(normals, V.Scale(targetCor, R.Two)))
              + targetNormSqr;

  BEGIN
    RETURN FnD.T{zeroth := dist, first :=
                 V.Scale(V.Sub(normals, targetCor), R.Two), second :=
                 M.Scale(normalMat, R.Two)};
  END DeriveDist;

PROCEDURE DeriveSSE (lpDual, hpDual0, s: S.T; ): FnD.T =
  VAR
    hpDual   := hpDual0.superpose(lpDual.upConvolve(s, 2));
    lpPrimal := hpDual.alternate();
    hpPrimal := lpDual.alternate();

    hSums := lpPrimal.wrapCyclic(3);
    dSums := M.Cyclic(hpPrimal.translate(2 * s.getFirst()).wrapCyclic(3),
                      s.getNumber(), -1);
  BEGIN
    (*
    IO.Put(MF.Fmt(dSums) & "\n");
    RETURN polypart;
    *)
    RETURN FnD.T{zeroth := RefnSm.ComputeSSE(hSums^), first :=
                 M.MulV(dSums, RefnSm.ComputeDSSE(hSums^)), second :=
                 M.Mul(M.Mul(dSums, RefnSm.ComputeDDSSE(hSums^)),
                       M.Transpose(dSums))};
  END DeriveSSE;

PROCEDURE DeriveWSSE (lpDual, hpDual0, s: S.T; c: R.T): FnD.T =
  VAR
    hsDual   := lpDual.upConvolve(s, 2);
    hpDual   := hpDual0.superpose(hsDual.scale(1.0D0 / c));
    lpPrimal := hpDual.alternate();
    hpPrimal := lpDual.alternate();
    gsPrimal := hsDual.alternate();

    hSums := lpPrimal.wrapCyclic(3);
    dSums := M.Cyclic(hpPrimal.scale(1.0D0 / c).translate(
                        2 * s.getFirst()).wrapCyclic(3), s.getNumber(), -1);
    cSums := V.Scale(gsPrimal.wrapCyclic(3), -1.0D0 / (c * c));
    dcSums := M.FromMatrixArray(
                ARRAY OF
                  ARRAY OF M.T{
                  ARRAY [0 .. 0] OF M.T{dSums},
                  ARRAY [0 .. 0] OF M.T{M.RowFromVector(cSums)}});

    sse   := RefnSm.ComputeSSE(hSums^);
    dsse  := RefnSm.ComputeDSSE(hSums^);
    ddsse := RefnSm.ComputeDDSSE(hSums^);

    dgSums := V.New(s.getNumber());

  BEGIN
    (*
    IO.Put(MF.Fmt(dSums) & "\n");
    RETURN polypart;
    *)
    FOR i := 0 TO s.getNumber() - 1 DO
      dgSums[i] := V.Inner(dsse, hpPrimal.translate(
                                   2 * (s.getFirst() + i)).wrapCyclic(3))
                     * (R.MinusOne / (c * c));
    END;

    RETURN FnD.T{zeroth := sse, first := M.MulV(dcSums, dsse), second :=
                 M.Add(M.Mul(M.Mul(dcSums, ddsse), M.Transpose(dcSums)),
                       M.FromMatrixArray(
                         ARRAY OF
                           ARRAY OF M.T{
                           ARRAY [0 .. 1] OF
                             M.T{M.NewZero(s.getNumber(), s.getNumber()),
                                 M.ColumnFromVector(dgSums)},
                           ARRAY [0 .. 1] OF
                             M.T{M.RowFromVector(dgSums),
                                 M.FromScalar(
                                   V.Inner(dsse, cSums) * -2.0D0 / c)}}))};
  END DeriveWSSE;



BEGIN
END WaveletMatchGradientLift.
