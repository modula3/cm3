GENERIC MODULE MatchWaveletGradient(R, V, M, S, RefnSm, FnD);


PROCEDURE DeriveDist (normalMat    : M.T;
                      targetCor    : V.T;
                      targetNormSqr: R.T;
                      s            : S.T; ): FnD.T =
  VAR
    normals := M.MulV(normalMat, s.getData());
    dist := V.Inner(s.getData(), V.Sub(normals, V.Scale(targetCor, R.Two)))
              + targetNormSqr;

  BEGIN
    RETURN FnD.T{zeroth := dist, first :=
                 V.Scale(V.Sub(normals, targetCor), R.Two), second :=
                 M.Scale(normalMat, R.Two)};
  END DeriveDist;

PROCEDURE DeriveSSE (hDual, gDual0, s: S.T; ): FnD.T =
  VAR
    gDual   := gDual0.superpose(hDual.upConvolve(s, 2));
    hPrimal := gDual.alternate();
    gPrimal := hDual.alternate();

    hSums := hPrimal.wrapCyclic(3);
    dSums := M.Cyclic(gPrimal.translate(2 * s.getFirst()).wrapCyclic(3),
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

PROCEDURE DeriveWSSE (hDual, gDual0, s: S.T; c: R.T): FnD.T =
  VAR
    hsDual   := hDual.upConvolve(s, 2);
    gDual    := gDual0.superpose(hsDual.scale(1.0D0 / c));
    hPrimal  := gDual.alternate();
    gPrimal  := hDual.alternate();
    gsPrimal := hsDual.alternate();

    hSums := hPrimal.wrapCyclic(3);
    dSums := M.Cyclic(gPrimal.scale(1.0D0 / c).translate(
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
      dgSums[i] := V.Inner(dsse, gPrimal.translate(
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
END MatchWaveletGradient.
