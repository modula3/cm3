GENERIC MODULE MatchWavelet(R, V, M, MIntPow, LA, S, Refn, RefnSm, FnD);

IMPORT Integer32IntegerPower AS IIntPow;

(*
IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT IO, Fmt, Wr, Thread;
*)

IMPORT Arithmetic AS Arith;


PROCEDURE MatchPatternGenWav (target                                : S.T;
                              refineMask, generatorMask, waveletMask: S.T;
                              numLevels     : CARDINAL;
                              firstTranslate: INTEGER;
                              numTranslates : CARDINAL; ): MatchGenWav =
  VAR
    generator := Refn.Refine(generatorMask, refineMask, numLevels);
    wavelet   := Refn.Refine(waveletMask, refineMask, numLevels);

    lastTranslate := firstTranslate + numTranslates - 1;

    twonit := IIntPow.MulPower(2, 2, numLevels);
    first := MIN(wavelet.getFirst(),
                 generator.getFirst() + twonit * firstTranslate);
    last := MAX(wavelet.getLast(),
                generator.getLast() + twonit * lastTranslate);
    size := last - first + 1;

    targetVec := target.clipToVector(first, size);
    basis     := M.New(numTranslates + 1, size);

  <* FATAL Arith.Error *>
  BEGIN
    wavelet.clipToArray(first, basis[LAST(basis^)]);
    FOR j := firstTranslate TO lastTranslate DO
      generator.clipToArray(first - twonit * j, basis[j - firstTranslate]);
    END;

    (*
    IO.Put(Fmt.FN("normal matrix %s, right hand side %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(M.MulMMA(basis)),
                         VF.Fmt(M.MulV(basis, targetvec))}));
    *)

    VAR
      coef := LA.LeastSquares(basis, ARRAY OF V.T{targetVec},
                              flags := LA.LSFlagSet{LA.LSFlag.transposed})[
                0];
      approx := M.MulTV(basis, coef.x);

    BEGIN
      (*
      <*FATAL Thread.Alerted, Wr.Failure*>
      IO.Put(Fmt.FN("numTranslates %s, size %s, residuum %s, %s\n",
                    ARRAY OF
                      TEXT{Fmt.Int(numTranslates), Fmt.Int(size),
                           RF.Fmt(coef.res), VF.Fmt(coef.x)}));
      *)
      (*
      IO.Put(Fmt.FN("residuum - %s\n", ARRAY OF TEXT{RF.Fmt(coef.res)}));
      *)

      RETURN
        MatchGenWav{NEW(S.T).fromArray(
                      SUBARRAY(coef.x^, 0, numTranslates), firstTranslate),
                    coef.x[LAST(coef.x^)],
                    NEW(S.T).fromVector(approx, first), basis, targetVec};
    END;
  END MatchPatternGenWav;

PROCEDURE ComputeNormalEqu (target                                : S.T;
                            refineMask, generatorMask, waveletMask: S.T;
                            numLevels     : CARDINAL;
                            firstTranslate: INTEGER;
                            numTranslates : CARDINAL; ): NormalEqu =
  <* FATAL Arith.Error *>        (*MulPower can't fail*)
  VAR
    refineSize := refineMask.getNumber() - 1;

    refineTrans := Refn.TransitionMatrix(refineMask);

    generatorMaskAutoCor := generatorMask.autocorrelate();
    waveletMaskAutoCor   := waveletMask.autocorrelate();

    refinePower := MIntPow.Power(refineTrans, numLevels);
    (* extract the center column of the refinement matrix power *)
    refineAutoCor := NEW(S.T).fromVector(
                       M.GetColumn(refinePower, refineSize), -refineSize);
    generatorAutoCor := refineAutoCor.convolveDown(generatorMaskAutoCor, 2);
    generatorAutoCorMat := NEW(M.T, numTranslates, numTranslates);
    generatorWaveletCor := refineAutoCor.convolveDown(
                             generatorMask.adjoint().convolve(waveletMask),
                             2).clipToVector(firstTranslate, numTranslates);
    waveletAutoCor := refineAutoCor.inner(waveletMaskAutoCor);

    targetCor := NEW(V.T, numTranslates + 1);

  BEGIN
    FOR i := 0 TO numTranslates - 1 DO
      generatorAutoCor.clipToArray(-i, generatorAutoCorMat[i]);
    END;
    (* DWT routine is only of little help here *)
    VAR
      x := target;
      y := refineMask.adjoint();
    BEGIN
      FOR i := 0 TO numLevels - 1 DO x := x.convolveDown(y, 2); END;
      x.convolveDown(generatorMask.adjoint(), 2).clipToArray(
        firstTranslate, SUBARRAY(targetCor^, 0, numTranslates));
      targetCor[numTranslates] :=
        x.convolveDown(waveletMask.adjoint(), 2).getValue(0);
    END;

    RETURN NormalEqu{
             M.FromMatrixArray(
               M.TMBody{ARRAY [0 .. 1] OF
                          M.T{generatorAutoCorMat,
                              M.ColumnFromVector(generatorWaveletCor)},
                        ARRAY [0 .. 1] OF
                          M.T{M.RowFromVector(generatorWaveletCor),
                              M.FromScalar(waveletAutoCor)}}), targetCor};
  END ComputeNormalEqu;

PROCEDURE MatchPatternGen (target                   : S.T;
                           refineMask, generatorMask: S.T;
                           numLevels                : CARDINAL;
                           firstTranslate           : INTEGER;
                           numTranslates            : CARDINAL; ):
  MatchGen =
  VAR
    generator := Refn.Refine(generatorMask, refineMask, numLevels);

    lastTranslate := firstTranslate + numTranslates - 1;

    twonit := IIntPow.MulPower(2, 2, numLevels);
    first  := generator.getFirst() + twonit * firstTranslate;
    last   := generator.getLast() + twonit * lastTranslate;
    size   := last - first + 1;

    targetVec := target.clipToVector(first, size);
    basis     := M.New(numTranslates, size);

  <* FATAL Arith.Error *>
  BEGIN
    FOR j := firstTranslate TO lastTranslate DO
      generator.clipToArray(first - twonit * j, basis[j - firstTranslate]);
    END;

    WITH coef = LA.LeastSquares(
                  basis, ARRAY OF V.T{targetVec},
                  flags := LA.LSFlagSet{LA.LSFlag.transposed})[0],
         approx = M.MulTV(basis, coef.x) DO
      RETURN
        MatchGen{NEW(S.T).fromVector(coef.x, firstTranslate),
                 NEW(S.T).fromVector(approx, first), basis, targetVec};
    END;
  END MatchPatternGen;

PROCEDURE MatchPatternWav (target                 : S.T;
                           refineMask, waveletMask: S.T;
                           numLevels              : CARDINAL; ): R.T =
  (* Compute this by two different approaches. *)
  VAR
    wavelet     := Refn.Refine(waveletMask, refineMask, numLevels);
    targetCor   := wavelet.inner(target);
    waveletNorm := wavelet.inner(wavelet);
  BEGIN
    (* The alternative implementation looks more complicated but might be
       faster for high number of levels. *)
    VAR
      x := target;
      y := refineMask.adjoint();
    BEGIN
      FOR i := 0 TO numLevels - 1 DO x := x.convolveDown(y, 2); END;
      WITH targetCor2 = x.convolveDown(waveletMask.adjoint(), 2).getValue(
                          0) DO
        <* ASSERT ABS(targetCor2 - targetCor) <= ABS(targetCor) * 1.0D-15 *>
      END;
    END;
    <* FATAL Arith.Error *>      (* Power can't fail since we use square
                                    matrices *)
    VAR
      refineSize  := refineMask.getNumber() - 1;
      refineTrans := Refn.TransitionMatrix(refineMask);
      refinePower := MIntPow.Power(refineTrans, numLevels);
      (* extract the center column of the refinement matrix power *)
      refineAutoCor := NEW(S.T).fromVector(
                         M.GetColumn(refinePower, refineSize), -refineSize);
      waveletMaskAutoCor := waveletMask.autocorrelate();
      waveletNorm2       := refineAutoCor.inner(waveletMaskAutoCor);
    BEGIN
      <* ASSERT ABS(waveletNorm2 - waveletNorm)
                  <= ABS(waveletNorm) * 1.0D-15 *>
    END;
    RETURN targetCor / waveletNorm;
  END MatchPatternWav;



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
END MatchWavelet.
