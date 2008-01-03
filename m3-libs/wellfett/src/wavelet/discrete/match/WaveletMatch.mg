GENERIC MODULE WaveletMatch(R, V, M, MIntPow, LA, S, Refn);

IMPORT Integer32IntegerPower AS IIntPow;
IMPORT Arithmetic, Range;

(*
IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT IO, Fmt, Wr, Thread;
*)


PROCEDURE TranslatesBasis
  (x: S.T; clip: Range.T; translates: Range.T; unit: CARDINAL; ): M.T =
  VAR
    basis  := M.New(translates.number, clip.number);
    offset := clip.first - translates.first * unit;
  BEGIN
    FOR j := 0 TO translates.number - 1 DO
      x.clipToArray(offset, basis[j]);
      DEC(offset, unit);
    END;
    RETURN basis;
  END TranslatesBasis;

PROCEDURE ComputeNormalEqu (target                                : S.T;
                            refineMask, generatorMask, waveletMask: S.T;
                            translates: Range.T;
                            numLevels : CARDINAL; ): NormalEqu =
  <* FATAL Arithmetic.Error *>   (*MulPower can't fail*)
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
    generatorAutoCorMat := NEW(M.T, translates.number, translates.number);
    generatorWaveletCor := refineAutoCor.convolveDown(
                             generatorMask.adjoint().convolve(waveletMask),
                             2).clipToVector(
                             translates.first, translates.number);
    waveletAutoCor := refineAutoCor.inner(waveletMaskAutoCor);

    targetCor := NEW(V.T, translates.number + 1);

  BEGIN
    FOR i := 0 TO translates.number - 1 DO
      generatorAutoCor.clipToArray(-i, generatorAutoCorMat[i]);
    END;
    (* DWT routine is only of little help here *)
    VAR
      x := target;
      y := refineMask.adjoint();
    BEGIN
      FOR i := 0 TO numLevels - 1 DO x := x.convolveDown(y, 2); END;
      x.convolveDown(generatorMask.adjoint(), 2).clipToArray(
        translates.first, SUBARRAY(targetCor^, 0, translates.number));
      targetCor[translates.number] :=
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

PROCEDURE ComputeLeastSquaresProblem
  (target                                : S.T;
   refineMask, generatorMask, waveletMask: S.T;
   translates                            : Range.T;
   numLevels                             : CARDINAL; ):
  LeastSquaresProblem =
  <* FATAL Arithmetic.Error *>   (* MulPower is not dangerous here. *)
  VAR
    generator := Refn.Refine(generatorMask, refineMask, numLevels);
    wavelet   := Refn.Refine(waveletMask, refineMask, numLevels);

    twonit := IIntPow.MulPower(2, 2, numLevels);
    clip := Range.Union(wavelet.getRange(),
                        Range.Add(generator.getRange(),
                                  Range.Scale(translates, twonit)));

    targetVec := target.clipToVector(clip.first, clip.number);
    basis     := M.New(translates.number + 1, clip.number);

  BEGIN
    wavelet.clipToArray(clip.first, basis[LAST(basis^)]);
    VAR offset := clip.first - translates.first * twonit;
    BEGIN
      FOR j := 0 TO translates.number - 1 DO
        generator.clipToArray(offset, basis[j]);
        DEC(offset, twonit);
      END;
    END;

    (*
    IO.Put(Fmt.FN("normal matrix %s, right hand side %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(M.MulMMA(basis)),
                         VF.Fmt(M.MulV(basis, targetvec))}));
    *)

    RETURN LeastSquaresProblem{clip, basis, targetVec};
  END ComputeLeastSquaresProblem;


PROCEDURE WithPattern (target                                : S.T;
                       refineMask, generatorMask, waveletMask: S.T;
                       translates                            : Range.T;
                       numLevels                             : CARDINAL; ):
  T =
  VAR
    lsqr := ComputeLeastSquaresProblem(target, refineMask, generatorMask,
                                       waveletMask, translates, numLevels);
    coef := LA.LeastSquares(lsqr.basis, ARRAY OF V.T{lsqr.targetPad},
                            flags := LA.LSFlagSet{LA.LSFlag.Transposed})[0];
    approx := M.MulTV(lsqr.basis, coef.x);

  <* FATAL Arithmetic.Error *>
  BEGIN
    (*
     <*FATAL Thread.Alerted, Wr.Failure*>
     IO.Put(Fmt.FN("translates.number %s, size %s, residuum %s, %s\n",
                   ARRAY OF
                     TEXT{Fmt.Int(translates.number), Fmt.Int(size),
                          RF.Fmt(coef.res), VF.Fmt(coef.x)}));
     *)
    (*
    IO.Put(Fmt.FN("residuum - %s\n", ARRAY OF TEXT{RF.Fmt(coef.res)}));
    *)

    RETURN T{NEW(S.T).fromArray(
               SUBARRAY(coef.x^, 0, translates.number), translates.first),
             coef.x[LAST(coef.x^)],
             NEW(S.T).fromVector(approx, lsqr.clip.first)};
  END WithPattern;

PROCEDURE WithPatternGenerator (target                   : S.T;
                                refineMask, generatorMask: S.T;
                                translates               : Range.T;
                                numLevels                : CARDINAL; ):
  Generator =
  VAR
    generator := Refn.Refine(generatorMask, refineMask, numLevels);

    twonit := IIntPow.MulPower(2, 2, numLevels);
    clip := Range.Add(
              generator.getRange(), Range.Scale(translates, twonit));

    targetVec := target.clipToVector(clip.first, clip.number);
    basis     := TranslatesBasis(generator, clip, translates, twonit);

  <* FATAL Arithmetic.Error *>
  BEGIN
    WITH coef = LA.LeastSquares(
                  basis, ARRAY OF V.T{targetVec},
                  flags := LA.LSFlagSet{LA.LSFlag.Transposed})[0],
         approx = M.MulTV(basis, coef.x) DO
      RETURN Generator{NEW(S.T).fromVector(coef.x, translates.first),
                       NEW(S.T).fromVector(approx, clip.first)};
    END;
  END WithPatternGenerator;


PROCEDURE WithPatternWavelet
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ): R.T =
  VAR
    match := WithPatternWaveletFast(
               target, refineMask, waveletMask, numLevels);
  BEGIN
    RETURN match.targetCor / match.waveletNorm;
  END WithPatternWavelet;

PROCEDURE WithPatternWaveletSimple
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ):
  InternalWaveletMatch =
  VAR wavelet := Refn.Refine(waveletMask, refineMask, numLevels);
  BEGIN
    RETURN InternalWaveletMatch{targetCor := wavelet.inner(target),
                                waveletNorm := wavelet.inner(wavelet)};
  END WithPatternWaveletSimple;

PROCEDURE WithPatternWaveletFast
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ):
  InternalWaveletMatch =
  VAR match: InternalWaveletMatch;
  BEGIN
    (* The alternative implementation looks more complicated but might be
       faster for high number of levels. *)
    VAR
      x := target;
      y := refineMask.adjoint();
    BEGIN
      FOR i := 0 TO numLevels - 1 DO x := x.convolveDown(y, 2); END;
      match.targetCor :=
        x.convolveDown(waveletMask.adjoint(), 2).getValue(0);
    END;
    <* FATAL Arithmetic.Error *> (* Power can't fail since we use square
                                    matrices *)
    VAR
      refineSize  := refineMask.getNumber() - 1;
      refineTrans := Refn.TransitionMatrix(refineMask);
      refinePower := MIntPow.Power(refineTrans, numLevels);
      (* extract the center column of the refinement matrix power *)
      refineAutoCor := NEW(S.T).fromVector(
                         M.GetColumn(refinePower, refineSize), -refineSize);
      waveletMaskAutoCor := waveletMask.autocorrelate();
    BEGIN
      match.waveletNorm := refineAutoCor.inner(waveletMaskAutoCor);
    END;
    RETURN match;
  END WithPatternWaveletFast;

BEGIN
END WaveletMatch.
