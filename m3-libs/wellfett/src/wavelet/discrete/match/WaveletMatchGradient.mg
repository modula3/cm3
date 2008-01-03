GENERIC MODULE WaveletMatchGradient(R, RF, V, VF, M, S, FnD);

IMPORT IO, Fmt, Wr, Thread;


REVEAL
  VariableWaveletAmplitude = T BRANDED OBJECT
                             OVERRIDES
                               splitParamVec     := VarSplitParamVec;
                               deriveRegularized := VarDeriveRegularized;
                             END;

  FixedWaveletAmplitude = FixedWaveletAmplitudePublic BRANDED OBJECT
                          OVERRIDES
                            splitParamVec     := FixSplitParamVec;
                            deriveRegularized := FixDeriveRegularized;
                          END;

PROCEDURE VarSplitParamVec (SELF: VariableWaveletAmplitude; x: V.T; ):
  Parameters =
  BEGIN
    RETURN Parameters{NEW(S.T).fromArray(
                        SUBARRAY(x^, FIRST(x^), NUMBER(x^) - 1),
                        SELF.yFirst), x[LAST(x^)], R.One};
  END VarSplitParamVec;


PROCEDURE VarDeriveRegularized (<* UNUSED *> SELF: VariableWaveletAmplitude;
                                         derDist: FnD.T;
                                READONLY mc     : Parameters;
                                waveletVec, waveletCor, targetVec: V.T; ):
  FnD.T =
  BEGIN
    RETURN ExtendDervTarget(derDist, mc.lift.getData(), mc.wavelet0Amp,
                            waveletVec, waveletCor, targetVec);
  END VarDeriveRegularized;

PROCEDURE FixSplitParamVec (SELF: FixedWaveletAmplitude; x: V.T; ):
  Parameters =
  BEGIN
    RETURN Parameters{NEW(S.T).fromArray(
                        SUBARRAY(x^, FIRST(x^), NUMBER(x^) - 1),
                        SELF.yFirst), SELF.wavAmp, R.One};
  END FixSplitParamVec;

PROCEDURE FixDeriveRegularized (<* UNUSED *> SELF: FixedWaveletAmplitude;
                                         derDist: FnD.T;
                                READONLY mc     : Parameters;
                                <* UNUSED *> waveletVec, waveletCor,
                                               targetVec: V.T; ): FnD.T =
  VAR zeroVec := V.NewZero(mc.lift.getNumber());
  BEGIN
    RETURN
      FnD.T{
        zeroth := derDist.zeroth, first :=
        V.FromVectorArray(
          ARRAY OF V.T{derDist.first, V.FromScalar(R.Zero)}), second :=
        M.FromMatrixArray(
          ARRAY [0 .. 1], [0 .. 1] OF
            M.T{
            ARRAY OF M.T{derDist.second, M.ColumnFromArray(zeroVec^)},
            ARRAY OF M.T{M.RowFromArray(zeroVec^), M.FromScalar(R.One)}})};
  END FixDeriveRegularized;



PROCEDURE ExtendDervTarget (READONLY x        : FnD.T;
                                     lift     : V.T;
                                     targetAmp: R.T;
                                     target   : V.T;
                                     targetCor: V.T;
                                     wavelet0 : V.T    ): FnD.T =
  BEGIN
    RETURN
      FnD.T{zeroth :=
            x.zeroth + R.Two * targetAmp * V.Inner(targetCor, lift)
              + targetAmp * targetAmp * V.Inner(target, target)
              - R.Two * targetAmp * V.Inner(wavelet0, target), first :=
            V.FromVectorArray(
              ARRAY OF
                V.T{V.Add(x.first, V.Scale(targetCor, targetAmp * R.Two)),
                    V.FromScalar(
                      R.Two * (V.Inner(lift, targetCor)
                                 + targetAmp * V.Inner(target, target)
                                 - V.Inner(wavelet0, target)))}), second :=
            M.FromMatrixArray(
              ARRAY [0 .. 1], [0 .. 1] OF
                M.T{ARRAY OF
                      M.T{x.second,
                          M.ColumnFromArray(V.Scale(targetCor, R.Two)^)},
                    ARRAY OF
                      M.T{M.RowFromArray(V.Scale(targetCor, R.Two)^),
                          M.FromScalar(V.Inner(target, target) * R.Two)}})};
  END ExtendDervTarget;

PROCEDURE PutDervDif (READONLY der   : FnD.T;
                      READONLY derArr: ARRAY OF FnD.T;
                               delta : R.T             )
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    (* compare first derivative (gradient) with the first difference *)
    IO.Put(VF.Fmt(V.Scale(der.first, delta)) & "\n");
    FOR j := 0 TO LAST(derArr) DO
      IO.Put(Fmt.FN("der[%s] %s, %s\n",
                    ARRAY OF
                      TEXT{Fmt.Int(j (* + y.getFirst() *)),
                           RF.Fmt(derArr[j].zeroth - der.zeroth),
                           RF.Fmt(der.first[j] * delta)}));
    END;

    (* compare second derivative with the first difference of the first
       derivative *)
    FOR j := 0 TO LAST(derArr) DO
      IO.Put(
        Fmt.FN("der[%s]\n %s %s\n",
               ARRAY OF
                 TEXT{Fmt.Int(j (* + y.getFirst() *)),
                      VF.Fmt(V.Sub(derArr[j].first, der.first)),
                      VF.Fmt(V.Scale(M.GetRow(der.second, j), delta))}));
    END;
  END PutDervDif;

BEGIN
END WaveletMatchGradient.
