GENERIC MODULE ContinuousWaveletTransform(R, V, VS, RT, RV, S, C, CV, FFT);


(* One can improve performance a lot here: Eliminate the monolithic Fourier
   transforms by using overlapping blocks for convolution.  Maybe FFTW
   supports that in a future version.  Store FFTW plans to accelerate
   repeated FFT application. *)


PROCEDURE Analyse (         x       : S.T;
                            wavelet : Wavelet;
                            width   : Width;
                   READONLY scales  : RV.T;
                            analysis: Analysis; ): T =
  VAR y := NEW(T, NUMBER(scales));
  BEGIN
    IF analysis = NIL THEN analysis := NEW(AnalysisFourier); END;
    EVAL analysis.init(x, wavelet, width);
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := analysis.scale(scales[i]);
    END;
    RETURN y;
  END Analyse;


REVEAL
  AnalysisFourier = Analysis BRANDED OBJECT
                      xFT    : CV.T;
                      first  : INTEGER;
                      number : CARDINAL;
                      wavelet: Wavelet;
                      width  : Width;
                    OVERRIDES
                      init  := AnalysisFourierInit;
                      scale := AnalysisFourierScale;
                    END;

PROCEDURE AnalysisFourierInit (h      : AnalysisFourier;
                               x      : S.T;
                               wavelet: Wavelet;
                               width  : Width;           ): Analysis =
  VAR
    xNumber := x.getNumber();
    number  := xNumber + width - 1;
    padded  := NEW(V.T, number);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure "
                  & "that the resulting signal can be represented as Signal.T" *>
    SUBARRAY(padded^, 0, xNumber) := x.getData()^;
    VS.Clear(SUBARRAY(padded^, xNumber, number - xNumber));
    h.xFT := FFT.DFTR2C1D(padded^);
    h.number := number;
    h.wavelet := wavelet;
    h.first := x.getFirst() - (width - 1) DIV 2;
    h.width := width;
    RETURN h;
  END AnalysisFourierInit;

PROCEDURE DiscretizeWavelet (    wavelet: Wavelet;
                                 scale  : R.T;
                             VAR samples: V.TBody; ) =
  VAR
    center := LAST(samples) DIV 2;
    amp    := R.One / RT.SqRt(scale);
  BEGIN
    FOR i := FIRST(samples) TO LAST(samples) DO
      samples[i] := amp * wavelet(R.FromInteger(i - center) / scale);
    END;
  END DiscretizeWavelet;

PROCEDURE AnalysisFourierScale (h: AnalysisFourier; scale: R.T; ): S.T =
  VAR wav := NEW(V.T, h.number);
  BEGIN
    DiscretizeWavelet(h.wavelet, scale, SUBARRAY(wav^, 0, h.width));
    VS.Clear(SUBARRAY(wav^, h.width, NUMBER(wav^) - h.width));
    WITH wavFT = FFT.DFTR2C1D(wav^)^ DO
      FOR i := FIRST(wavFT) TO LAST(wavFT) DO
        wavFT[i] := C.Mul(wavFT[i], h.xFT[i]);
      END;
      RETURN
        NEW(S.T).fromVector(FFT.DFTC2R1D(wavFT, h.number MOD 2), h.first);
    END;
  END AnalysisFourierScale;


REVEAL
  AnalysisNaive = Analysis BRANDED OBJECT
                    x      : S.T;
                    wavelet: Wavelet;
                    width  : Width;
                  OVERRIDES
                    init  := AnalysisNaiveInit;
                    scale := AnalysisNaiveScale;
                  END;

PROCEDURE AnalysisNaiveInit (h      : AnalysisNaive;
                             x      : S.T;
                             wavelet: Wavelet;
                             width  : Width;         ): Analysis =
  BEGIN
    h.x := x;
    h.wavelet := wavelet;
    h.width := width;
    RETURN h;
  END AnalysisNaiveInit;

PROCEDURE AnalysisNaiveScale (h: AnalysisNaive; scale: R.T; ): S.T =
  VAR wav := NEW(V.T, h.width);
  BEGIN
    DiscretizeWavelet(h.wavelet, scale, wav^);
    RETURN h.x.convolve(NEW(S.T).fromVector(wav, (h.width - 1) DIV 2));
  END AnalysisNaiveScale;





REVEAL AnalysisDWHandle = BRANDED REF RECORD END;

PROCEDURE AnalysisDW (x, wavelet: S.T; READONLY scales: RV.T; ): T =
  VAR
    y := NEW(T, NUMBER(scales));
    h := AnalysisDWInit(x, wavelet, RV.Max(scales));
  BEGIN
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := AnalysisDWScale(h, scales[i]);
    END;
    RETURN y;
  END AnalysisDW;

PROCEDURE AnalysisDWInit (<* UNUSED *> x, wavelet: S.T;
                          <* UNUSED *> maxScale  : R.T; ):
  AnalysisDWHandle =
  BEGIN
    <* ASSERT FALSE, "AnalysisDWInit not yet implemented" *>
  END AnalysisDWInit;

PROCEDURE AnalysisDWScale (<* UNUSED *> h    : AnalysisDWHandle;
                           <* UNUSED *> scale: R.T;              ): S.T =
  BEGIN
    <* ASSERT FALSE, "AnalysisDWScale not yet implemented" *>
  END AnalysisDWScale;

BEGIN
END ContinuousWaveletTransform.
