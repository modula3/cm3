GENERIC MODULE ContinuousWaveletTransform(R, RT, RV, C, CV, S, Conv);


PROCEDURE Analyse (         x      : S.T;
                            wavelet: Wavelet;
                            width  : Width;
                   READONLY scales : RV.T;
                            conv   : Conv.Handle := NIL; ): T =
  VAR
    y        := NEW(T, NUMBER(scales));
    analysis := NewAnalysis(x, wavelet, width, conv);
  BEGIN
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := AnalysisScale(analysis, scales[i]);
    END;
    RETURN y;
  END Analyse;



PROCEDURE DiscretizeWavelet (wavelet: Wavelet; scale: R.T; width: Width; ):
  CV.T =
  VAR
    samples := NEW(CV.T, width);
    center  := LAST(samples^) DIV 2;
    amp     := R.One / RT.SqRt(scale);
  BEGIN
    FOR i := FIRST(samples^) TO LAST(samples^) DO
      samples[i] :=
        C.Scale(wavelet(R.FromInteger(i - center) / scale), amp);
    END;
    RETURN samples;
  END DiscretizeWavelet;

REVEAL
  Analysis = BRANDED REF RECORD
                           first  : INTEGER;
                           wavelet: Wavelet;
                           width  : Width;
                           conv   : Conv.Handle;
                         END;

PROCEDURE NewAnalysis (x      : S.T;
                       wavelet: Wavelet;
                       width  : Width;
                       conv   : Conv.Handle := NIL; ): Analysis =
  VAR h := NEW(Analysis);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure "
                  & "that the resulting signal can be represented as Signal.T" *>
    IF conv = NIL THEN conv := NEW(Conv.HandleFourier); END;
    h.first := x.getFirst();
    h.wavelet := wavelet;
    h.width := width;
    h.conv := conv.init(x.getData(), width);
    RETURN h;
  END NewAnalysis;

PROCEDURE AnalysisScale (h: Analysis; scale: R.T; ): S.T =
  VAR wav := DiscretizeWavelet(h.wavelet, scale, h.width);
  BEGIN
    RETURN NEW(S.T).fromVector(h.conv.convolve(wav), (h.width - 1) DIV 2);
  END AnalysisScale;





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
