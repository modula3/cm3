GENERIC MODULE ContinuousWaveletTransform(R, V, VS, RV, S, C, CV, FFT);


(* One can improve performance a lot here: Eliminate the monolithic Fourier
   transforms by using overlapping blocks for convolution.  Store FFTW
   plans to accelerate repeated FFT application. *)

REVEAL
  AnalysisHandle = BRANDED REF RECORD
                                 xFT    : CV.T;
                                 size   : CARDINAL;
                                 wavelet: Wavelet;
                                 first  : INTEGER;
                                 width  : Width;
                               END;

PROCEDURE Analysis (         x      : S.T;
                             wavelet: Wavelet;
                             width  : Width;
                    READONLY scales : RV.T;    ): T =
  VAR
    y := NEW(T, NUMBER(scales));
    h := AnalysisInit(x, wavelet, width);
  BEGIN
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := AnalysisScale(h, scales[i]);
    END;
    RETURN y;
  END Analysis;


PROCEDURE AnalysisInit (x: S.T; wavelet: Wavelet; width: Width; ):
  AnalysisHandle =
  VAR
    xSize  := x.getNumber();
    size   := xSize + width - 1;
    padded := NEW(V.T, size);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure that the resulting signal can be represented as Signal.T" *>
    SUBARRAY(padded^, 0, xSize) := x.getData()^;
    VS.Clear(SUBARRAY(padded^, xSize, size - xSize));
    RETURN NEW(AnalysisHandle, xFT := FFT.DFTR2C1D(padded^), size := size,
               wavelet := wavelet,
               first := x.getFirst() - (width - 1) DIV 2, width := width);
  END AnalysisInit;

PROCEDURE AnalysisScale (h: AnalysisHandle; scale: R.T; ): S.T =
  VAR
    wav    := NEW(V.T, h.size);
    center := (h.width - 1) DIV 2;
  BEGIN
    FOR i := 0 TO h.width - 1 DO
      wav[i] := h.wavelet(R.FromInteger(i - center) / scale);
    END;
    VS.Clear(SUBARRAY(wav^, h.width, NUMBER(wav^) - h.width));
    WITH wavFT = FFT.DFTR2C1D(wav^)^ DO
      FOR i := FIRST(wavFT) TO LAST(wavFT) DO
        wavFT[i] := C.Mul(wavFT[i], h.xFT[i]);
      END;
      RETURN
        NEW(S.T).fromVector(FFT.DFTC2R1D(wavFT, h.size MOD 2), h.first);
    END;
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
