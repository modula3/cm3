GENERIC MODULE ContinuousWaveletAnalysis(R, RT, V, S, Conv, CWT);

FROM CWT IMPORT Wavelet, Width;

REVEAL
  T = BRANDED REF RECORD
                    conv   : Conv.T;
                    first  : INTEGER;
                    wavelet: Wavelet;
                    width  : Width;
                  END;


PROCEDURE Do (         x      : S.T;
                       wavelet: Wavelet;
                       width  : Width;
              READONLY scales : V.T;
                       conv   : Conv.T    := NIL; ): CWT.T =
  VAR
    y        := NEW(CWT.T, NUMBER(scales));
    analysis := New(x, wavelet, width, conv);
  BEGIN
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := GetScale(analysis, scales[i], R.One / RT.SqRt(scales[i]));
    END;
    RETURN y;
  END Do;


PROCEDURE New
  (x: S.T; wavelet: Wavelet; width: Width; conv: Conv.T := NIL; ): T =
  VAR h := NEW(T);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure "
                  & "that the resulting signal can be represented as Signal.T" *>
    IF conv = NIL THEN conv := NEW(Conv.Fourier); END;
    h.first := x.getFirst();
    h.wavelet := wavelet;
    h.width := width;
    h.conv := conv.init(x.getData(), width);
    RETURN h;
  END New;

PROCEDURE GetScale (h: T; scale, amp: R.T; ): S.T =
  VAR wav := CWT.DiscretizeWavelet(h.wavelet, scale, amp, h.width);
  BEGIN
    RETURN
      NEW(S.T).fromVector(h.conv.convolve(wav), h.first - h.width DIV 2);
  END GetScale;





REVEAL TDW = BRANDED REF RECORD END;

PROCEDURE DoDW (x, wavelet: S.T; READONLY scales: V.T; ): CWT.T =
  VAR
    y := NEW(CWT.T, NUMBER(scales));
    h := NewDW(x, wavelet, V.Max(scales));
  BEGIN
    FOR i := FIRST(scales) TO LAST(scales) DO
      y[i] := GetScaleDW(h, scales[i]);
    END;
    RETURN y;
  END DoDW;

PROCEDURE NewDW
  (<* UNUSED *> x, wavelet: S.T; <* UNUSED *> maxScale: R.T; ): TDW =
  BEGIN
    <* ASSERT FALSE, "NewDW not yet implemented" *>
  END NewDW;

PROCEDURE GetScaleDW (<* UNUSED *> h: TDW; <* UNUSED *> scale: R.T; ):
  S.T =
  BEGIN
    <* ASSERT FALSE, "CopmuteScaleDW not yet implemented" *>
  END GetScaleDW;

BEGIN
END ContinuousWaveletAnalysis.
