GENERIC MODULE ContinuousWaveletSynthesis(R, V, S, Conv, CWT);

FROM CWT IMPORT Wavelet, Width;

REVEAL
  T = BRANDED REF RECORD
                    y      : S.T;
                    conv   : Conv.Handle;
                    wavelet: Wavelet;
                    width  : Width;
                  END;


PROCEDURE Do (READONLY w      : CWT.TBody;
                       wavelet: Wavelet;
                       width  : Width;
              READONLY scales : V.T;
                       conv   : Conv.Handle := NIL; ): S.T =
  VAR synthesis := New(wavelet, width, conv);
  BEGIN
    <* ASSERT NUMBER(w) = NUMBER(scales), "Number of scales must match" *>
    FOR i := FIRST(scales) TO LAST(scales) DO
      PutScale(synthesis, w[i], scales[i]);
    END;
    RETURN Finish(synthesis);
  END Do;


PROCEDURE New (wavelet: Wavelet; width: Width; conv: Conv.Handle := NIL; ):
  T =
  VAR h := NEW(T);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure "
                  & "that the resulting signal can be represented as Signal.T" *>
    IF conv = NIL THEN conv := NEW(Conv.HandleFourier); END;
    h.y := NIL;
    h.wavelet := wavelet;
    h.width := width;
    h.conv := conv;
    RETURN h;
  END New;

PROCEDURE PutScale (h: T; w: S.T; scale: R.T; ) =
  VAR
    wav := CWT.DiscretizeWavelet(h.wavelet, scale, h.width);
    y   := h.conv.init(w.getData(), h.width).convolve(wav);
    ys  := NEW(S.T).fromVector(y, w.getFirst() - h.width DIV 2);
  BEGIN
    h.conv.exit();
    IF h.y = NIL THEN h.y := ys; ELSE h.y := h.y.superpose(ys); END;
  END PutScale;

PROCEDURE Finish (h: T; ): S.T =
  BEGIN
    RETURN h.y;
  END Finish;


BEGIN
END ContinuousWaveletSynthesis.
