GENERIC MODULE ContinuousWaveletSynthesis(R, RT, V, S, Conv, CWT);

FROM CWT IMPORT Wavelet, Width;

REVEAL
  T = BRANDED REF RECORD
                    y      : S.T;
                    conv   : Conv.T;
                    wavelet: Wavelet;
                    width  : Width;
                  END;


PROCEDURE Do (READONLY w      : CWT.TBody;
                       wavelet: Wavelet;
                       width  : Width;
              READONLY scales : V.T;
                       conv   : Conv.T      := NIL; ): S.T =
  VAR synthesis := New(wavelet, width, conv);
  BEGIN
    <* ASSERT NUMBER(w) = NUMBER(scales), "Number of scales must match" *>
    <* ASSERT NUMBER(scales) >= 2,
                "You need at least scales, otherwise differencing won't work." *>
    FOR i := FIRST(scales) TO LAST(scales) DO
      VAR dif: R.T;
      BEGIN
        IF i = FIRST(scales) THEN
          dif := scales[i + 1] - scales[i];
        ELSIF i = LAST(scales) THEN
          dif := scales[i] - scales[i - 1];
        ELSE
          dif := R.Half * (scales[i + 1] - scales[i - 1]);
        END;
        WITH sc = scales[i] DO
          PutScale(synthesis, w[i], sc, dif / (sc * sc * RT.SqRt(sc)));
        END;
      END;
    END;
    RETURN Finish(synthesis);
  END Do;


PROCEDURE New (wavelet: Wavelet; width: Width; conv: Conv.T := NIL; ): T =
  VAR h := NEW(T);
  BEGIN
    <* ASSERT width MOD 2 = 1,
                "'width' must be odd in order to assure "
                  & "that the resulting signal can be represented as Signal.T" *>
    IF conv = NIL THEN conv := NEW(Conv.Fourier); END;
    h.y := NIL;
    h.wavelet := wavelet;
    h.width := width;
    h.conv := conv;
    RETURN h;
  END New;

PROCEDURE PutScale (h: T; w: S.T; scale, amp: R.T; ) =
  VAR
    wav := CWT.DiscretizeWavelet(h.wavelet, scale, amp, h.width);
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
