GENERIC MODULE ContinuousWaveletTransform(R, C, CV);


PROCEDURE DiscretizeWavelet (wavelet   : Wavelet;
                             scale, amp: R.T;
                             width     : Width;   ): CV.T =
  VAR
    samples := NEW(CV.T, width);
    center  := LAST(samples^) DIV 2;
  BEGIN
    FOR i := FIRST(samples^) TO LAST(samples^) DO
      samples[i] :=
        C.Scale(wavelet(R.FromInteger(i - center) / scale), amp);
    END;
    RETURN samples;
  END DiscretizeWavelet;


BEGIN
END ContinuousWaveletTransform.
