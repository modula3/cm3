GENERIC MODULE ContinuousWaveletTransform(R, RT, C, CV);


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


BEGIN
END ContinuousWaveletTransform.
