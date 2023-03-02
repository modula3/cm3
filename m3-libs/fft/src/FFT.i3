INTERFACE FFT;

IMPORT LongRealComplex AS LC;

TYPE
  FFTArr = REF ARRAY OF LC.T;

  T <: Fft;
  Fft = OBJECT
  METHODS
    forward(data : FFTArr);
    inverse(data : FFTArr; scale := TRUE);
  END;

END FFT.
