GENERIC INTERFACE DaubechiesWavelet(S);

(*the wavelet filter convoluted with its adjungate,
  to obtain the filter itself you have to apply spectral factorization*)
PROCEDURE FilterAbsSqr(n : CARDINAL) : S.T;

(*the wavelet filter convoluted with its adjungate
  without the factor (1,1)^m that corresponds to m zeros at -1*)
PROCEDURE FilterPureAbsSqr(n : CARDINAL) : S.T;

END DaubechiesWavelet.
