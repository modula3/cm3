GENERIC INTERFACE BSplineWavelet(S);

(* Cohen-Daubechies-Feauveau's Wavelet Basis *)

EXCEPTION
  DifferentParity;               (*for the wavelet mask, n and m must be
                                    both odd or both even*)

PROCEDURE GeneratorMask (n: CARDINAL; base: CARDINAL): S.T;

PROCEDURE WaveletMask (n, m: CARDINAL): S.T RAISES {DifferentParity};

END BSplineWavelet.
