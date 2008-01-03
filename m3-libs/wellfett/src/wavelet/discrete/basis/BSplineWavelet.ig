GENERIC INTERFACE BSplineWavelet(S);

(* Cohen-Daubechies-Feauveau's Wavelet Basis *)

PROCEDURE GeneratorMask (n: CARDINAL; base: [1 .. LAST(CARDINAL)] := 2):
  S.T;

PROCEDURE WaveletMask (n, m: CARDINAL): S.T;
(* The smoothness order n and the number of vanishing moments m of the
   wavelet must be both odd or both even. *)

PROCEDURE WaveletMaskNoVan (n, m: CARDINAL): S.T;

END BSplineWavelet.
