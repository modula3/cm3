GENERIC INTERFACE MatchWaveletGradient(R, V, M, S, FnD);

(* Following functions determine how much a certain value changes if one
   alters the lifting filter 's'.  That is they (numerically) differentiate
   the value with respect to the lifting filter 's'. *)

PROCEDURE DeriveDist (normalMat    : M.T;
                      targetCor    : V.T;
                      targetNormSqr: R.T;
                      s            : S.T; ): FnD.T;
(* 'normalMat' is the matrix of the normal equations, i.e.  it contains the
   cross correlations between the lifting basis filters, i.e.  one version
   of the generator mask and several translated versions of the basic
   wavelet mask.  'targetCor' contains the correlations of the lifting
   basis with the target vector.  'targetNormSqr' contains the square of
   the Euclidean norm of the target.  Instead of these three parameters we
   could also implement the function with the inputs 'hDual', 'gDual0',
   'target'.  But we stick to the preprocessed data for efficiency
   reasons. *)

PROCEDURE DeriveSSE (hDual, gDual0, s: S.T; ): FnD.T;
(* The generator mask 'hDual', a basic wavelet mask 'gDual0' and the
   lifting filter 's' form a new wavelet mask, say 'gDual'.  Derive 'gDual'
   with respect to the lifting filter 's'. *)

PROCEDURE DeriveWSSE (hDual, gDual0, s: S.T; c: R.T): FnD.T;
(* In the fitting routine the expression hDual*s+gDual0*c is fitted to the
   target.  But the wavelet won't be smoother if s and c become smaller.
   Instead hDual/c*s+gDual0, or more precisely gPrimal/c*s+hPrimal0, is the
   quantity we have to apply our estimate to.

   The whole computation is horribly inefficient but this is for research
   only! *)


END MatchWaveletGradient.
