GENERIC INTERFACE MatchWavelet(R, V, M, S, FnD);


CONST MatchPattern = MatchPatternGenWav;

TYPE
  MatchGenWav = RECORD
                  lift       : S.T;  (* lifting filter *)
                  wavelet0Amp: R.T;  (* coefficient for unlifted wavelet *)
                  approx     : S.T;  (* approximative function *)

                  (* for debugging and visualization purposes *)
                  basis: M.T;    (* basis of generators and the unlifted
                                    wavelet *)
                  targetPad: V.T;  (* the target padded for fitting to the
                                      basis vectors *)
                END;

PROCEDURE MatchPatternGenWav (target                                : S.T;
                              refineMask, generatorMask, waveletMask: S.T;
                              numLevels     : CARDINAL;
                              firstTranslate: INTEGER;
                              numTranslates : CARDINAL; ): MatchGenWav;
(* Approximate 'target' by a discrete wavelet constructed from modified
   generators and a wavelet.  The approximation error is measured with
   squared sums (Euclidean norm).  This corresponds to a lifting step with
   filter 'lift'.  Basis functions are: 'numTranslates' translations
   starting from 'firstTranslate' of 'generatorMask' refined with respect
   to 'refineMask' and a single copy of the 'waveletMask' refined by
   'refineMask'.  We refine over 'numLevels' levels. *)

TYPE
  NormalEqu = RECORD
                mat: M.T;
                vec: V.T;
              END;

PROCEDURE ComputeNormalEqu (target                                : S.T;
                            refineMask, generatorMask, waveletMask: S.T;
                            numLevels     : CARDINAL;
                            firstTranslate: INTEGER;
                            numTranslates : CARDINAL; ): NormalEqu;
(* The linear least squares problem that arises in MatchPatternGenWav could
   be solved more efficiently by normal equations.  I don't know if the
   numerical properties are still good, at least the normal equations can
   be computed efficiently due to the special structure of the matrix. *)


TYPE
  MatchGen = RECORD
               lift  : S.T;
               approx: S.T;

               basis    : M.T;
               targetPad: V.T;
             END;

PROCEDURE MatchPatternGen (target                   : S.T;
                           refineMask, generatorMask: S.T;
                           numLevels                : CARDINAL;
                           firstTranslate           : INTEGER;
                           numTranslates            : CARDINAL; ):
  MatchGen;
(* Like MatchPattern with the difference that an unlifted wavelet can not
   be specified.  This is required in case you want to fix the amplitude of
   the unlifted wavelet.  Then you have to subtract the wavelet from target
   before approximation. *)

PROCEDURE MatchPatternWav (target                 : S.T;
                           refineMask, waveletMask: S.T;
                           numLevels              : CARDINAL; ): R.T;
(* Find the amplitude where the wavelet best matches the target. *)



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


END MatchWavelet.
