GENERIC INTERFACE MatchWavelet(R, V, M, S);

CONST MatchPattern = MatchPatternGenWav;

TYPE
  MatchGenWav = RECORD
                  lift       : S.T;  (*lifting filter*)
                  wavelet0Amp: R.T;  (*coefficient for unlifted wavelet*)
                  approx     : S.T;  (*approximative function*)

                  (*for debugging and visualization purposes*)
                  basis: M.T;    (*basis of generators and the unlifted
                                    wavelet*)
                  targetPad: V.T;  (*the target padded for fitting to the
                                      basis vectors*)
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
(*Like MatchPattern with the difference that an unlifted wavelet can not be
   specified.  This is required in case you want to fix the amplitude of
   the unlifted wavelet.  Then you have to subtract the wavelet from target
   before approximation. *)

PROCEDURE MatchPatternWav (target                 : S.T;
                           refineMask, waveletMask: S.T;
                           numLevels              : CARDINAL; ): R.T;
(*Find the amplitude where the wavelet best matches the target.*)


END MatchWavelet.
