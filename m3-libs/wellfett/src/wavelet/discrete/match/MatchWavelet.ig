GENERIC INTERFACE MatchWavelet(R, V, M, S);

TYPE
  SimpleApprox = RECORD
                   lift       : S.T;
                   wavelet0Amp: R.T;
                   approx     : S.T;  (*approximative function*)

                   basis    : M.T;
                   targetPad: V.T;
                 END;

PROCEDURE MatchPattern (target                                : S.T;
                        refineMask, generatorMask, waveletMask: S.T;
                        numLevels                             : CARDINAL;
                        firstTranslate                        : INTEGER;
                        numTranslates                         : CARDINAL; ):
  SimpleApprox;

END MatchWavelet.
