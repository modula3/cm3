INTERFACE LongRealMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealVector AS V;
IMPORT LongRealMatrix AS M;
IMPORT LongRealSignal AS S;

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

END LongRealMatchWavelet.
