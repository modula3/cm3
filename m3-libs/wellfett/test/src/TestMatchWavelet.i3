INTERFACE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealVector AS V;
IMPORT LongRealMatrix AS M;

IMPORT LongRealSignal AS S;
IMPORT LongRealBSplineWavelet AS BSpl;
IMPORT LongRealDyadicFilterBank AS FB;

PROCEDURE Test ();

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
                        numLevels, numTranslates              : CARDINAL; ):
  SimpleApprox;

PROCEDURE TestMatchPatternSmooth (target: S.T;
                                  levels, smooth, vanishing,
                                    smallVanishing, translates: CARDINAL;
                                  smoothWeight: R.T):
  ARRAY [0 .. 1] OF FB.T RAISES {BSpl.DifferentParity};

END TestMatchWavelet.
