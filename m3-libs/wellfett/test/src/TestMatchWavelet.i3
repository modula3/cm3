INTERFACE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealBSplineWavelet AS BSpl;
IMPORT LongRealSignal AS S;
IMPORT LongRealDyadicFilterBank AS FB;

PROCEDURE Test ();

TYPE
  SimpleApprox = RECORD
                   lift       : S.T;
                   wavelet0Amp: R.T;
                   approx     : S.T;  (*approximative function*)
                 END;

PROCEDURE MatchPattern (target                               : S.T;
                        levels, smooth, vanishing, translates: CARDINAL;
                        plot := TRUE; ): SimpleApprox
  RAISES {BSpl.DifferentParity};

PROCEDURE TestMatchPatternSmooth (target: S.T;
                                  levels, smooth, vanishing,
                                    smallVanishing, translates: CARDINAL;
                                  smoothWeight: R.T):
  ARRAY [0 .. 1] OF FB.T RAISES {BSpl.DifferentParity};

END TestMatchWavelet.
