
GENERIC INTERFACE DiscreteWaveletTransform(S, VS, MS);

IMPORT NADefinitions AS NA;

CONST Brand = "DiscreteWaveletTransform";

PROCEDURE FilterBankToPolyphase (READONLY x: VS.TBody; ): MS.T;
PROCEDURE PolyphaseToFilterBank (READONLY x: MS.TBody; ): VS.T
  RAISES {NA.Error};

PROCEDURE FilterBankAnalysisSingle (x: S.T; READONLY y: VS.TBody; ): VS.T;
(*Transform signal x into NUMBER(y) downsampled channels*)

PROCEDURE FilterBankSynthesisSingle (READONLY x, y: VS.TBody; ): S.T
  RAISES {NA.Error};
(*Transform NUMBER(y) downsampled channels into one signal*)

TYPE
  WaveletCoeffs = RECORD
                    low : S.T;
                    high: MS.T;
                  END;

  DyadicWaveletCoeffs = RECORD
                          low : S.T;
                          high: VS.T;
                        END;

(*
PROCEDURE FilterBankAnalysis (         x        : S.T;
                               READONLY y        : VS.TBody;
                                        numLevels: CARDINAL; ):
  WaveletCoeffs;
*)

PROCEDURE DyadicFilterBankAnalysis (         x: S.T;
                                    READONLY y: ARRAY [0 .. 1] OF S.T;
                                    numLevels: CARDINAL; ):
  DyadicWaveletCoeffs;
(*Transform signal x into 'numLevels' frequency bands with octave distance
   and one low frequency band.*)

PROCEDURE DyadicFilterBankSynthesis (READONLY x: DyadicWaveletCoeffs;
                                     READONLY y: ARRAY [0 .. 1] OF S.T; ):
  S.T;
(*Transform sub bands into the original signal.  The reconstructed signal
   will be longer than the original one but the extra values should be zero
   if the filter bank allows for perfect reconstruction.*)

END DiscreteWaveletTransform.
