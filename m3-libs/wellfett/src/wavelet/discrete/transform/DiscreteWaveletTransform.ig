
GENERIC INTERFACE DiscreteWaveletTransform(S, VS, VSR, MS);

IMPORT NADefinitions AS NA;

CONST Brand = "DiscreteWaveletTransform";

PROCEDURE FilterBankToPolyphase (READONLY x: VS.TBody; step: CARDINAL; ):
  MS.T;
(*'step' is the factor of sub-sampling which may differ from the number of
   channels, in that case the polyphase matrix is not square.*)

PROCEDURE PolyphaseToFilterBank (READONLY x: MS.TBody; ): VS.T;
(*step=NUMBER(x[0])*)

PROCEDURE FilterBankAnalysisSingle (         x   : S.T;
                                    READONLY y   : VS.TBody;
                                             step: CARDINAL; ): VS.T;
(*Transform signal x into NUMBER(y) channels downsampled by 'step'*)

PROCEDURE FilterBankSynthesisSingle (READONLY x, y: VS.TBody;
                                              step: CARDINAL; ): S.T
  RAISES {NA.Error};
(*Transform NUMBER(y) downsampled channels into one signal*)

CONST
  FilterBankAnalysisTISingle = VSR.Scale; (*the order of operands is not
                                             natural*)
  FilterBankSynthesisTISingle = VSR.Inner; (*there is an adjoint too much
                                              in it*)

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
