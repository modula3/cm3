
GENERIC INTERFACE DiscreteWaveletTransform(S, SV, SVR, SM);

IMPORT Arithmetic AS Arith;

CONST Brand = "DiscreteWaveletTransform";

TYPE
  IndexType = INTEGER;
  SizeType = CARDINAL;
  ScalingType = CARDINAL;
  SignalPP = ARRAY OF S.T;

PROCEDURE FilterBankToPolyphase (READONLY x      : SV.TBody;
                                          scaling: ScalingType; ): SM.T;
(* 'scaling' is the factor of sub-sampling which may differ from the number
   of channels, in that case the polyphase matrix is not square. *)

PROCEDURE PolyphaseToFilterBank (READONLY x: SM.TBody; ): SV.T;
(* scaling=NUMBER(x[0]) *)

PROCEDURE FilterBankAnalysisSingle (         x      : S.T;
                                    READONLY filter : SV.TBody;
                                             scaling: ScalingType; ): SV.T;
(* Transform signal x into NUMBER(y) channels downsampled by 'scaling'. *)

PROCEDURE FilterBankSynthesisSingle (READONLY x, filter: SV.TBody;
                                              scaling  : ScalingType; ):
  S.T RAISES {Arith.Error};
(* Transform NUMBER(y) downsampled channels into one signal.  Make sure
   that there are as many filters as signal channels. *)

<* INLINE *>
PROCEDURE FilterBankAnalysisTISingle (x: S.T; READONLY filter: SV.TBody; ):
  SV.T;

CONST
  FilterBankSynthesisTISingle: PROCEDURE (READONLY x, filter: SV.TBody; ):
                                 S.T RAISES {Arith.Error} = SVR.Dot;

TYPE
  WaveletCoeffs = RECORD
                    low : S.T;
                    high: SM.T;
                  END;

  DyadicWaveletCoeffs = RECORD
                          low : S.T;
                          high: SV.T;
                        END;

(*
PROCEDURE FilterBankAnalysis (         x        : S.T;
                               READONLY y        : SV.TBody;
                                        numLevels: CARDINAL; ):
  WaveletCoeffs;
*)

PROCEDURE DyadicFilterBankAnalysis (         x: S.T;
                                    READONLY y: ARRAY [0 .. 1] OF S.T;
                                    numLevels: CARDINAL; ):
  DyadicWaveletCoeffs;
(* Transform signal x into 'numLevels' frequency bands with octave distance
   and one low frequency band.*)

PROCEDURE DyadicFilterBankSynthesis (READONLY x: DyadicWaveletCoeffs;
                                     READONLY y: ARRAY [0 .. 1] OF S.T; ):
  S.T;
(* Transform sub bands into the original signal.  The reconstructed signal
   will be longer than the original one but the extra values should be zero
   if the filter bank allows for perfect reconstruction.*)

PROCEDURE DyadicFilterBankAnalysisTI (         x: S.T;
                                      READONLY y: ARRAY [0 .. 1] OF S.T;
                                      numLevels: CARDINAL; ):
  DyadicWaveletCoeffs;
(* Translation invariant transform, this is achieved by omitting the
   subsampling.*)

PROCEDURE DyadicFilterBankSynthesisTI (READONLY x: DyadicWaveletCoeffs;
                                       READONLY y: ARRAY [0 .. 1] OF S.T; ):
  S.T;

END DiscreteWaveletTransform.
