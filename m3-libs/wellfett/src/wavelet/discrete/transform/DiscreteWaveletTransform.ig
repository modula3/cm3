
GENERIC INTERFACE DiscreteWaveletTransform(S, SV, SVR, SM, FB);

CONST Brand = "DiscreteWaveletTransform";


TYPE
  T = RECORD
        low : S.T;
        high: SM.T;
      END;


PROCEDURE SingleFromSignal (         x      : S.T;
                            READONLY filter : FB.TBody;
                                     scaling: S.ScalingType; ): SV.T;
(* Transform signal x into NUMBER(filter) channels downsampled by
   'scaling'. *)

PROCEDURE SingleToSignal (READONLY x      : SV.TBody;
                          READONLY filter : FB.TBody;
                                   scaling: S.ScalingType; ): S.T;
(* Transform NUMBER(filter) downsampled channels into one signal.
   Precondition: There must be as many filters as signal channels. *)

<* INLINE *>
PROCEDURE SingleShiftInvariantFromSignal (         x     : S.T;
                                          READONLY filter: FB.TBody; ):
  SV.T;

CONST
  SingleShiftInvariantToSignal: PROCEDURE (READONLY x     : SV.TBody;
                                           READONLY filter: FB.TBody; ):
                                  S.T = SVR.Dot;

<* OBSOLETE *>
PROCEDURE FromSignal (x: S.T; READONLY y: FB.TBody; numLevels: CARDINAL; ):
  T;
(* I'm uncertain about the most natural generalisation to more than two
   channels. *)


END DiscreteWaveletTransform.
