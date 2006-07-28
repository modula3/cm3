GENERIC INTERFACE WaveletMatchGradient(R, V, S, FnD);

IMPORT Wr, Thread;

(*
We want to find the parameters which minimize
  || wavelet0Amp*waveletO + lift <*> generator - targetAmp*pattern ||_2

Given the zeroth, first and second derivatives with respect to 'lift'
we compute these derivatives when the coefficients wavelet0Amp and targetAmp
are considered, too.

FixedWaveletAmplitude handles the case where the coefficient
of the base wavelet is fixed,
thus it is not subject of optimization and its derivative is zero.

VariableWaveletAmplitude handles the case
where the coefficient of the base wavelet must be determined by the optimizer.
*)


TYPE
  (* These are the parameters which describe the matching.

     lift and wavelet0Amp are also part of MatchWavelet.MatchGenWav *)
  Parameters = RECORD
                 lift: S.T;      (* lifting filter which lifts from the
                                    base wavelet to the pattern matched
                                    wavelet *)
                 wavelet0Amp: R.T;  (* coefficient for the base wavelet
                                       function *)
                 targetAmp: R.T;  (* coefficient for the target function *)
               END;

TYPE
  T =
    OBJECT
      yFirst: INTEGER;
    METHODS
      splitParamVec (x: V.T): Parameters;
      deriveRegularized (         derDist: FnD.T;
                         READONLY mc     : Parameters;
                         waveletVec, waveletCor, targetVec: V.T; ): FnD.T;
    END;

  VariableWaveletAmplitude <: T;

  FixedWaveletAmplitude <: FixedWaveletAmplitudePublic;
  FixedWaveletAmplitudePublic = T OBJECT wavAmp: R.T END;


(* extend derivatives by information that are contributed by optimizing the
   amplitude of the target *)
PROCEDURE ExtendDervTarget (READONLY x        : FnD.T;
                                     lift     : V.T;
                                     targetAmp: R.T;
                                     target   : V.T;
                                     targetCor: V.T;
                                     wavelet0 : V.T    ): FnD.T;

(* This is for testing only.  Show the differential and the difference
   quotients of a functional.  It's probably better to turn this into a
   routine preparing a TEXT (that is a Fmt like routine). *)
PROCEDURE PutDervDif (READONLY der   : FnD.T;
                      READONLY derArr: ARRAY OF FnD.T;
                               delta : R.T             )
  RAISES {Thread.Alerted, Wr.Failure};

END WaveletMatchGradient.
