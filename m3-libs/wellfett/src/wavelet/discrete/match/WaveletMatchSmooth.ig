GENERIC INTERFACE WaveletMatchSmooth(R, S, FB, WMBasis, WMGrad, RefnSm);

IMPORT Range, Arithmetic;

(* Here we match a wavelet with a pattern while preserving that the
   functions of the opposite basis are sufficiently smooth.

   Known problem: You should normalise the target pattern e.g.  to a
   Euclidean norm of 1, otherwise the smoothness iteration will not
   properly detect convergence. *)

TYPE
  Flag = {Verbose, Plot, Debug};
  FlagSet = SET OF Flag;

  Options =
    RECORD
      smoothness: PROCEDURE (x: S.T): R.T RAISES {Arithmetic.Error} := RefnSm.Frobenius;
      (* Function for estimating smoothness of the primal generator.  Use
         one the functions provided in module RefinableSmooth. *)

      variableWaveletAmplitude := TRUE;
      (* If TRUE, then an appropriate coefficient for the coefficient of
         the unlifted highpass filter is searched.  Otherwise the highpass
         filter is not scaled. *)

      smoothWeightProgress: R.T := 100.0D0;
      (* Solve the regularised optimization for different weights of the
         fractality measurement.  Reduce weighting by a factor of
         smoothWeightProgress per iteration. *)
      maxIter: CARDINAL := 10;
      (* maximal number of weights to be tested subsequently *)
      maxSubIter: CARDINAL := 30;
      (* maximal number of iterations per weight *)

      tol    : R.T := 1.0D-4;
      difDist: R.T := 1.0D-5;
      (* step size for calculation of numerical derivative *)
    END;



PROCEDURE WithPattern
  (target   : S.T;               (* pattern to match *)
   dualBasis: WMBasis.T;         (* generator, vanishing moments *)
   translates: Range.T;          (* range (start and size) of the lifting
                                    filter *)
   numLevels: CARDINAL;          (* number of refinement levels *)
   smoothWeight: R.T;            (* the higher the weight, the more smooth
                                    the opposite generator, the worse the
                                    matching *)
            flags   := FlagSet{};
   READONLY options := Options{}; ): WMGrad.Parameters
  RAISES {Arithmetic.Error};

PROCEDURE BSplineWithPattern
  (target                                 : S.T;
   dualSmooth, primalSmooth, dualVanishing: CARDINAL;
   translates                             : Range.T;
   numLevels                              : CARDINAL;
   smoothWeight                           : R.T;
   flags                                               := FlagSet{};
   READONLY options := Options{}; ): ARRAY [0 .. 1] OF FB.TBody
  RAISES {Arithmetic.Error};
(* Match a wavelet from the CDF family to the 'target' pattern.  The
   corresponding filters are in the dual filter bank (result[1]).  Compare
   WaveletMatchBasis.InitBSpline for more details. *)


END WaveletMatchSmooth.
