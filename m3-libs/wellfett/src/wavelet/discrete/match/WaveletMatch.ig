GENERIC INTERFACE WaveletMatch(R, V, M, S);

IMPORT Range;


TYPE
  T = RECORD
        lift       : S.T;        (* lifting filter *)
        wavelet0Amp: R.T;        (* coefficient for unlifted wavelet *)
        approx     : S.T;        (* approximative function *)
      END;


PROCEDURE WithPattern (target                                : S.T;
                       refineMask, generatorMask, waveletMask: S.T;
                       translates                            : Range.T;
                       numLevels                             : CARDINAL; ):
  T;
(* Approximate 'target' by a discrete wavelet constructed from modified
   generators and a wavelet.  The approximation error is measured with
   squared sums (Euclidean norm).  This corresponds to a lifting step with
   filter 'lift'.  Basis functions are: 'numTranslates' translations
   starting from 'firstTranslate' of 'generatorMask' refined with respect
   to 'refineMask' and a single copy of the 'waveletMask' refined by
   'refineMask'.  We refine over 'numLevels' levels. *)


TYPE
  Generator = RECORD
                lift  : S.T;
                approx: S.T;
              END;

PROCEDURE WithPatternGenerator (target                   : S.T;
                                refineMask, generatorMask: S.T;
                                translates               : Range.T;
                                numLevels                : CARDINAL; ):
  Generator;
(* Like WithPattern with the difference that an unlifted wavelet can not be
   specified.  This is required in case you want to fix the amplitude of
   the unlifted wavelet.  Then you have to subtract the wavelet from target
   before approximation. *)

PROCEDURE WithPatternWavelet
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ): R.T;
(* Find the amplitude where the wavelet best matches the target. *)


(* two different implementations of WithPatternWavelet, for test purposes
   only *)
TYPE
  InternalWaveletMatch =
    RECORD
      targetCor: R.T;            (* correlation of target pattern with
                                    wavelet *)
      waveletNorm: R.T;          (* square of the norm of the wavelet *)
    END;

PROCEDURE WithPatternWaveletSimple
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ):
  InternalWaveletMatch;

PROCEDURE WithPatternWaveletFast
  (target: S.T; refineMask, waveletMask: S.T; numLevels: CARDINAL; ):
  InternalWaveletMatch;






PROCEDURE TranslatesBasis
  (x: S.T; clip: Range.T; translates: Range.T; unit: CARDINAL; ): M.T;
(* Creates a matrix where each row is a translate of x, where it is
   translated in 'unit' steps. *)


TYPE
  LeastSquaresProblem = RECORD
                          clip: Range.T;  (* the index range of the clip
                                             represented by the vectors in
                                             basis and targetPad *)
                          basis: M.T;  (* basis of generators and the
                                          unlifted wavelet *)
                          targetPad: V.T;  (* the target padded for fitting
                                              to the basis vectors *)
                        END;

PROCEDURE ComputeLeastSquaresProblem
  (target                                : S.T;
   refineMask, generatorMask, waveletMask: S.T;
   translates                            : Range.T;
   numLevels                             : CARDINAL; ):
  LeastSquaresProblem;
(* Setup the least squares problem that is solved by
   WaveletMatch.WithPattern. *)


TYPE
  NormalEqu = RECORD
                mat: M.T;
                vec: V.T;
              END;

PROCEDURE ComputeNormalEqu (target                                : S.T;
                            refineMask, generatorMask, waveletMask: S.T;
                            translates: Range.T;
                            numLevels : CARDINAL; ): NormalEqu;
(* The linear least squares problem that arises in WithPatternGenWav could
   be solved more efficiently by normal equations.  I don't know if the
   numerical properties are still good, at least the normal equations can
   be computed efficiently due to the special structure of the matrix. *)


END WaveletMatch.
