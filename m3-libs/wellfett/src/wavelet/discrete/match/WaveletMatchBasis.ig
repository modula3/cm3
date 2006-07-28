GENERIC INTERFACE WaveletMatchBasis(S, FB, WMGrad);

FROM WMGrad IMPORT Parameters;

(*
  If lp and hp are complementary filters,
  then whenever hps = hp + lp <*> upsample(s,2)
  lp and hps are complementary, too.
  We try to find the best lifting filter s for this purpose.
  That is we search the matched wavelet in an affine space
  with offset hp and
  the vector space induced by some convolutional products with lp.

  We also want to have a smooth generator in the opposite wavelet basis,
  thus we must add some vanishing moments to hp.
  In the affine sub-space where all hps have m vanishing moments
  the high pass filters have the form
    hpVans = hp + lpVan <*> upsample(s,2)
  where lpVan = lp*(1,0,-1)^m,
        (lp, hp) is complementary
        and hp has m vanishing moments.
  That is lpVan has m vanishing moments
  but in the form (1,0,-1)^m instead of (1,-1)^m
  because we have to assert that the lifting can be done
  with a "dyadic sparse" filter upsample(s,2).
  Of course the final lifting is done with
  the lowpass filter lp and
  the lifting filter (1,0,-1)^m <*> upsample(s,2).
  This basis is represented by the class 'Standard'.

  Unfortunately the many vanishing moments
  implied by the smoothness of the opposite basis functions
  are often too much for the pattern we want to match.
  Thus we use a modified DWT
  where the application of the vanishing moment factors
  is deferred to the synthesis transform.
  We have to adapt the affine base accordingly.
  Say we start with a basis with m vanishing moments
  where m1 moments are deferred.
  We divide the factor (1,-1)^m1 off the lowpass and the highpass.
  Eventually we ask for the best s with
     hpVans = hpSmallVan + lpSmallVan <*> upsample(s,2)
  This is addressed by the class 'NonVanishing'.

  In principle the standard case can be considered
  as the modified case with 0 deferred moments.
*)

TYPE
  Public = OBJECT
             lp,                 (* --h-- *)
               lpVan,            (* lp*(1,0,-1)^m *)
               lpSmallVan,       (* lp*(1,0,-1)^m0*(1,1)^m1 = lpVan /
                                    (1,-1)^m1 with m=m0+m1 *)
               lpNoVan,          (* lp*(1,1)^m = lpVan / (1,-1)^m *)
               hp,               (* --g0-- lp and hp are complementary *)
               hpSmallVan,       (* hp*(1,-1)^m0 *)
               hpNoVan           (* lpNoVan and hpNoVan are
                                    complementary *)
                                                    : S.T;

             shiftVan: INTEGER;  (* shift lpVan relatively to lp to achieve
                                    a more centered basis *)
             shiftSmallVan: INTEGER;  (* shift hpSmallVan *)

           METHODS
             getRefineMask             (): S.T;
             getLSGeneratorMask        (): S.T;
             getLSWavelet0Mask         (): S.T;
             getLiftedWaveletMaskNoVan (READONLY mc: Parameters; ): S.T;
             getShapeWaveletMask       (READONLY mc: Parameters; ): S.T;
             (* The following two functions are abstract and are overloaded
                in class Standard and NonVanishing. *)
             getFilterBank (READONLY mc: Parameters; ):
                            ARRAY [0 .. 1] OF FB.TBody;
             plotBase (READONLY mc: Parameters; numLevels: CARDINAL; );
           END;

  T <: Public;

  Standard <: T;
  NonVanishing <: T;


VAR
  vanishingAtom: S.T;            (* 1/2 * (1,-1), a factor that provides
                                    one vanishing moment *)
  vanishingAtom2: S.T;           (* 1/2 * (1,0,-1), a factor that provides
                                    one vanishing moment and order of
                                    smoothness *)


PROCEDURE InitBSpline
  (basis: T;
   dualSmooth: CARDINAL;         (* number of smoothness factors (1,1) of
                                    the dual generator *)
   primalSmooth: CARDINAL;       (* number of smoothness factors (1,1) of
                                    the primal generator *)
   dualVanishing: CARDINAL;  (* number of vanishing moments (1,-1) of the
                                dual wavelet, must at most 'vanishing' *)):
  T;
(* Initialise the basis with the filters of the CDF basis and return
   'basis'.  It is asumed that the target pattern is centered around or
   even symmetric to point 0.  Thus the basis is initialized with the
   unlifted wavelet centered around 0.  Please note, that in a discrete
   wavelet basis there is no wavelet centered around 0, but only one
   generator function which is at point 0.  Thus a perfectly centered
   pattern will never occur at point 0 in the basis.  To be more precise,
   the matched high pass filter is translated by 1 to the right with
   respect to the pattern. *)

END WaveletMatchBasis.
