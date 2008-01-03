MODULE TestWaveletMatch;

IMPORT LongRealBasic       AS R,
       LongRealTrans       AS RT,
       LongRealVector      AS V,
       LongRealVectorTrans AS VT,
       LongRealSignal      AS S;

IMPORT LongRealMatrix       AS M,
       LongRealMatrixTrans  AS MT,
       LongRealMatrixLapack AS LA;

IMPORT LongRealFunctionalDeriv2         AS FnD,
       LongRealRefinableFunction        AS Refn,
       LongRealRefinableSmooth          AS RefnSm,
       LongRealBSplineWavelet           AS BSpline,
       LongRealWaveletMatchGradient     AS WMGrad,
       LongRealWaveletMatchGradientLift AS WMGradLift,
       LongRealWaveletMatch             AS WaveletMatch,
       LongRealWaveletMatchSmooth       AS WaveletMatchSmooth;

IMPORT LongRealFmtLex       AS RF,
       LongRealVectorFmtLex AS VF,
       LongRealMatrixFmtLex AS MF,
       LongRealSignalFmtLex AS SF;

IMPORT UnitTest, UnitTestList, UnitTestSignal;

IMPORT Fmt, Wr, Thread, Atom;
IMPORT Arithmetic, Range;
(* IMPORT LongRealPLPlot AS PL; *)


PROCEDURE CheckWaveletMatch (SELF: UnitTestSignal.T; ) =
  <* FATAL Arithmetic.Error *>
    (* The iteration in WaveletMatchSmooth.BSplineWithPattern is not
       employed, thus it cannot diverge. *)

  CONST
    numLevels = 7;

  BEGIN
    (* PL.Init(); *)
    (* A wavelet function must be perfectly matched. *)
    FOR i := 0 TO 6 DO
      FOR j := i MOD 2 TO 8 BY 2 DO
        FOR k := j MOD 2 TO j BY 2 DO
          IF NOT (i = 0 AND k = 0) THEN

            WITH lp       = BSpline.GeneratorMask(i),
                 hpTarget = BSpline.WaveletMask(i, j),
                 target = Refn.Refine(hpTarget.translate(-1),
                                      lp.scale(2.0D0), numLevels),
                 filterBank = WaveletMatchSmooth.BSplineWithPattern(
                                target, i, k, k, Range.New(-5, 10),
                                numLevels, R.Zero,
                                (* WaveletMatchSmooth.FlagSet{
                                   WaveletMatchSmooth.Flag.Plot}, *)
                                options :=
                                  WaveletMatchSmooth.Options{maxIter := 0}),
                 hpMatch = filterBank[1, 1] DO

              IF NOT SELF.signalMatch(hpMatch, hpTarget, 2.0D-13) THEN
                SELF.message(
                  Fmt.FN(
                    "CDF-%s,%s with CDF-%s,%s as unlifted wavelet\n",
                    ARRAY OF
                      TEXT{Fmt.Int(i), Fmt.Int(j), Fmt.Int(i), Fmt.Int(k)}));
              END;

            END;
          END;
        END;
      END;
    END;
    (* PL.Exit(); *)
  END CheckWaveletMatch;

PROCEDURE CheckSmoothness (SELF: UnitTestSignal.T; ) =

  CONST
    numLevels    = 7;
    dualSmooth   = 2;
    primalSmooth = 2;
    addSmooth    = 2;

  BEGIN
    (* If smoothness is weighted highly then the pattern shouldn't play a
       role and we obtain a primal generator with higher smoothness order.
       But there seems to be a wavelet base where the primal generator is
       slightly smoother than that of the CDF base. *)
    (* PL.Init(); *)
    TRY
      WITH lp = BSpline.GeneratorMask(dualSmooth),
           hpTarget = BSpline.WaveletMask(
                        dualSmooth, primalSmooth + addSmooth),
           (* lpPrimal =
              FilterBank.DualHighPassToPrimalLowPass(hpTarget), *)
           target = Refn.Refine(hpTarget.translate(-1).superpose(S.One),
                                lp.scale(2.0D0), numLevels),
           filterBank = WaveletMatchSmooth.BSplineWithPattern(
                          target, dualSmooth, primalSmooth, primalSmooth,
                          Range.New(-addSmooth DIV 2, addSmooth),
                          numLevels, 1.0D4,
                          (* WaveletMatchSmooth.FlagSet{
                             WaveletMatchSmooth.Flag.Verbose,
                             WaveletMatchSmooth.Flag.Plot}, *)
                          options := WaveletMatchSmooth.Options{
                                       maxIter := 40, smoothness :=
                                       RefnSm.SpectralRadius,
                                       smoothWeightProgress := 1.5D0}),
           hpMatch = filterBank[1, 1] DO

        IF NOT SELF.signalMatch(hpMatch, hpTarget, 1.0D-1) THEN
          (* SELF.signalMatch( hpMatch.scale(VT.Norm1(hpTarget.getData()) /
             VT.Norm1( hpMatch.getData())), hpTarget, 2.0D-13) *)
          SELF.message(
            Fmt.FN("CDF-%s,%s smoothed to CDF-%s,%s\n",
                   ARRAY OF
                     TEXT{Fmt.Int(dualSmooth), Fmt.Int(primalSmooth),
                          Fmt.Int(dualSmooth),
                          Fmt.Int(primalSmooth + addSmooth)}));
        END;

      END;
    EXCEPT
      (* NoConvergence *)
    | Arithmetic.Error (err) => SELF.error(Atom.ToText(err.head) & "\n");
    END;
    (* PL.Exit(); *)
  END CheckSmoothness;

PROCEDURE CheckNormalEquation (SELF: UnitTestSignal.T; ) =
  CONST
    numLevels    = 7;
    translates   = Range.T{-6, 9};
    realFmtStyle = RF.FmtStyle{prec := 5};

  VAR
    target := Refn.Refine(S.One, BSpline.GeneratorMask(4).scale(2.0D0),
                          numLevels).translate(30);

    refineMask    := NEW(S.T).fromArray(ARRAY OF R.T{0.9D0, 1.1D0, 0.7D0});
    generatorMask := NEW(S.T).fromArray(ARRAY OF R.T{0.7D0, 0.0D0, 0.8D0});
    waveletMask := NEW(S.T).fromArray(
                     ARRAY OF R.T{-0.5D0, 0.2D0, 0.9D0}, -1);

    lsqr := WaveletMatch.ComputeLeastSquaresProblem(
              target, refineMask, generatorMask, waveletMask, translates,
              numLevels);
    covar     := M.MulMMA(lsqr.basis);
    targetCor := M.MulV(lsqr.basis, lsqr.targetPad);

    normEqu := WaveletMatch.ComputeNormalEqu(
                 target, refineMask, generatorMask, waveletMask,
                 translates, numLevels);

  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    IF NOT SELF.matrixMatch(covar, normEqu.mat, MT.Norm1(covar) * 1.0D-15)
         OR NOT SELF.vectorMatch(
                  targetCor, normEqu.vec, VT.Norm1(targetCor) * 1.0D-16) THEN
      SELF.message(Fmt.F(
                     "naively computed normal matrix:\n%s\n"
                       & "correlation with target:\n%s\n",
                     MF.Fmt(covar,
                            style := MF.FmtStyle{width := 10, elemStyle :=
                                                 realFmtStyle}),
                     VF.Fmt(
                       targetCor,
                       style := VF.FmtStyle{elemStyle := realFmtStyle})));
      SELF.message(
        Fmt.F("fast computed normal matrix:\n%s\n"
                & "correlation with target:\n%s\n",
              MF.Fmt(
                normEqu.mat, style := MF.FmtStyle{width := 10, elemStyle :=
                                                  realFmtStyle}),
              VF.Fmt(normEqu.vec,
                     style := VF.FmtStyle{elemStyle := realFmtStyle})));
    END;
  END CheckNormalEquation;

PROCEDURE CheckWaveletMatchWithPatternWavelet (SELF: UnitTestSignal.T; ) =
  CONST numLevels = 7;

  VAR
    target := Refn.Refine(S.One, BSpline.GeneratorMask(4).scale(2.0D0),
                          numLevels).translate(30);

    refineMask := NEW(S.T).fromArray(ARRAY OF R.T{0.9D0, 1.1D0, 0.7D0});
    waveletMask := NEW(S.T).fromArray(
                     ARRAY OF R.T{-0.5D0, 0.2D0, 0.9D0}, -1);

    match0 := WaveletMatch.WithPatternWaveletSimple(
                target, refineMask, waveletMask, numLevels);
    match1 := WaveletMatch.WithPatternWaveletFast(
                target, refineMask, waveletMask, numLevels);

  BEGIN
    IF NOT SELF.scalarMatch(
             match0.targetCor, match1.targetCor,
             (ABS(match0.targetCor) + ABS(match1.targetCor)) * 1.0D-15) THEN
      SELF.message("error in targetCor\n");
    END;
    IF NOT SELF.scalarMatch(
             match0.waveletNorm, match1.waveletNorm,
             (ABS(match0.waveletNorm) + ABS(match1.waveletNorm)) * 1.0D-15) THEN
      SELF.message("error in waveletNorm\n");
    END;
  END CheckWaveletMatchWithPatternWavelet;


(** SSE  - Square Smoothness Estimate,
           that is the estimate that depends on the sum of the squares of the eigenvalues
    WSSE - Weighted Square Smoothness Estimate,
           workaround that takes into account that the hpDual0 filter (alias waveletDual0)
           was amplified within the linear least squares term
*)

TYPE
  UnitTestSSE =
    UnitTestSignal.T OBJECT
      filter: V.T;
    METHODS
      init (READONLY filter: V.TBody; ): UnitTestSSE := InitSSE;
    OVERRIDES
      test := CheckSSE;
    END;

PROCEDURE InitSSE (SELF: UnitTestSSE; READONLY filter: V.TBody; ):
  UnitTestSSE =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    SELF.filter := V.FromArray(filter);
    RETURN UnitTestSignal.T.init(SELF, VF.Fmt(SELF.filter));
  END InitSSE;

PROCEDURE CheckSSE (SELF: UnitTestSSE; ) =
  VAR
    x   := SELF.filter;
    dif := RT.SqRt(RT.Eps);
    dx0 := V.FromArray(ARRAY OF R.T{dif, 0.0D0, 0.0D0});
    dx1 := V.FromArray(ARRAY OF R.T{0.0D0, dif, 0.0D0});
    dx2 := V.FromArray(ARRAY OF R.T{0.0D0, 0.0D0, dif});

  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    WITH rho  = RefnSm.ComputeSSE(x^),
         rho0 = RefnSm.ComputeSSE(V.Add(x, dx0)^),
         rho1 = RefnSm.ComputeSSE(V.Add(x, dx1)^),
         rho2 = RefnSm.ComputeSSE(V.Add(x, dx2)^),
         difRho = V.FromArray(
                    ARRAY OF R.T{rho0 - rho, rho1 - rho, rho2 - rho}),
         gradRho = V.Scale(RefnSm.ComputeDSSE(x^), dif) DO

      IF NOT SELF.vectorMatch(difRho, gradRho, 1.0D-12) THEN
        SELF.message(Fmt.F("rho %s\n", RF.Fmt(rho)));
      END;
    END;

    WITH gradRho  = RefnSm.ComputeDSSE(x^),
         gradRho0 = RefnSm.ComputeDSSE(V.Add(x, dx0)^),
         gradRho1 = RefnSm.ComputeDSSE(V.Add(x, dx1)^),
         gradRho2 = RefnSm.ComputeDSSE(V.Add(x, dx2)^),
         difGradRho = M.FromArray(
                        ARRAY [0 .. 2] OF
                          ARRAY [0 .. 2] OF R.T{V.Sub(gradRho0, gradRho)^,
                                                V.Sub(gradRho1, gradRho)^,
                                                V.Sub(gradRho2, gradRho)^}),
         jacobiRho = M.Scale(RefnSm.ComputeDDSSE(x^), dif) DO

      IF NOT SELF.matrixMatch(difGradRho, jacobiRho, 1.0D-12) THEN
        SELF.message(Fmt.F("gradRho %s\n", VF.Fmt(gradRho)));
      END;
    END;
  END CheckSSE;

PROCEDURE InverseDSSE (x: V.T): V.T RAISES {Arithmetic.Error} =
  (* Find the parameter vector y for which DSSE(y)=x *)
  CONST tol = 1.0D-14;
  (* VAR y := V.New(3); *)
  VAR y := x;                    (* zero is a bad initial value *)
  BEGIN
    FOR j := 0 TO 100 DO
      WITH ax = RefnSm.ComputeDSSE(y^) DO
        IF VT.Norm1(V.Sub(ax, x)) <= tol * VT.Norm1(x) THEN RETURN y; END;
        y := V.Add(y, LA.LeastSquares(RefnSm.ComputeDDSSE(y^),
                                      ARRAY OF V.T{V.Sub(x, ax)})[0].x);
        (*
          SELF.message(Fmt.FN("y %s, DSSE(y) %s\n",
                       ARRAY OF TEXT{VF.Fmt(y), VF.Fmt(RefnSm.ComputeDSSE(y^))}));
        *)
      END;
    END;
    RAISE Arithmetic.Error(NEW(Arithmetic.ErrorNoConvergence).init());
  END InverseDSSE;

TYPE
  UnitTestInverseDSSE = UnitTestSignal.T OBJECT
                          filter: V.T;
                        METHODS
                          init (READONLY filter: ARRAY [0 .. 2] OF R.T; ):
                                UnitTestInverseDSSE := InitInverseDSSE;
                        OVERRIDES
                          test := CheckInverseDSSE;
                        END;

PROCEDURE InitInverseDSSE
  (SELF: UnitTestInverseDSSE; READONLY filter: ARRAY [0 .. 2] OF R.T; ):
  UnitTestInverseDSSE =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    SELF.filter := V.FromArray(filter);
    RETURN UnitTestSignal.T.init(SELF, VF.Fmt(SELF.filter));
  END InitInverseDSSE;


PROCEDURE CheckInverseDSSE (SELF: UnitTestInverseDSSE; ) =
  <* FATAL Arithmetic.Error, Thread.Alerted, Wr.Failure *>
  VAR
    x  := SELF.filter;
    y  := InverseDSSE(x);
    x1 := RefnSm.ComputeDSSE(y^);
  BEGIN
    IF NOT SELF.vectorMatch(x, x1, 1.0D-13) THEN
      SELF.message(Fmt.F("y %s\n", VF.Fmt(y)));
    END;
  END CheckInverseDSSE;

PROCEDURE CheckDeriveWSSE (SELF: UnitTestSignal.T; ) =
  VAR
    dif    := RT.SqRt(RT.Eps);
    lpDual := NEW(S.T).fromArray(ARRAY OF R.T{0.23D0, 1.678D0, -0.85D0});
    hpDual0 := NEW(S.T).fromArray(ARRAY OF R.T{0.723D0, -1.078D0, 0.585D0});
    s      := NEW(S.T).fromArray(ARRAY OF R.T{0.2D0, -0.3D0, 0.1D0});
    sDif   := NEW(S.T).fromArray(ARRAY OF R.T{dif}, s.getFirst());
    c      := 0.73D0;
    der    := WMGradLift.DeriveWSSE(lpDual, hpDual0, s, c);
    derArr := NEW(REF ARRAY OF FnD.T, s.getNumber() + 1);

  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    FOR i := 0 TO s.getNumber() - 1 DO
      derArr[i] := WMGradLift.DeriveWSSE(
                     lpDual, hpDual0, s.superpose(sDif.translate(i)), c);
    END;
    derArr[LAST(derArr^)] :=
      WMGradLift.DeriveWSSE(lpDual, hpDual0, s, c + dif);

    (* compare first derivative (gradient) with the first difference *)
    WITH derFirst = V.Scale(der.first, dif),
         difFirst = V.New(NUMBER(derArr^))   DO
      FOR i := FIRST(derArr^) TO LAST(derArr^) DO
        difFirst[i] := derArr[i].zeroth - der.zeroth;
      END;
      IF NOT SELF.vectorMatch(difFirst, derFirst, 1.0D-11) THEN
        WMGrad.PutDervDif(der, derArr^, dif);
      END;
    END;

    (* compare second derivative with the first difference of the first
       derivative *)
    WITH derSecond = M.Scale(der.second, dif),
         difSecond = M.New(NUMBER(derArr^), NUMBER(derArr^)) DO
      FOR i := FIRST(derArr^) TO LAST(derArr^) DO
        difSecond[i] := V.Sub(derArr[i].first, der.first)^;
      END;
      IF NOT SELF.matrixMatch(difSecond, derSecond, 1.0D-11) THEN
        WMGrad.PutDervDif(der, derArr^, dif);
      END;
    END;
  END CheckDeriveWSSE;



PROCEDURE Test (): UnitTest.T =
  VAR
    testsSSE := NEW(UnitTestList.T).init(
                  "exact gradient of the Square Smoothness Estimate",
                  ARRAY OF
                    UnitTest.T{
                    NEW(UnitTestSSE).init(V.TBody{0.9D0, 0.7D0, -0.6D0}),
                    NEW(UnitTestSSE).init(V.TBody{1.0D0, 1.0D0, 1.0D0})});
    testsInverseDSSE := NEW(UnitTestList.T).init(
                          "inversion Square Smoothness Estimate using Newton method",
                          ARRAY OF
                            UnitTest.T{NEW(UnitTestInverseDSSE).init(
                                         V.TBody{0.9D0, 0.7D0, -0.6D0}),
                                       NEW(UnitTestInverseDSSE).init(
                                         V.TBody{1.0D0, 1.0D0, 0.1D0}),
                                       NEW(UnitTestInverseDSSE).init(
                                         V.TBody{1.0D0, 1.0D0, 1.0D0})});

  BEGIN
    RETURN
      NEW(UnitTestList.T).init(
        "wavelet match",
        ARRAY OF
          UnitTest.T{
          NEW(UnitTestSignal.T, test := CheckNormalEquation).init(
            "optimized computation of normal equation"),
          NEW(UnitTestSignal.T,
              test := CheckWaveletMatchWithPatternWavelet).init(
            "optimized computation of amplitude adaption"
              & " of a wavelet to a pattern"), testsSSE, testsInverseDSSE,
          NEW(UnitTestSignal.T, test := CheckDeriveWSSE).init(
            "exact gradient of the Weighted Square Smoothness Estimate"),
          NEW(UnitTestSignal.T, test := CheckSmoothness).init(
            "optimizing for smoothness must lead to higher order B-Spline"),
          NEW(UnitTestSignal.T, test := CheckWaveletMatch).init(
            "match of B-Spline wavelet to the B-Spline wavelet family")});

  END Test;

BEGIN
END TestWaveletMatch.
