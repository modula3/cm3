MODULE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;
IMPORT LongRealIntegerPower AS RIntPow;

IMPORT LongRealVector AS V;
IMPORT LongRealVectorRep AS VR;
IMPORT LongRealVectorFast AS VFs;
IMPORT LongRealVectorTrans AS VT;

IMPORT LongRealMatrix AS M;
IMPORT LongRealMatrixTrans AS MT;
IMPORT LongRealMatrixLapack AS LA;
IMPORT LongRealMatrixIntegerPower AS MIntPow;

IMPORT LongRealFunctional AS Fn;
IMPORT LongRealFunctionalDeriv2 AS FnD;
IMPORT LongRealMatchWavelet AS WM;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealRefinableSmooth AS RefnSm;
IMPORT LongRealBSplineWavelet AS BSpl;
IMPORT LongRealDyadicFilterBank AS FB;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
(*IMPORT LongRealComplexVectorFmtLex AS CVF;*)
IMPORT LongRealMatrixFmtLex AS MF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, FileRd, Wr, Thread;

IMPORT OSError, FloatMode, Lex, Rd;

IMPORT NADefinitions AS NA;

PROCEDURE PlotFrame (READONLY abscissa       : V.TBody;
                     READONLY frame          : M.TBody;
                     READONLY wavelet, target: V.TBody; ) =

  CONST normalizeHeight = TRUE;

  PROCEDURE PlotLines (READONLY x, y: V.TBody; ) =
    BEGIN
      IF normalizeHeight THEN
        PL.PlotLines(x, VR.Scale(y, R.One / MAX(VFs.Max(y), -VFs.Min(y)))^);
      ELSE
        PL.PlotLines(x, y);
      END;
    END PlotLines;

  VAR ymin, ymax: R.T;

  <*FATAL PL.SizeMismatch*>(*vector size mismatch can't occur*)
  BEGIN
    IF normalizeHeight THEN
      ymin := -1.1D0;
      ymax := 1.1D0;
    ELSE
      VAR
        waveymin := MIN(VFs.Min(frame[0]), VFs.Min(wavelet));
        waveymax := MAX(VFs.Max(frame[0]), VFs.Max(wavelet));
      BEGIN
        ymin := 1.1D0 * MIN(VFs.Min(target), waveymin);
        ymax := 1.1D0 * MAX(VFs.Max(target), waveymax);
      END;
    END;

    PL.SetFGColorDiscr(7);
    PL.SetEnvironment(
      abscissa[FIRST(abscissa)], abscissa[LAST(abscissa)], ymin, ymax,
      axis := PL.TileSet{PL.Tile.box, PL.Tile.ticks, PL.Tile.axes,
                         PL.Tile.gridMajor, PL.Tile.gridMinor});

    PL.SetFGColorDiscr(1);
    PlotLines(abscissa, wavelet);
    PL.SetFGColorDiscr(2);
    FOR j := FIRST(frame) TO LAST(frame) DO
      PlotLines(abscissa, frame[j]);
    END;

    PL.SetFGColorDiscr(3);
    PlotLines(abscissa, target);
  END PlotFrame;

(*{RT.Half, R.Zero, -RT.Half} instead of {R.One, R.Zero, -R.One} has the
   advantage that the sum of the coefficients of the primal filter doesn't
   change.*)

PROCEDURE TestMatchPattern (target: S.T;
                            numLevels, smooth, vanishing, numTranslates: CARDINAL; )
  RAISES {BSpl.DifferentParity} =
  (*The degree of freedom, i.e.  the number of parameters to minimize for,
     is 2*numTranslates*)
  <*FATAL PL.SizeMismatch*>
  VAR
    hdual := BSpl.GeneratorMask(smooth).scale(R.Two).translate(2);
    gdual := BSpl.WaveletMask(smooth, vanishing).scale(R.Two);
    hdualvan := SIntPow.MulPower(
                  hdual, NEW(S.T).fromArray(
                           ARRAY OF R.T{RT.Half, R.Zero, -RT.Half}),
                  vanishing).translate(-smooth - vanishing);
    approx := WM.MatchPattern(target, hdual, hdualvan, gdual, numLevels,
                              -numTranslates, 2 * numTranslates);

    altWavelet0Amp := WM.MatchPatternWav(target, hdual, gdual, numLevels);
    targetSubWavelet0 := target.superpose(
                           Refn.Refine(gdual.scale(-altWavelet0Amp), hdual,
                                       numLevels));
    altApprox := WM.MatchPatternGen(
                   targetSubWavelet0, hdual, hdualvan, numLevels,
                   -numTranslates, 2 * numTranslates);
    altWaveletMask := gdual.scale(altWavelet0Amp).superpose(
                        hdualvan.upConvolve(altApprox.lift, 2));

    unit   := IIntPow.MulPower(1, 2, numLevels);
    twopow := FLOAT(unit, R.T);
    grid   := R.One / twopow;

    first    := approx.approx.getFirst();
    size     := approx.approx.getNumber();
    abscissa := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

  BEGIN
    IO.Put(Fmt.FN("%s - %s\n", ARRAY OF
                                 TEXT{RF.Fmt(approx.wavelet0Amp),
                                      RF.Fmt(altWavelet0Amp)}));
    IO.Put(
      Fmt.FN("lift:\n%s\n%s\n",
             ARRAY OF TEXT{SF.Fmt(approx.lift), SF.Fmt(altApprox.lift)}));
    PL.Init();
    PlotFrame(abscissa^, SUBARRAY(approx.basis^, 0, 2 * numTranslates),
              approx.basis[LAST(approx.basis^)], approx.targetPad^);
    PL.SetFGColorDiscr(4);
    PL.PlotLines(abscissa^, approx.approx.getData()^);
    PL.SetFGColorDiscr(5);
    PL.PlotLines(
      abscissa^,
      Refn.Refine(altWaveletMask, hdual, numLevels).clipToVector(
        first, size)^);
    IF FALSE THEN
      PL.SetFGColorDiscr(6);
      PL.PlotLines(abscissa^, targetSubWavelet0.clipToVector(first, size)^);
    END;
    PL.Exit();
  END TestMatchPattern;


TYPE
  NormEqu = RECORD
              mat: M.T;
              vec: V.T;
            END;

PROCEDURE ComputeNormalEqu (target                                : S.T;
                            refineMask, generatorMask, waveletMask: S.T;
                            numLevels     : CARDINAL;
                            firstTranslate: INTEGER;
                            numTranslates : CARDINAL; ): NormEqu
  RAISES {NA.Error} =
  VAR
    refineSize := refineMask.getNumber() - 1;

    refineTrans := Refn.TransitionMatrix(refineMask);

    generatorMaskAutoCor := generatorMask.autocorrelate();
    waveletMaskAutoCor   := waveletMask.autocorrelate();

    refinePower := MIntPow.Power(refineTrans, numLevels);
    (*extract the center column of the refinement matrix power*)
    refineAutoCor := NEW(S.T).fromVector(
                       M.GetColumn(refinePower, refineSize), -refineSize);
    generatorAutoCor := refineAutoCor.convolveDown(generatorMaskAutoCor, 2);
    generatorAutoCorMat := NEW(M.T, numTranslates, numTranslates);
    generatorWaveletCor := refineAutoCor.convolveDown(
                             generatorMask.adjoint().convolve(waveletMask),
                             2).clipToVector(firstTranslate, numTranslates);
    waveletAutoCor := refineAutoCor.inner(waveletMaskAutoCor);

    targetCor := NEW(V.T, numTranslates + 1);

  BEGIN
    FOR i := 0 TO numTranslates - 1 DO
      generatorAutoCor.clipToArray(-i, generatorAutoCorMat[i]);
    END;
    (*DWT routine is only of little help here*)
    VAR
      x := target;
      y := refineMask.adjoint();
    BEGIN
      FOR i := 0 TO numLevels - 1 DO x := x.convolveDown(y, 2); END;
      x.convolveDown(generatorMask.adjoint(), 2).clipToArray(
        firstTranslate, SUBARRAY(targetCor^, 0, numTranslates));
      targetCor[numTranslates] :=
        x.convolveDown(waveletMask.adjoint(), 2).getValue(0);
    END;

    RETURN
      NormEqu{M.FromMatrixArray(
                M.TMBody{ARRAY [0 .. 1] OF
                           M.T{generatorAutoCorMat,
                               M.ColumnFromVector(generatorWaveletCor)},
                         ARRAY [0 .. 1] OF
                           M.T{M.RowFromVector(generatorWaveletCor),
                               M.FromScalar(waveletAutoCor)}}), targetCor};
  END ComputeNormalEqu;

PROCEDURE TestNormalEqu () =
  CONST
    numLevels      = 7;
    firstTranslate = -6;
    numTranslate   = 9;
    realFmtStyle   = RF.FmtStyle{prec := 5};

  VAR
    target := Refn.Refine(
                S.One, BSpl.GeneratorMask(4).scale(2.0D0), 7).translate(30);

    refineMask    := NEW(S.T).fromArray(ARRAY OF R.T{0.9D0, 1.1D0, 0.7D0});
    generatorMask := NEW(S.T).fromArray(ARRAY OF R.T{0.7D0, 0.0D0, 0.8D0});
    waveletMask := NEW(S.T).fromArray(
                     ARRAY OF R.T{-0.5D0, 0.2D0, 0.9D0}, -1);

    approx := WM.MatchPattern(
                target, refineMask, generatorMask, waveletMask, numLevels,
                firstTranslate, numTranslate);
    covar     := M.MulMMA(approx.basis);
    targetCor := M.MulV(approx.basis, approx.targetPad);

    normEqu := ComputeNormalEqu(
                 target, refineMask, generatorMask, waveletMask, numLevels,
                 firstTranslate, numTranslate);

    error := MT.Norm1(M.Sub(covar, normEqu.mat)) / MT.Norm1(covar);

  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  BEGIN
    IO.Put(
      Fmt.FN(
        "normal matrix:\n%s\ncorrelation with target:\n%s\n",
        ARRAY OF
          TEXT{MF.Fmt(covar, style := MF.FmtStyle{width := 10, elemStyle :=
                                                  realFmtStyle}),
               VF.Fmt(targetCor,
                      style := VF.FmtStyle{elemStyle := realFmtStyle})}));
    IO.Put(Fmt.FN(
             "normal matrix:\n%s\ncorrelation with target:\n%s\n",
             ARRAY OF
               TEXT{
               MF.Fmt(
                 normEqu.mat,
                 style :=
                   MF.FmtStyle{width := 10, elemStyle := realFmtStyle}),
               VF.Fmt(normEqu.vec,
                      style := VF.FmtStyle{elemStyle := realFmtStyle})}));
    IO.Put(Fmt.FN("error: %s\n", ARRAY OF TEXT{RF.Fmt(error)}));
    <*ASSERT error<1.0D-15*>
  END TestNormalEqu;


(** SSE  - Square Smoothness Estimate,
           that is the estimate that depends on the sum of the squares of the eigenvalues
    WSSE - Weighted Square Smoothness Estimate,
           workaround that takes into account that the gdual0 filter (alias psi0)
           was amplified within the linear least squares term
*)

PROCEDURE TestSSE (x: V.T) =
  VAR
    dx0 := V.FromArray(ARRAY OF R.T{1.0D-8, 0.0D0, 0.0D0});
    dx1 := V.FromArray(ARRAY OF R.T{0.0D0, 1.0D-8, 0.0D0});
    dx2 := V.FromArray(ARRAY OF R.T{0.0D0, 0.0D0, 1.0D-8});
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  BEGIN
    VAR
      rho     := RefnSm.ComputeSSE(x^);
      rho0    := RefnSm.ComputeSSE(V.Add(x, dx0)^);
      rho1    := RefnSm.ComputeSSE(V.Add(x, dx1)^);
      rho2    := RefnSm.ComputeSSE(V.Add(x, dx2)^);
      gradrho := V.Scale(RefnSm.ComputeDSSE(x^), 1.0D-8);
    BEGIN
      IO.Put(
        Fmt.FN("rho %s, difrho={%s,%s,%s}, approxdiff=%s\n",
               ARRAY OF
                 TEXT{RF.Fmt(rho), RF.Fmt(rho0 - rho), RF.Fmt(rho1 - rho),
                      RF.Fmt(rho2 - rho), VF.Fmt(gradrho)}));
    END;
    VAR
      gradrho   := RefnSm.ComputeDSSE(x^);
      gradrho0  := RefnSm.ComputeDSSE(V.Add(x, dx0)^);
      gradrho1  := RefnSm.ComputeDSSE(V.Add(x, dx1)^);
      gradrho2  := RefnSm.ComputeDSSE(V.Add(x, dx2)^);
      jacobirho := M.Scale(RefnSm.ComputeDDSSE(x^), 1.0D-8);
    BEGIN
      IO.Put(
        Fmt.FN("gradrho %s, difgradrho={%s,%s,%s}, approxdiff=%s\n",
               ARRAY OF
                 TEXT{VF.Fmt(gradrho), VF.Fmt(V.Sub(gradrho0, gradrho)),
                      VF.Fmt(V.Sub(gradrho1, gradrho)),
                      VF.Fmt(V.Sub(gradrho2, gradrho)), MF.Fmt(jacobirho)}));
    END;
  END TestSSE;

PROCEDURE InverseDSSE (x: V.T): V.T RAISES {NA.Error} =
  (*Find the parameter vector y for which DSSE(y)=x*)
  CONST tol = 1.0D-14;
  (*VAR y := V.New(3);*)
  VAR y := x;                    (*zero is a bad initial value*)
  BEGIN
    FOR j := 0 TO 100 DO
      VAR ax := RefnSm.ComputeDSSE(y^);
      BEGIN
        IF VT.Norm1(V.Sub(ax, x)) <= tol * VT.Norm1(x) THEN RETURN y; END;
        y := V.Add(y, LA.LeastSquares(RefnSm.ComputeDDSSE(y^),
                                      ARRAY OF V.T{V.Sub(x, ax)})[0].x);
        (*
          IO.Put(Fmt.FN("y %s, DSSE(y) %s\n",
                       ARRAY OF TEXT{VF.Fmt(y), VF.Fmt(RefnSm.ComputeDSSE(y^))}));
        *)
      END;
    END;
    RAISE NA.Error(NA.Err.not_converging);
  END InverseDSSE;

PROCEDURE TestInverseDSSE (READONLY x0: ARRAY [0 .. 2] OF R.T) =
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  VAR
    x := V.FromArray(x0);
    y := InverseDSSE(x);
  BEGIN
    IO.Put(
      Fmt.FN("x %s, RefnSm.ComputeDSSE(y) %s, y %s\n",
             ARRAY OF
               TEXT{VF.Fmt(x), VF.Fmt(RefnSm.ComputeDSSE(y^)), VF.Fmt(y)}));
  END TestInverseDSSE;

PROCEDURE DeriveDist (normalMat    : M.T;
                      targetCor    : V.T;
                      targetNormSqr: R.T;
                      s            : S.T; ): FnD.T RAISES {NA.Error} =
  VAR
    normals := M.MulV(normalMat, s.getData());
    dist := V.Inner(s.getData(), V.Sub(normals, V.Scale(targetCor, R.Two)))
              + targetNormSqr;

  BEGIN
    RETURN FnD.T{zeroth := dist, first :=
                 V.Scale(V.Sub(normals, targetCor), R.Two), second :=
                 M.Scale(normalMat, R.Two)};
  END DeriveDist;

PROCEDURE DeriveSSE (hdual, gdual0, s: S.T; ): FnD.T RAISES {NA.Error} =
  VAR
    gdual   := gdual0.superpose(hdual.upConvolve(s, 2));
    hprimal := gdual.alternate();
    gprimal := hdual.alternate();

    hsums := hprimal.wrapCyclic(3).getData();
    dsums := M.Cyclic(gprimal.translate(2 * s.getFirst()).wrapCyclic(
                        3).getData(), s.getNumber(), -1);
  BEGIN
    (*
    IO.Put(MF.Fmt(dsums) & "\n");
    RETURN polypart;
    *)
    RETURN FnD.T{zeroth := RefnSm.ComputeSSE(hsums^), first :=
                 M.MulV(dsums, RefnSm.ComputeDSSE(hsums^)), second :=
                 M.Mul(M.Mul(dsums, RefnSm.ComputeDDSSE(hsums^)),
                       M.Transpose(dsums))};
  END DeriveSSE;

PROCEDURE DeriveWSSE (hdual, gdual0, s: S.T; c: R.T): FnD.T
  RAISES {NA.Error} =
  (* In the fitting routine the expression hdual*s+gdual0*c is fitted to
     the target.  But the wavelet won't be smoother if s and c becomes
     smaller.  Instead hdual/c*s+gdual0, or more precisely
     gprimal/c*s+hprimal0, is the quantity we have to apply our estimate
     to.

     The whole computation is horribly inefficient but this is for research
     only! *)
  VAR
    hsdual   := hdual.upConvolve(s, 2);
    gdual    := gdual0.superpose(hsdual.scale(1.0D0 / c));
    hprimal  := gdual.alternate();
    gprimal  := hdual.alternate();
    gsprimal := hsdual.alternate();

    hsums := hprimal.wrapCyclic(3).getData();
    dsums := M.Cyclic(gprimal.scale(1.0D0 / c).translate(
                        2 * s.getFirst()).wrapCyclic(3).getData(),
                      s.getNumber(), -1);
    csums := gsprimal.wrapCyclic(3).scale(-1.0D0 / (c * c)).getData();
    dcsums := M.FromMatrixArray(
                ARRAY OF
                  ARRAY OF M.T{
                  ARRAY [0 .. 0] OF M.T{dsums},
                  ARRAY [0 .. 0] OF M.T{M.RowFromVector(csums)}});

    sse   := RefnSm.ComputeSSE(hsums^);
    dsse  := RefnSm.ComputeDSSE(hsums^);
    ddsse := RefnSm.ComputeDDSSE(hsums^);

    dgsums := V.New(s.getNumber());

  BEGIN
    (*
    IO.Put(MF.Fmt(dsums) & "\n");
    RETURN polypart;
    *)
    FOR i := 0 TO s.getNumber() - 1 DO
      dgsums[i] :=
        V.Inner(dsse, gprimal.translate(2 * (s.getFirst() + i)).wrapCyclic(
                        3).getData()) * (R.MinusOne / (c * c));
    END;

    RETURN FnD.T{zeroth := sse, first := M.MulV(dcsums, dsse), second :=
                 M.Add(M.Mul(M.Mul(dcsums, ddsse), M.Transpose(dcsums)),
                       M.FromMatrixArray(
                         ARRAY OF
                           ARRAY OF M.T{
                           ARRAY [0 .. 1] OF
                             M.T{M.NewZero(s.getNumber(), s.getNumber()),
                                 M.ColumnFromVector(dgsums)},
                           ARRAY [0 .. 1] OF
                             M.T{M.RowFromVector(dgsums),
                                 M.FromScalar(
                                   V.Inner(dsse, csums) * -2.0D0 / c)}}))};
  END DeriveWSSE;

PROCEDURE TestDeriveWSSE () =
  <*FATAL NA.Error*>
  CONST delta = 1.0D-8;
  VAR
    hdual  := NEW(S.T).fromArray(ARRAY OF R.T{0.23D0, 1.678D0, -0.85D0});
    gdual0 := NEW(S.T).fromArray(ARRAY OF R.T{0.723D0, -1.078D0, 0.585D0});
    s      := NEW(S.T).fromArray(ARRAY OF R.T{0.2D0, -0.3D0, 0.1D0});
    sdelta := NEW(S.T).fromArray(ARRAY OF R.T{delta}, s.getFirst());
    c      := 0.73D0;
    der    := DeriveWSSE(hdual, gdual0, s, c);
    derArr := NEW(REF ARRAY OF FnD.T, s.getNumber() + 1);
  BEGIN
    FOR i := 0 TO s.getNumber() - 1 DO
      derArr[i] :=
        DeriveWSSE(hdual, gdual0, s.superpose(sdelta.translate(i)), c);
    END;
    derArr[LAST(derArr^)] := DeriveWSSE(hdual, gdual0, s, c + delta);
    PutDervDif(der, derArr^, delta);
  END TestDeriveWSSE;


PROCEDURE PutDervDif (READONLY der   : FnD.T;
                      READONLY derArr: ARRAY OF FnD.T;
                               delta : R.T             ) =
  <*FATAL Thread.Alerted, Wr.Failure, NA.Error *>
  BEGIN
    (*compare first derivative (gradient) with the first difference*)
    IO.Put(VF.Fmt(V.Scale(der.first, delta)) & "\n");
    FOR j := 0 TO LAST(derArr) DO
      IO.Put(Fmt.FN("der[%s] %s, %s\n",
                    ARRAY OF
                      TEXT{Fmt.Int(j (* + y.getFirst()*)),
                           RF.Fmt(derArr[j].zeroth - der.zeroth),
                           RF.Fmt(der.first[j] * delta)}));
    END;

    (*compare second derivative with the first difference of the first
       derivative*)
    FOR j := 0 TO LAST(derArr) DO
      IO.Put(
        Fmt.FN("der[%s]\n %s %s\n",
               ARRAY OF
                 TEXT{Fmt.Int(j (*+ y.getFirst()*)),
                      VF.Fmt(V.Sub(derArr[j].first, der.first)),
                      VF.Fmt(V.Scale(M.GetRow(der.second, j), delta))}));
    END;
  END PutDervDif;


(*extend derivatives by information that are contributed by optimizing the
   amplitude of the target*)
PROCEDURE ExtendDervTarget (READONLY x        : FnD.T;
                                     lift     : V.T;
                                     targetAmp: R.T;
                                     target   : V.T;
                                     targetCor: V.T;
                                     wavelet0 : V.T    ): FnD.T
  RAISES {NA.Error} =
  BEGIN
    RETURN
      FnD.T{zeroth :=
            x.zeroth + R.Two * targetAmp * V.Inner(targetCor, lift)
              + targetAmp * targetAmp * V.Inner(target, target)
              - R.Two * targetAmp * V.Inner(wavelet0, target), first :=
            V.FromVectorArray(
              ARRAY OF
                V.T{V.Add(x.first, V.Scale(targetCor, targetAmp * R.Two)),
                    V.FromScalar(
                      R.Two * (V.Inner(lift, targetCor)
                                 + targetAmp * V.Inner(target, target)
                                 - V.Inner(wavelet0, target)))}), second :=
            M.FromMatrixArray(
              ARRAY [0 .. 1], [0 .. 1] OF
                M.T{ARRAY OF
                      M.T{x.second,
                          M.ColumnFromArray(V.Scale(targetCor, R.Two)^)},
                    ARRAY OF
                      M.T{M.RowFromArray(V.Scale(targetCor, R.Two)^),
                          M.FromScalar(V.Inner(target, target) * R.Two)}})};
  END ExtendDervTarget;

PROCEDURE TranslatesBasis (generatorvan : S.T;
                           first        : INTEGER;
                           twonit, size : CARDINAL;
                           numTranslates: CARDINAL  ): M.T =
  VAR basis := M.New(2 * numTranslates, size);
  BEGIN
    FOR j := -numTranslates TO numTranslates - 1 DO
      generatorvan.clipToArray(
        first - twonit * j, basis[j + numTranslates]);
      (*
            targetCor[j + numTranslates] :=
              VS.Inner(basis[j + numTranslates], targetVec^);
      *)
    END;
    RETURN basis;
  END TranslatesBasis;

<*OBSOLETE*>
PROCEDURE GetLiftedPrimalGeneratorMask (         hdual, gdual0: S.T;
                                        READONLY mc           : MatchCoef):
  S.T =
  VAR
    hsdual := hdual.upConvolve(mc.lift, 2);
    gdual  := gdual0.superpose(hsdual.scale(R.One / mc.wavelet0Amp));
  BEGIN
    RETURN gdual.alternate();
  END GetLiftedPrimalGeneratorMask;


TYPE
  Matching =
    OBJECT
      yfirst: INTEGER;
    METHODS
      splitParamVec (x: V.T): MatchCoef;
      deriveRegularized (         derdist: FnD.T;
                         READONLY mc     : MatchCoef;
                         waveletVec, waveletCor, targetVec: V.T; ): FnD.T;
    END;

  VarWavAmpMatching = Matching OBJECT
                      OVERRIDES
                        splitParamVec     := VarSplitParamVec;
                        deriveRegularized := VarDeriveRegularized;
                      END;

  FixedWavAmpMatching = Matching OBJECT
                          wavAmp: R.T
                        OVERRIDES
                          splitParamVec     := FixSplitParamVec;
                          deriveRegularized := FixDeriveRegularized;
                        END;

PROCEDURE VarSplitParamVec (SELF: VarWavAmpMatching; x: V.T; ): MatchCoef =
  BEGIN
    RETURN MatchCoef{NEW(S.T).fromArray(SUBARRAY(
                                          x^, FIRST(x^), NUMBER(x^) - 1),
                                        SELF.yfirst), x[LAST(x^)], R.One};
  END VarSplitParamVec;


PROCEDURE VarDeriveRegularized ( <*UNUSED*>SELF   : VarWavAmpMatching;
                                           derdist: FnD.T;
                                           READONLY mc: MatchCoef;
                                           waveletVec, waveletCor,
                                             targetVec: V.T; ): FnD.T =
  <*FATAL NA.Error*>(*size mismatches can't occur*)
  BEGIN
    RETURN ExtendDervTarget(derdist, mc.lift.getData(), mc.wavelet0Amp,
                            waveletVec, waveletCor, targetVec);
  END VarDeriveRegularized;

PROCEDURE FixSplitParamVec (SELF: FixedWavAmpMatching; x: V.T; ):
  MatchCoef =
  BEGIN
    RETURN MatchCoef{NEW(S.T).fromArray(SUBARRAY(
                                          x^, FIRST(x^), NUMBER(x^) - 1),
                                        SELF.yfirst), SELF.wavAmp, R.One};
  END FixSplitParamVec;

PROCEDURE FixDeriveRegularized ( <*UNUSED*>SELF   : FixedWavAmpMatching;
                                           derdist: FnD.T;
                                           READONLY mc: MatchCoef;
                                           <*UNUSED*>
                                           waveletVec, waveletCor,
                                             targetVec: V.T; ): FnD.T =
  <*FATAL NA.Error*>(*size mismatches can't occur*)
  VAR zerovec := V.NewZero(mc.lift.getNumber());
  BEGIN
    RETURN
      FnD.T{
        zeroth := derdist.zeroth, first :=
        V.FromVectorArray(
          ARRAY OF V.T{derdist.first, V.FromScalar(R.Zero)}), second :=
        M.FromMatrixArray(
          ARRAY [0 .. 1], [0 .. 1] OF
            M.T{
            ARRAY OF M.T{derdist.second, M.ColumnFromArray(zerovec^)},
            ARRAY OF M.T{M.RowFromArray(zerovec^), M.FromScalar(R.One)}})};
  END FixDeriveRegularized;


TYPE
  FilterBasis =
    OBJECT
      lp,                        (*--h--*)
        lpVan,                   (*lp*(1,0,-1)^m*)
        lpSmallVan,              (*lp*(1,0,-1)^m0*(1,1)^m1 with m=m0+m1*)
        lpNoVan,                 (*lp*(1,1)^m, lpVan with (1,-1)^m
                                    removed*)
        hp,                      (*--g0-- lp and hp are complementary*)
        hpSmallVan,              (*hp*(1,-1)^m0*)
        hpNoVan                  (*lpNoVan and hpNoVan are complementary*)
                                                                          : S.T;
      shiftSmallVan: INTEGER;
      negateWavelet: BOOLEAN;
    METHODS
      getRefineMask      (): S.T := GetRefineMask;
      getLSGeneratorMask (): S.T := GetLSGeneratorMask;
      getLSWavelet0Mask  (): S.T := GetLSWavelet0Mask;
      getLiftedWaveletMaskNoVan (READONLY mc: MatchCoef; ): S.T := GetLiftedWaveletMaskNoVan;
      getShapeWaveletMask (READONLY mc: MatchCoef; ): S.T := GetShapeWaveletMask;
      getFilterBank (READONLY mc: MatchCoef; ): ARRAY [0 .. 1] OF FB.T;
      plotBase      (READONLY mc: MatchCoef; numLevels: CARDINAL; );
    END;

  StdFilterBasis = FilterBasis OBJECT
                   OVERRIDES
                     plotBase      := StdPlotBase;
                     getFilterBank := StdGetFilterBank;
                   END;

  NoVanFilterBasis = FilterBasis OBJECT
                     OVERRIDES
                       plotBase      := NoVanPlotBase;
                       getFilterBank := NoVanGetFilterBank;
                     END;

PROCEDURE GetLiftedWaveletMaskNoVan (         SELF: FilterBasis;
                                     READONLY mc  : MatchCoef;   ): S.T =
  VAR
    hpdif := SELF.lpNoVan.upConvolve(
               mc.lift.scale(R.One / mc.wavelet0Amp), 2);
  BEGIN
    RETURN SELF.hpNoVan.superpose(hpdif);
  END GetLiftedWaveletMaskNoVan;


PROCEDURE GetRefineMask (SELF: FilterBasis; ): S.T =
  BEGIN
    RETURN SELF.lp;
  END GetRefineMask;

PROCEDURE GetLSGeneratorMask (SELF: FilterBasis; ): S.T =
  BEGIN
    RETURN SELF.lpSmallVan;
  END GetLSGeneratorMask;

PROCEDURE GetLSWavelet0Mask (SELF: FilterBasis; ): S.T =
  BEGIN
    RETURN SELF.hpSmallVan;
  END GetLSWavelet0Mask;

PROCEDURE GetShapeWaveletMask (SELF: FilterBasis; READONLY mc: MatchCoef; ):
  S.T =
  (*Reverse translation that was made to center the masks.*)
  BEGIN
    RETURN SELF.hpSmallVan.superpose(
             SELF.lpSmallVan.upConvolve(
               mc.lift.scale(R.One / mc.wavelet0Amp), 2)).translate(
             -SELF.shiftSmallVan);
  END GetShapeWaveletMask;

PROCEDURE StdGetFilterBank (SELF: StdFilterBasis; READONLY mc: MatchCoef; ):
  ARRAY [0 .. 1] OF FB.T =
  VAR dual := FB.T{SELF.getRefineMask(), SELF.getShapeWaveletMask(mc)};
  BEGIN
    RETURN ARRAY OF FB.T{FB.GetComplement(dual), dual};
  END StdGetFilterBank;

PROCEDURE StdPlotBase (         SELF     : StdFilterBasis;
                       READONLY mc       : MatchCoef;
                                numLevels: CARDINAL;       ) =
  CONST
    ymin = -3.5D0;
    ymax = 3.5D0;
  BEGIN
    WP.PlotBiorthogonalYLim(
      SELF.getRefineMask(), SELF.getShapeWaveletMask(mc), numLevels, ymin,
      ymax);
  END StdPlotBase;

PROCEDURE NoVanGetFilterBank (         SELF: NoVanFilterBasis;
                              READONLY mc  : MatchCoef;        ):
  ARRAY [0 .. 1] OF FB.T =
  VAR
    bank := ARRAY [0 .. 1] OF
              FB.T{FB.GetComplement(
                     FB.T{SELF.lp,
                          SELF.hp.superpose(
                            SELF.lpVan.upConvolve(
                              mc.lift.scale(R.One / mc.wavelet0Amp), 2))}),
                   FB.T{SELF.lp, SELF.getShapeWaveletMask(mc)}};
  BEGIN
    (*I have still not understand why this negation is necessary, but it
       doesn't work without it.*)
    IF SELF.negateWavelet THEN
      RETURN ARRAY OF
               FB.T{FB.T{bank[0, 0].negate(), bank[0, 1].negate()},
                    FB.T{bank[1, 0], bank[1, 1]}};
    ELSE
      RETURN bank;
    END;
    (*
        RETURN ARRAY OF
                 FB.T{FB.GetComplement(
                        FB.T{SELF.lp,
                             SELF.hp.superpose(
                               SELF.lpVan.upConvolve(
                                 mc.lift.scale(R.One / mc.wavelet0Amp), 2))}),
                      FB.T{SELF.lp, SELF.getShapeWaveletMask(mc)}};
    *)
    (*
      Check if vanishing moments are added correctly.
      Note that the Dual filters are used for Analysis!

          vanBank := ARRAY [0 .. 1] OF
                       FB.T{FB.T{bank[0, 0], bank[0, 1]},
                            FB.T{bank[1, 0], SIntPow.MulPower(
                                               bank[1, 1], vanAtom, van1)}};
          VAR primal := FB.GetComplement(vanBank[1]);
          BEGIN
            IO.Put(
              Fmt.FN("Filter bank should be:\n%s\n%s\n%s\n%s\n",
                     ARRAY OF
                       TEXT{SF.Fmt(vanBank[0, 0]), SF.Fmt(primal[0]),
                            SF.Fmt(vanBank[0, 1]), SF.Fmt(primal[1])}));
          END;
    *)
  END NoVanGetFilterBank;

PROCEDURE NoVanPlotBase (         SELF     : NoVanFilterBasis;
                         READONLY mc       : MatchCoef;
                                  numLevels: CARDINAL;         ) =
  CONST
    ymin = -8.0D0;
    ymax = 8.0D0;
  VAR
    bank       := SELF.getFilterBank(mc);
    refnPrimal := bank[0, 0];
    refnDual   := bank[1, 0];
  BEGIN
    WP.PlotAnyYLim(
      refnPrimal.scale(R.Two), refnDual.scale(R.Two), bank[0, 0],
      bank[0, 1], bank[1, 0], bank[1, 1], numLevels, ymin, ymax);
  END NoVanPlotBase;


TYPE
  MatchCoef =
    RECORD
      lift                  : S.T;
      wavelet0Amp, targetAmp: R.T;  (*coefficient of the target function*)
    END;

PROCEDURE MatchPatternSmooth (target                  : S.T;
                              dualBasis               : FilterBasis;
                              numLevels, numTranslates: CARDINAL;
                              smoothWeight            : R.T;         ):
  MatchCoef RAISES {NA.Error} =

  <*UNUSED*>
  PROCEDURE CheckDerivatives () RAISES {NA.Error} =
    CONST
      delta = 1.0D-8;
      cf    = 0.7D0;
    VAR
      (*this array length can only be used if a total of 6 translates is
         considered*)
      y := NEW(S.T).fromArray(
             ARRAY OF R.T{0.2D0, -0.3D0, 0.0D0, -0.1D0, 0.0D0, 0.4D0}, -3);
      der := FnD.Add(DeriveDist(normalMat, waveletCor, waveletNormSqr, y),
                     FnD.Scale(DeriveSSE(dualBasis.lpVan, dualBasis.hp, y),
                               smoothWeight));
      derArr := NEW(REF ARRAY OF FnD.T, NUMBER(der.first^));
      extder := ExtendDervTarget(
                  der, y.getData(), cf, targetVec, targetCor, waveletVec);
      extderArr := NEW(REF ARRAY OF FnD.T, NUMBER(extder.first^));

    BEGIN
      FOR j := 0 TO LAST(derArr^) DO
        VAR
          yp := y.superpose(NEW(S.T).fromArray(
                              ARRAY OF R.T{delta}, j + y.getFirst()));
        BEGIN
          derArr[j] :=
            FnD.Add(DeriveDist(normalMat, waveletCor, waveletNormSqr, yp),
                    FnD.Scale(DeriveSSE(dualBasis.lpVan, dualBasis.hp, yp),
                              smoothWeight));
          extderArr[j] :=
            ExtendDervTarget(derArr[j], yp.getData(), cf, targetVec,
                             targetCor, waveletVec);
        END;
      END;
      extderArr[LAST(extderArr^)] :=
        ExtendDervTarget(
          der, y.getData(), cf + delta, targetVec, targetCor, waveletVec);

      PutDervDif(der, derArr^, delta);
      PutDervDif(extder, extderArr^, delta);
    END CheckDerivatives;

  VAR
    generatorvan := Refn.Refine(dualBasis.getLSGeneratorMask(),
                                dualBasis.getRefineMask(), numLevels);
    wavelet := Refn.Refine(dualBasis.getLSWavelet0Mask(),
                           dualBasis.getRefineMask(), numLevels);

    unit   := IIntPow.Power(2, numLevels);
    twonit := 2 * unit;
    first := MIN(wavelet.getFirst(),
                 generatorvan.getFirst() - twonit * numTranslates);
    last := MAX(wavelet.getLast(),
                generatorvan.getLast() + twonit * (numTranslates - 1));
    size      := last - first + 1;
    wavescale := FLOAT(unit, R.T);
    grid      := R.One / FLOAT(unit, R.T);
    abscissa  := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

    waveletVec     := wavelet.clipToVector(first, size);
    waveletNormSqr := V.Inner(waveletVec, waveletVec);
    targetVec      := target.clipToVector(first, size);
    targetNormSqr  := V.Inner(targetVec, targetVec);
    (* the target vector might have been clipped, thus
       V.Inner(target.getData(),target.getData()) may be different *)

    basis := TranslatesBasis(
               generatorvan, first, twonit, size, numTranslates);
    normalMat  := M.MulMMA(basis);
    targetCor  := M.MulV(basis, targetVec);
    waveletCor := M.MulV(basis, waveletVec);

  BEGIN
    PL.Init();
    PlotFrame(abscissa^, M.Scale(basis, wavescale)^,
              V.Scale(waveletVec, wavescale)^, targetVec^);
    PL.Exit();
    (*
    CheckDerivatives();

    IO.Put(Fmt.FN("normal matrix %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(normalMat)}));
    *)

    VAR
      yfirst             := -numTranslates;
      matching: Matching;

    <*UNUSED*>
    PROCEDURE ComputeOptCritDeriv (x: V.T): FnD.T RAISES {NA.Error} =
      VAR
        (*SplitParamVec may return initWavelet0Amp as waveletAmp and this
           won't work if we compute the real derivative instead of a finite
           difference.*)
        mc := matching.splitParamVec(x);
        derdist := DeriveDist(normalMat, targetCor, targetNormSqr, mc.lift);
        derwavdist := ExtendDervTarget(
                        derdist, mc.lift.getData(), mc.wavelet0Amp,
                        waveletVec, waveletCor, targetVec);
      BEGIN
        (*
        IO.Put(
          Fmt.FN("y %s, cf %s\n", ARRAY OF TEXT{SF.Fmt(y), RF.Fmt(cf)}));
        *)
        RETURN FnD.Add(derwavdist,
                       FnD.Scale(
                         DeriveWSSE(dualBasis.getLSGeneratorMask(),
                                    dualBasis.getLSWavelet0Mask(), mc.lift,
                                    mc.wavelet0Amp), smoothWeightFade));
      END ComputeOptCritDeriv;

    PROCEDURE ComputeOptCritDiff (x: V.T): FnD.T RAISES {NA.Error} =

      PROCEDURE EstimateSmoothness (x: V.T): R.T =
        VAR
          hprimal := dualBasis.getLiftedWaveletMaskNoVan(
                       matching.splitParamVec(x)).adjoint();
        <*FATAL NA.Error*>
        BEGIN
          CASE 4 OF
          | 0 => RETURN RefnSm.SquareSmoothEstimate(hprimal);
          | 1 => RETURN RefnSm.SpecRad(hprimal);
          | 2 => RETURN RefnSm.BSpline(hprimal);
          | 3 => RETURN RefnSm.Binomial(hprimal);
          | 4 => RETURN RefnSm.Frobenius(hprimal);
          | 5 => RETURN RefnSm.SimpFrobenius(hprimal);
          | 6 => RETURN RefnSm.SumNorm(hprimal);
          ELSE
            <*ASSERT FALSE*>
          END;
        END EstimateSmoothness;

      VAR
        mc := matching.splitParamVec(x);
        derdist := DeriveDist(normalMat, targetCor, targetNormSqr, mc.lift);

        dx  := V.New(NUMBER(x^));
        dxv := VT.Norm1(x) * difdist;
        (*dx := V.Scale(x, 1.0D-2);*)
        derwavdist := matching.deriveRegularized(
                        derdist, mc, waveletVec, waveletCor, targetVec);
        dersmooth: FnD.T;

      <*FATAL Thread.Alerted, Wr.Failure*>
      BEGIN
        IO.Put(
          Fmt.FN("ComputeOptCritDiff for x=%s", ARRAY OF TEXT{VF.Fmt(x)}));
        FOR i := FIRST(dx^) TO LAST(dx^) DO dx[i] := dxv END;
        dersmooth := Fn.EvalCentralDiff2(EstimateSmoothness, x, dx);
        IO.Put(
          Fmt.FN(
            "dist %s, smooth %s, weight %s\n",
            ARRAY OF
              TEXT{RF.Fmt(derwavdist.zeroth), RF.Fmt(dersmooth.zeroth),
                   RF.Fmt(smoothWeightFade)}));
        IO.Put(
          Fmt.FN("dist' %ssmooth' %s\n",
                 ARRAY OF
                   TEXT{VF.Fmt(derwavdist.first), VF.Fmt(dersmooth.first)}));
        RETURN FnD.Add(derwavdist, FnD.Scale(dersmooth, smoothWeightFade));
      END ComputeOptCritDiff;


    CONST
      maxIter           = 10;
      smoothWeightProgr = 100.0D0;
      maxSubIter        = 30;

      tol     = 1.0D-4;
      difdist = 1.0D-5;

    VAR
      (*
      x := V.FromVectorArray(
             ARRAY OF V.T{V.New(2 * numTranslates), V.FromScalar(R.One)});
      *)
      (*
      x := V.FromVectorArray(
             ARRAY OF
               V.T{V.ArithSeq(2 * numTranslates, -0.45D0, 0.1D0),
                   V.FromScalar(R.One)});
      *)
      (* use this initialization if you want to compare the results with
         MatchPattern

         x := V.New(2 * numTranslates + 1); *)
      initlift := NEW(S.T).init(yfirst, 2 * numTranslates);
      initderdist := DeriveDist(
                       normalMat, targetCor, targetNormSqr, initlift);
      initderwavdist := ExtendDervTarget(
                          initderdist, initlift.getData(), R.Zero,
                          waveletVec, waveletCor, targetVec);
      x := V.Neg(LA.LeastSquares(initderwavdist.second,
                                 ARRAY OF V.T{initderwavdist.first})[0].x);

      smoothWeightFade := smoothWeight / RIntPow.Power(
                            smoothWeightProgr, maxIter);

    BEGIN
      CASE 0 OF
      | 0 => matching := NEW(VarWavAmpMatching, yfirst := yfirst);
      | 1 =>
          matching := NEW(FixedWavAmpMatching, yfirst := yfirst,
                          wavAmp := x[LAST(x^)]);
      ELSE
      END;
      PL.Init();
      (*IO.Put(Fmt.FN("targetCor %s", ARRAY OF TEXT{VF.Fmt(targetCor)}));*)
      FOR iter := 0 TO maxIter DO
        VAR
          precOk            := FALSE;
          subIter: CARDINAL := 0;
        BEGIN
          WHILE NOT precOk DO
            IF subIter >= maxSubIter THEN
              RAISE NA.Error(NA.Err.not_converging);
            END;
            INC(subIter);
            VAR der := ComputeOptCritDiff(x);
            BEGIN
              x := V.Sub(x, LA.LeastSquares(
                              der.second, ARRAY OF V.T{der.first})[0].x);
              precOk := VT.Norm1(der.first) <= tol * RT.Abs(der.zeroth);
            END;
          END;
        END;

        (*draw resulting wavelet base*)
        PL.StartPage();
        dualBasis.plotBase(matching.splitParamVec(x), numLevels);
        (*PL.StopPage();*)

        smoothWeightFade := smoothWeightFade * smoothWeightProgr;
      END;
      PL.Exit();
      RETURN matching.splitParamVec(x);
    END;
  END MatchPatternSmooth;

PROCEDURE TestMatchPatternSmooth (target: S.T;
                                  numLevels, smooth, vanishing,
                                    smallVanishing, numTranslates: CARDINAL;
                                  smoothWeight: R.T):
  ARRAY [0 .. 1] OF FB.T RAISES {BSpl.DifferentParity} =
  <*FATAL NA.Error, PL.SizeMismatch, Thread.Alerted, Wr.Failure*>
  VAR
    shiftVan      := 2 - smooth - vanishing;
    shiftSmallVan := (vanishing - smallVanishing) DIV 2 - 1;
    bigVanAtom := NEW(S.T).fromArray(
                    ARRAY OF R.T{RT.Half, R.Zero, -RT.Half});

    hdual  := BSpl.GeneratorMask(smooth);
    gdual0 := BSpl.WaveletMask(smooth, vanishing);
    hdualvan := SIntPow.MulPower(hdual, bigVanAtom, vanishing).translate(
                  shiftVan);

    (*GeneratorMask uses factors (0.5,0.5), Vanishing moments (0.5,-0.5),
       this results in (0.25,-0.25) but the lifting mask is convolved with
       (0.5,-0.5).  We have to compensate that with 'scale'. *)
    (*Translate mask so that it is symmetric again.*)
    hdualsmallvan := SIntPow.MulPower(
                       BSpl.GeneratorMask(
                         smooth + vanishing - smallVanishing), bigVanAtom,
                       smallVanishing).translate(
                       shiftVan + shiftSmallVan).scale(
                       RIntPow.MulPower(
                         R.One, R.Two, vanishing - smallVanishing));
    gdual0smallvan := BSpl.WaveletMask(smooth + vanishing - smallVanishing,
                                       smallVanishing).translate(
                        shiftSmallVan);

    (*compensate factors (0.5,0.5) from the mask*)
    hdualnovan := BSpl.GeneratorMask(smooth + vanishing).translate(
                    shiftVan).scale(
                    RIntPow.MulPower(R.One, R.Two, vanishing));

    gdual0novan := BSpl.WaveletMask(smooth + vanishing, 0);

    dualBasis := NEW(            (*StdFilterBasis*)
                   NoVanFilterBasis, lp := hdual, hp := gdual0,
                   lpVan := hdualvan, lpSmallVan := hdualsmallvan,
                   hpSmallVan := gdual0smallvan, lpNoVan := hdualnovan,
                   hpNoVan := gdual0novan, shiftSmallVan := shiftSmallVan,
                   negateWavelet := (vanishing DIV 2) MOD 2 # 0);

    (*
    mc := MatchCoef{NEW(S.T).fromArray(
                      ARRAY OF R.T{1.0D0, 0.0D0, 0.0D0, 1.0D0}), 1.5D0};
    *)
    mc := MatchPatternSmooth(
            target, dualBasis, numLevels, numTranslates, smoothWeight);
    vanAtom := NEW(S.T).fromArray(ARRAY OF R.T{RT.Half, -RT.Half});
    s := SIntPow.MulPower(
           mc.lift.translate(shiftVan DIV 2), vanAtom, vanishing);
    gdual0a := gdual0.scale(mc.wavelet0Amp);
    gduala  := gdual0a.superpose(hdual.upConvolve(s, 2));
    gdual := gdual0.superpose(
               hdual.scale(R.One / mc.wavelet0Amp).upConvolve(s, 2));
    (*
gdual := GetLiftedPrimalGeneratorMask(hdual,gdual0,MatchCoef{lift:=s,wavelet0Amp:=mc.wavelet0Amp,targetAmp:=R.One});
    *)
    unit          := IIntPow.Power(2, numLevels);
    twopow        := FLOAT(unit, R.T);
    grid          := R.One / twopow;
    psidual0      := Refn.Refine(gdual0a, hdual, numLevels);
    psidual       := Refn.Refine(gduala, hdual, numLevels);
    leftpsidual0  := FLOAT(psidual0.getFirst(), R.T) * grid;
    rightpsidual0 := FLOAT(psidual0.getLast(), R.T) * grid;
    leftpsidual   := FLOAT(psidual.getFirst(), R.T) * grid;
    rightpsidual  := FLOAT(psidual.getLast(), R.T) * grid;
    lefttarget    := FLOAT(target.getFirst(), R.T) * grid;
    righttarget   := FLOAT(target.getLast(), R.T) * grid;

  CONST
    ymin = -1.5D0;
    ymax = 1.5D0;

  BEGIN
    IO.Put(
      Fmt.FN(
        "orig with all vanishing moments:\nlp %s,\nhp %s\n"
          & "only some of the vanishing moments, extended to full vanishing:\nlp %s,\nhp %s\n",
        ARRAY OF
          TEXT{
          SF.Fmt(hdualvan), SF.Fmt(gdual0),
          SF.Fmt(SIntPow.MulPower(hdualsmallvan, vanAtom,
                                  vanishing - smallVanishing).translate(
                   -shiftSmallVan)),
          SF.Fmt(SIntPow.MulPower(gdual0smallvan, vanAtom,
                                  vanishing - smallVanishing).translate(
                   -shiftSmallVan))}));
    IO.Put(
      Fmt.FN("optimal lift %s,\ncyclic wrap of gdual %s\n\n"
             (* & "hsdual\n%s\n%s\n\n" & "gdual0\n%s\n%s\n\n" &
                "gdual\n%s\n%s\n"*),
             ARRAY OF
               TEXT{SF.Fmt(s), SF.Fmt(gdual.alternate().wrapCyclic(3))
               (*, SF.Fmt(hdual.upConvolve(s,2)),
                  SF.Fmt(SIntPow.MulPower(hdualnovan.upConvolve(mc.lift,2),
                  vanatom, vanishing)), SF.Fmt(gdual0),
                  SF.Fmt(SIntPow.MulPower(gdual0novan, vanatom,
                  vanishing)), SF.Fmt(gdual), SF.Fmt(SIntPow.MulPower(
                  GetLiftedPrimalGeneratorMask( hdualnovan, gdual0novan,
                  mc).alternate(), vanatom, vanishing))*)}));
    CASE 2 OF
    | 0 =>
        PL.Init();
        WP.PlotBiorthogonal(hdual, gdual, numLevels);
        PL.Exit();
    | 1 =>
        PL.Init();
        PL.SetEnvironment(
          MIN(lefttarget, MIN(leftpsidual, leftpsidual0)),
          MAX(righttarget, MAX(rightpsidual, rightpsidual0)), ymin, ymax);
        PL.SetFGColorDiscr(3);
        PL.PlotLines(V.ArithSeq(target.getNumber(), lefttarget, grid)^,
                     target.getData()^);
        PL.SetFGColorDiscr(1);
        PL.PlotLines(V.ArithSeq(psidual0.getNumber(), leftpsidual0, grid)^,
                     psidual0.getData()^);
        PL.SetFGColorDiscr(4);
        PL.PlotLines(V.ArithSeq(psidual.getNumber(), leftpsidual, grid)^,
                     psidual.getData()^);
        PL.Exit();
    | 2 =>
    ELSE
      <*ASSERT FALSE*>
    END;
    RETURN dualBasis.getFilterBank(mc);
  END TestMatchPatternSmooth;


(*create symmetric clip of the sin x / x curve*)
PROCEDURE SincVector (size, width: CARDINAL): V.T =
  VAR
    z := V.New(2 * size + 1);
    k := RT.Pi / FLOAT(2 * width, R.T);
  BEGIN
    z[size] := R.One;
    FOR i := 1 TO size - 1 DO
      VAR
        x := FLOAT(i, R.T) * k;
        y := RT.Sin(x) / x;
      BEGIN
        z[size + i] := y;
        z[size - i] := y;
      END;
    END;
    RETURN z;
  END SincVector;

PROCEDURE GaussianVector (size, width: CARDINAL): V.T =
  VAR
    z := V.New(2 * size + 1);
    k := R.One / FLOAT(width, R.T);
  BEGIN
    z[size] := R.One;
    FOR i := 1 TO size - 1 DO
      VAR
        x := FLOAT(i, R.T) * k;
        y := RT.Exp(-x * x);
      BEGIN
        z[size + i] := y;
        z[size - i] := y;
      END;
    END;
    RETURN z;
  END GaussianVector;

(*a sinc function that is modulated such that it covers band ready for
   dyadic partitioning*)
PROCEDURE ModulateReal (x: V.T; period: R.T): V.T =
  VAR
    size := (NUMBER(x^) - 1) DIV 2;
    z    := V.New(2 * size + 1);
    k    := RT.TwoPi / FLOAT(period, R.T);
  BEGIN
    z[size] := x[size];
    FOR i := 1 TO size - 1 DO
      VAR c := RT.Cos(FLOAT(i, R.T) * k);
      BEGIN
        z[size + i] := x[size + i] * c;
        z[size - i] := x[size - i] * c;
      END;
    END;
    RETURN z;
  END ModulateReal;

PROCEDURE ModulateImag (x: V.T; period: R.T): V.T =
  VAR
    size := (NUMBER(x^) - 1) DIV 2;
    z    := V.New(2 * size + 1);
    k    := RT.TwoPi / FLOAT(period, R.T);
  BEGIN
    z[size] := R.Zero;
    FOR i := 1 TO size - 1 DO
      VAR c := RT.Sin(FLOAT(i, R.T) * k);
      BEGIN
        z[size + i] := x[size + i] * c;
        z[size - i] := -x[size - i] * c;
      END;
    END;
    RETURN z;
  END ModulateImag;


(*multiplicate signal with ramp (linear progression)*)
PROCEDURE MulRamp (x: S.T; scale: R.T; ): S.T =
  VAR
    xData := x.getData();
    zData := NEW(V.T, x.getNumber());
    j     := 0;
  BEGIN
    FOR i := x.getFirst() TO x.getLast() DO
      zData[j] := xData[j] * FLOAT(i, R.T) * scale;
      INC(j);
    END;
    RETURN NEW(S.T).fromVector(zData, x.getFirst());
  END MulRamp;

PROCEDURE CheckVanishingMoments () =
  CONST
    size  = 1024;
    width = 300;
    step  = 50;
    scale = R.One / FLOAT(size, R.T);
  VAR difSig := NEW(S.T);
  BEGIN
    CASE 2 OF
    | 0 =>
        difSig := NEW(S.T).fromVector(GaussianVector(size, width), -size);
    | 1 =>
        difSig := NEW(S.T).fromVector(GaussianVector(size, width), -size);
        difSig := difSig.translate(-size DIV 4).scale(0.9D0).superpose(
                    difSig.translate(size DIV 3).scale(0.3D0));
    | 2 =>
        difSig :=
          NEW(S.T).fromVector(V.NewUniform(size + 1, R.Half), -size DIV 2);
    ELSE
    END;

    FOR i := 0 TO 20 DO
      (*
      VAR
        minX := FLOAT(difSig.getFirst(), R.T);
        maxX := FLOAT(difSig.getLast(), R.T);
      BEGIN
        PL.Init();
        PL.SetEnvironment(minX, maxX, -R.One, R.One);
        PL.PlotLines(V.ArithSeq(difSig.getNumber(), minX, R.One)^,
                     difSig.getData()^);
        PL.Exit();
      END;
      *)
      (*versions of the Sig multiplied with ...*)
      VAR
        polyDifSig      := difSig; (*...  power function*)
        chebyDifSig     := difSig; (*...  chebyshev function*)
        chebyDifSigPrev := MulRamp(difSig, scale);
      BEGIN
        FOR j := 0 TO i DO
          VAR
            polyMom := polyDifSig.sum() / VT.Norm1(polyDifSig.getData());
            chebyMom := chebyDifSig.sum() / VT.Norm1(chebyDifSig.getData());
          BEGIN
            IO.Put(
              Fmt.FN("%2s. moment: %20s, %20s\n",
                     ARRAY OF
                       TEXT{Fmt.Int(j), RF.Fmt(polyMom), RF.Fmt(chebyMom)}));
          END;
          (*
          VAR
            minX     := FLOAT(difSig.getFirst(), R.T);
            maxX     := FLOAT(difSig.getLast(), R.T);
            abscissa := V.ArithSeq(difSig.getNumber(), minX, R.One);
          BEGIN
            PL.Init();
            PL.SetEnvironment(minX, maxX, -R.One, R.One);
            PL.SetFGColorDiscr(2);
            PL.PlotLines(abscissa^, polyDifSig.getData()^);
            PL.SetFGColorDiscr(3);
            PL.PlotLines(abscissa^, chebyDifSig.getData()^);
            PL.Exit();
          END;
          *)

          polyDifSig := MulRamp(polyDifSig, scale);
          (*apply recursive construction of chebyshev polynomials*)
          VAR
            tmp := MulRamp(chebyDifSig, scale * R.Two).superpose(
                     chebyDifSigPrev.negate());
          BEGIN
            chebyDifSigPrev := chebyDifSig;
            chebyDifSig := tmp;
          END;
        END;
        IO.Put("\n");
      END;
      difSig :=
        difSig.translate(step).superpose(difSig.translate(-step).negate());
    END;
  END CheckVanishingMoments;


PROCEDURE Test () =
  CONST
    numlevel = 6;
    unit     = 64;
  TYPE
    Example = {matchBSpline, matchBSplineVan, matchBSplineWavelet,
               matchRamp, matchRampSmooth, matchSincSmooth, matchGaussian,
               matchLongRamp, matchMassPeak, checkVanishingMoments,
               testSSE, testInverseDSSE, testDeriveWSSE, testNormalEqu};
  <*FATAL BSpl.DifferentParity*>
  BEGIN
    CASE Example.matchRampSmooth OF
    | Example.matchBSpline =>
        TestMatchPattern(
          Refn.Refine(
            S.One, BSpl.GeneratorMask(4).scale(2.0D0), 7).translate(-50),
          numlevel, 4, 0, 5);
    | Example.matchBSplineVan =>
        TestMatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(1), 7).translate(10),
          numlevel, 4, 2, 5);
    | Example.matchRamp =>
        (* The figures given here previously was wrong because the
           generator was convolved with (1,0,-1) instead of (1,-1)
           numTranslates 5, size 1917, residuum 0.00340514677538585,
           V11{0.186869299925214, 0.269986933917237, 0.670508585560263,
           0.649776682132423, -0.175806649674353, -0.875993675942413,
           -0.856283049545732, -0.458477950438848, -0.31397715987086,
           -0.11516417311729, ...} 0.330691666379811 *)
        TestMatchPattern(NEW(S.T).fromArray(
                           V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                           192 - 256), numlevel, 3, 1, 5);
    | Example.matchRampSmooth =>
        (*
          EVAL TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), numlevel, 4, 2, 5, 50.0D0);
          EVAL TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), numlevel, 4, 4, 5, 20.0D0);
        *)

        CONST size = 20 * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
                 NEW(S.T).fromArray(
                   V.ArithSeq(size + 1, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
                   -size DIV 2), numlevel, 3, 7, 1, 10, 1.0D-12);
        END;

      (*
      FOR scale := 3 TO 10 DO
        VAR size := 2 * scale * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
            NEW(S.T).fromArray(
              V.ArithSeq(size, -1.0D0, 2.0D0 / FLOAT(size, R.T))^,
              -size DIV 2), numlevel, 3, 7, 1, 10, 1.0D-10);
        END;
      END;
      *)

    | Example.matchBSplineWavelet =>
        (*
          TestMatchPattern(
            Refn.Refine(
              BSpl.WaveletMask(2, 8), BSpl.GeneratorMask(2), numlevel).scale(
            FLOAT(unit,R.T)  ).translate(10), numlevel, 2, 8, 5);
        *)
        EVAL TestMatchPatternSmooth(
               Refn.Refine(BSpl.WaveletMask(2, 8), BSpl.GeneratorMask(2),
                           numlevel).scale(FLOAT(unit, R.T)).translate(50),
               numlevel, 2, 8, 8, 5, 1.0D-10);
    | Example.matchSincSmooth =>
        (*
          TestMatchPattern(
            NEW(S.T).fromArray(V.Neg(SincVector(2048, unit))^, unit - 2048), numlevel,
            4, 6, 10);
        *)
        FOR scale := 3 TO 10 DO
          VAR size := 5 * scale * unit;
          BEGIN
            TestMatchPattern(
              NEW(S.T).fromArray(V.Neg(ModulateReal(
                                         SincVector(size, scale * unit),
                                         FLOAT(scale * unit, R.T) * 4.0D0
                                           / 3.0D0))^, unit - size),
              numlevel, 4, 0, 20);
          END;
          (** which scale achieves best match?
             3 - 22.1834569283071
             4 -  5.45720352492108
             5 -  1.12455306018079
             6 -  0.32131075741861
             7 -  0.0526881888961695
             8 -  0.106707504091051
             9 -  3.60586130516876
            10 -  0.202799707663614
          *)
          VAR size := 5 * scale * unit;
          BEGIN
            TestMatchPattern(
              NEW(S.T).fromArray(V.Neg(ModulateImag(
                                         SincVector(size, scale * unit),
                                         FLOAT(scale * unit, R.T) * 4.0D0
                                           / 3.0D0))^, unit - size),
              numlevel, 3, 1, 20);
          END;
          (** which scale achieves best match?
             3 - 40.9491273229196
             4 -  9.13161843532297
             5 -  3.38459627541558
             6 -  1.7937034581564
             7 -  0.653510798955025
             8 -  0.91561743745227
             9 -  0.255038945635696
            10 -  0.476846592814738
          *)
        END;
      (*
      EVAL TestMatchPatternSmooth(
        NEW(S.T).fromArray(V.Neg(SincVector(2048, unit))^, unit - 2048), numlevel,
        4, 6, 10, 1.0D-3);
      *)
    | Example.matchGaussian =>
        (*
          FOR scale := 3 TO 10 DO
            IO.Put("scale: "&Fmt.Int(scale)&"\n");
            VAR size := 5 * scale * unit;
            BEGIN
              TestMatchPattern(
                NEW(S.T).fromArray(
                  ModulateReal(V.Neg(GaussianVector(size, scale * unit)),
                               FLOAT(scale * unit, R.T))^, unit - size),
                numlevel, 4, 0, 20);
            END;
            VAR size := 5 * scale * unit;
            BEGIN
              TestMatchPattern(NEW(S.T).fromArray(
                             ModulateImag(GaussianVector(size, scale * unit),
                                          FLOAT(scale * unit, R.T))^,
                             unit - size), numlevel, 3, 1, 20);
            END;
          END;
        *)
        CONST
          scale = 6;
          size  = 5 * scale * unit;
        BEGIN
          EVAL TestMatchPatternSmooth(
                 NEW(S.T).fromArray(
                   ModulateReal(V.Neg(GaussianVector(size, scale * unit)),
                                FLOAT(scale * unit, R.T))^, unit - size),
                 numlevel, 4, 4, 0, 6, 10.0D0);
        END;
    | Example.matchLongRamp =>
        (*matching a pattern with 1 vanishing moment with a wavelet of 9
           vanishing moments can't work obviously*)
        TestMatchPattern(NEW(S.T).fromArray(
                           V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,
                           unit - 1024), numlevel, 3, 9, 5);
      (*
      EVAL TestMatchPatternSmooth(
        NEW(S.T).fromArray(
          V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,32 -1024), numlevel, 3, 1,
        5, 0.0D-4);
      *)
    | Example.matchMassPeak =>
        CONST
          clipFirst  = 15500;
          clipNumber = 2500;
        <*FATAL OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted*>
        VAR
          rd := FileRd.Open(
                  "/home/thielema/projects/industry/bruker/data/Datasets"
                    & "/T/Normal/spectrum_28_23.dat");
          data  := MF.Lex(rd);
          dataX := M.GetColumn(data, 0);
          dataY := M.GetColumn(data, 1);
          clipX := V.FromArray(SUBARRAY(dataX^, clipFirst, clipNumber));
          clipY := V.FromArray(SUBARRAY(dataY^, clipFirst, clipNumber));
        <*FATAL PL.SizeMismatch*>
        BEGIN
          PL.Init();
          PL.SetEnvironment(dataX[FIRST(dataX^)], dataX[LAST(dataX^)],
                            VFs.Min(dataY^), VFs.Max(dataY^));
          PL.PlotLines(dataX^, dataY^);

          PL.SetEnvironment(clipX[FIRST(clipX^)], clipX[LAST(clipX^)],
                            VFs.Min(clipY^), VFs.Max(clipY^));
          PL.PlotLines(clipX^, clipY^);
          PL.Exit();
        END;
    | Example.checkVanishingMoments => CheckVanishingMoments();
    | Example.testSSE =>
        TestSSE(V.FromArray(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0}));
        TestSSE(V.FromArray(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0}));
    | Example.testInverseDSSE =>
        TestInverseDSSE(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 0.1D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0});
    | Example.testDeriveWSSE => TestDeriveWSSE();
    | Example.testNormalEqu => TestNormalEqu();
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestMatchWavelet.
