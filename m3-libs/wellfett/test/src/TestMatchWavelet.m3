MODULE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT LongRealComplexFast AS C;
IMPORT LongRealComplexTrans AS CT;
IMPORT Integer32IntegerPower AS IIntPow;
IMPORT LongRealIntegerPower AS RIntPow;

IMPORT LongRealVectorFast AS V;
IMPORT LongRealVectorSupport AS VS;
IMPORT LongRealVectorTrans AS VT;

IMPORT LongRealMatrixFast AS M;
IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealFunctional AS Fn;
IMPORT LongRealFunctionalDeriv2 AS FnD;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
(*IMPORT LongRealComplexVectorFmtLex AS CVF;*)
IMPORT LongRealMatrixFmtLex AS MF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
IMPORT IntBiList;

IMPORT NADefinitions AS NA;

(*{RT.Half, R.Zero, -RT.Half} instead of {R.One, R.Zero, -R.One} has the
   advantage that the sum of the coefficients of the primal filter doesn't
   change.*)

PROCEDURE MatchPattern (target                               : S.T;
                        levels, smooth, vanishing, translates: CARDINAL)
  RAISES {BSpl.DifferentParity} =
  (*The degree of freedom, i.e.  the number of parameters to minimize for,
     is 2*translates*)
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  VAR
    hdual := BSpl.GeneratorMask(smooth);
    gdual := BSpl.WaveletMask(smooth, vanishing);
    vancore := SIntPow.MulPower(
                 hdual, NEW(S.T).fromArray(
                          ARRAY OF R.T{RT.Half, R.Zero, -RT.Half}),
                 vanishing).translate(2 - smooth - vanishing);
    phivan := Refn.Refine(vancore, hdual, levels);
    psi    := Refn.Refine(gdual, hdual, levels);

    unit   := IIntPow.Power(2, levels);
    twonit := 2 * unit;
    first  := MIN(psi.getFirst(), phivan.getFirst() - twonit * translates);
    last := MAX(
              psi.getLast(), phivan.getLast() + twonit * (translates - 1));
    size := last - first + 1;

    twopow    := FLOAT(unit, R.T);
    grid      := R.One / twopow;
    wavescale := twopow;
    abscissa  := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

    targetvec := V.New(size);
    basis     := M.New(2 * translates + 1, size);

    coef: LA.LS;

    waveymin := MIN(V.Min(phivan.getData()^), V.Min(psi.getData()^));
    waveymax := MAX(V.Max(phivan.getData()^), V.Max(psi.getData()^));
    ymin := 1.1D0 * MIN(V.Min(target.getData()^), wavescale * waveymin);
    ymax := 1.1D0 * MAX(V.Max(target.getData()^), wavescale * waveymax);

  (*
    CONST
      ymin = -1.5D0;
      ymax = 1.5D0;
  *)

  BEGIN
    PL.Init();
    PL.SetFGColorDiscr(7);
    PL.SetEnvironment(
      abscissa[FIRST(abscissa^)], abscissa[LAST(abscissa^)], ymin, ymax,
      axis := PL.TileSet{PL.Tile.box, PL.Tile.ticks, PL.Tile.axes,
                         PL.Tile.gridMajor, PL.Tile.gridMinor});

    psi.scale(wavescale).clipToArray(first, basis[LAST(basis^)]);
    PL.SetFGColorDiscr(1);
    PL.PlotLines(abscissa^, basis[LAST(basis^)]);
    PL.SetFGColorDiscr(2);
    FOR j := -translates TO translates - 1 DO
      phivan.scale(wavescale).clipToArray(
        first - twonit * j, basis[j + translates]);
      PL.PlotLines(abscissa^, basis[j + translates]);
    END;

    target.clipToArray(first, targetvec^);
    PL.SetFGColorDiscr(3);
    PL.PlotLines(abscissa^, targetvec^);

    (*
    IO.Put(Fmt.FN("normal matrix %s, right hand side %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(M.MulMMA(basis)),
                         VF.Fmt(M.MulV(basis, targetvec))}));
    *)

    coef :=
      LA.LeastSquares(basis, ARRAY OF V.T{targetvec},
                      flags := LA.LSFlagSet{LA.LSFlag.transposed})[0];

    IO.Put(Fmt.FN("translates %s, size %s, residuum %s, %s\n",
                  ARRAY OF
                    TEXT{Fmt.Int(translates), Fmt.Int(size),
                         RF.Fmt(coef.res), VF.Fmt(coef.x)}));

    PL.SetFGColorDiscr(4);
    PL.PlotLines(abscissa^, M.MulTV(basis, coef.x)^);

    PL.Exit();
  END MatchPattern;

(** SSE  - Square Smoothness Estimate,
           that is the estimate that depends on the sum of the squares of the eigenvalues
    WSSE - Weighted Square Smoothness Estimate,
           workaround that takes into account that the gdual0 filter (alias psi0)
           was amplified within the linear least squares term
*)

PROCEDURE ComputeSSE (READONLY y: ARRAY [0 .. 2] OF R.T): R.T =
  <*FATAL NA.Error*>
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    dif := 3.0D0 * p2 - p12;
  BEGIN
    RETURN dif * dif + 2.0D0 * p12 * p12;
  END ComputeSSE;

PROCEDURE ComputeDSSE (READONLY y: ARRAY [0 .. 2] OF R.T): V.T =
  <*FATAL NA.Error*>
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    p1d := 12.0D0 * (p1 * (p12 - p2));
    p2d := 6.0D0 * (3.0D0 * p2 - p12)
             * 2.0D0 (*because p2' contains 2a'a and so on*);
    z := V.New(NUMBER(y));
  BEGIN
    FOR i := 0 TO LAST(y) DO z[i] := p1d + y[i] * p2d; END;
    RETURN z;
  END ComputeDSSE;

PROCEDURE ComputeDDSSE (READONLY y: ARRAY [0 .. 2] OF R.T): M.T =
  <*FATAL NA.Error*>
  (*derived with mathematica*)
  VAR
    p1 := VS.Sum(y);
    p2 := VS.Inner(y, y);

    z := M.New(NUMBER(y), NUMBER(y));
  BEGIN
    FOR i := 0 TO LAST(y) DO
      FOR j := i TO LAST(y) DO
        z[i, j] := (p1 + y[i] - y[j]) * (p1 - y[i] + y[j]);
        z[j, i] := z[i, j];
      END;
      z[i, i] := z[i, i] + (3.0D0 * y[i] - 2.0D0 * p1) * y[i] + p2;
    END;
    RETURN M.Scale(z, 24.0D0);
  END ComputeDDSSE;

PROCEDURE TestSSE (x: V.T) =
  VAR
    dx0 := V.FromArray(ARRAY OF R.T{1.0D-8, 0.0D0, 0.0D0});
    dx1 := V.FromArray(ARRAY OF R.T{0.0D0, 1.0D-8, 0.0D0});
    dx2 := V.FromArray(ARRAY OF R.T{0.0D0, 0.0D0, 1.0D-8});
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  BEGIN
    VAR
      rho     := ComputeSSE(x^);
      rho0    := ComputeSSE(V.Add(x, dx0)^);
      rho1    := ComputeSSE(V.Add(x, dx1)^);
      rho2    := ComputeSSE(V.Add(x, dx2)^);
      gradrho := V.Scale(ComputeDSSE(x^), 1.0D-8);
    BEGIN
      IO.Put(
        Fmt.FN("rho %s, difrho={%s,%s,%s}, approxdiff=%s\n",
               ARRAY OF
                 TEXT{RF.Fmt(rho), RF.Fmt(rho0 - rho), RF.Fmt(rho1 - rho),
                      RF.Fmt(rho2 - rho), VF.Fmt(gradrho)}));
    END;
    VAR
      gradrho   := ComputeDSSE(x^);
      gradrho0  := ComputeDSSE(V.Add(x, dx0)^);
      gradrho1  := ComputeDSSE(V.Add(x, dx1)^);
      gradrho2  := ComputeDSSE(V.Add(x, dx2)^);
      jacobirho := M.Scale(ComputeDDSSE(x^), 1.0D-8);
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
      VAR ax := ComputeDSSE(y^);
      BEGIN
        IF VT.Norm1(V.Sub(ax, x)) <= tol * VT.Norm1(x) THEN RETURN y; END;
        y :=
          V.Add(y, LA.LeastSquares(
                     ComputeDDSSE(y^), ARRAY OF V.T{V.Sub(x, ax)})[0].x);
        (*
          IO.Put(Fmt.FN("y %s, DSSE(y) %s\n",
                       ARRAY OF TEXT{VF.Fmt(y), VF.Fmt(ComputeDSSE(y^))}));
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
      Fmt.FN("x %s, ComputeDSSE(y) %s, y %s\n",
             ARRAY OF TEXT{VF.Fmt(x), VF.Fmt(ComputeDSSE(y^)), VF.Fmt(y)}));
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
    gdual   := gdual0.superpose(s.upsample(2).convolve(hdual));
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
    RETURN
      FnD.T{zeroth := ComputeSSE(hsums^), first :=
            M.MulV(dsums, ComputeDSSE(hsums^)), second :=
            M.Mul(M.Mul(dsums, ComputeDDSSE(hsums^)), M.Transpose(dsums))};
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
    hsdual   := s.upsample(2).convolve(hdual);
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

    sse   := ComputeSSE(hsums^);
    dsse  := ComputeDSSE(hsums^);
    ddsse := ComputeDDSSE(hsums^);

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

PROCEDURE TranslatesBasis (generatorvan: S.T;
                           first       : INTEGER;
                           twonit, size: CARDINAL;
                           translates  : CARDINAL  ): M.T =
  VAR basis := M.New(2 * translates, size);
  BEGIN
    FOR j := -translates TO translates - 1 DO
      generatorvan.clipToArray(first - twonit * j, basis[j + translates]);
      (*
            targetCor[j + translates] :=
              VS.Inner(basis[j + translates], targetVec^);
      *)
    END;
    RETURN basis;
  END TranslatesBasis;


TYPE
  MatrixElem = RECORD
                 enX, enY: IntBiList.Node;
                 value   : R.T;
               END;

PROCEDURE FindMinMatrix (READONLY mat               : M.TBody;
                                  enabledX, enabledY: IntBiList.T; ):
  MatrixElem =
  VAR
    result  : MatrixElem;
    enX, enY: IntBiList.Node;
  BEGIN
    result.value := R.PosInf;
    enX := enabledX.getlo();
    WHILE enX # NIL DO
      enY := enabledY.getlo();
      WHILE enY # NIL DO
        IF result.value
             > mat[enabledX.getvalue(enX), enabledY.getvalue(enY)] THEN
          result.value :=
            mat[enabledX.getvalue(enX), enabledY.getvalue(enY)];
          result.enX := enX;
          result.enY := enY;
        END;
        enY := enabledY.getnext(enY);
      END;
      enX := enabledX.getnext(enX);
    END;
    RETURN result;
  END FindMinMatrix;

(*Compute a kind of distance of a given eigenspectrum to the one of the
   transfer matrix of the B-Spline of corresponding order*)
PROCEDURE EigenDistBSpline (specX: REF ARRAY OF C.T): R.T =
  VAR
    enabledX, enabledY := NEW(IntBiList.T).init();
    specY              := NEW(V.T, NUMBER(specX^));
    distMatrix         := NEW(M.T, NUMBER(specX^), NUMBER(specX^));
  BEGIN
    VAR eigY := R.One;
    BEGIN
      FOR i := FIRST(specX^) TO LAST(specX^) DO
        EVAL enabledX.addhi(i);
        EVAL enabledY.addhi(i);
        eigY := eigY / R.Two;
        specY[i] := eigY;
      END;
      specY[LAST(specX^)] := R.Two * eigY;
    END;
    (*
    IO.Put(Fmt.FN("compute distance between spectra\n%s%s\n",
                  ARRAY OF TEXT{CVF.Fmt(specX), VF.Fmt(specY)}));
    *)
    FOR i := FIRST(specX^) TO LAST(specX^) DO
      FOR j := FIRST(specX^) TO LAST(specX^) DO
        distMatrix[i, j] :=
          CT.AbsSqr(C.T{specX[i].re - specY[j], specX[i].im});
      END;
    END;
    VAR sum := R.Zero;
    BEGIN
      FOR i := FIRST(specX^) TO LAST(specX^) DO
        VAR min := FindMinMatrix(distMatrix^, enabledX, enabledY);
        BEGIN
          sum := sum + min.value;
          enabledX.rem(min.enX);
          enabledY.rem(min.enY);
        END;
      END;
      RETURN sum;
    END;
  END EigenDistBSpline;


PROCEDURE GetLiftedPrimalGeneratorMask (         hdual, gdual0: S.T;
                                        READONLY mc           : MatchCoef):
  S.T =
  VAR
    hsdual := mc.lift.upsample(2).convolve(hdual);
    gdual  := gdual0.superpose(hsdual.scale(R.One / mc.wavelet0Amp));
  BEGIN
    RETURN gdual.alternate();
  END GetLiftedPrimalGeneratorMask;

TYPE
  MatchCoef =
    RECORD
      lift                  : S.T;
      wavelet0Amp, targetAmp: R.T;  (*coefficient of the target function*)
    END;

PROCEDURE MatchPatternSmooth (target                 : S.T;
                              hdual, gdual0, hdualvan: S.T;
                              hdualnovan, gdual0novan: S.T;
                              levels, translates     : CARDINAL;
                              smoothWeight           : R.T;      ):
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
      der := FnD.Add(
               DeriveDist(normalMat, waveletCor, waveletNormSqr, y),
               FnD.Scale(DeriveSSE(hdualvan, gdual0, y), smoothWeight));
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
            FnD.Add(
              DeriveDist(normalMat, waveletCor, waveletNormSqr, yp),
              FnD.Scale(DeriveSSE(hdualvan, gdual0, yp), smoothWeight));
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
    generatorvan := Refn.Refine(hdualvan, hdual, levels);
    wavelet      := Refn.Refine(gdual0, hdual, levels);

    unit   := IIntPow.Power(2, levels);
    twonit := 2 * unit;
    first := MIN(wavelet.getFirst(),
                 generatorvan.getFirst() - twonit * translates);
    last := MAX(wavelet.getLast(),
                generatorvan.getLast() + twonit * (translates - 1));
    size := last - first + 1;

    waveletVec     := wavelet.clipToVector(first, size);
    waveletNormSqr := V.Inner(waveletVec, waveletVec);
    targetVec      := target.clipToVector(first, size);
    targetNormSqr  := V.Inner(targetVec, targetVec);
    (* the target vector might have been clipped, thus
       V.Inner(target.getData(),target.getData()) may be different *)

    basis := TranslatesBasis(generatorvan, first, twonit, size, translates);
    normalMat  := M.MulMMA(basis);
    targetCor  := M.MulV(basis, targetVec);
    waveletCor := M.MulV(basis, waveletVec);

  BEGIN
    (*
    CheckDerivatives();

    IO.Put(Fmt.FN("normal matrix %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(normalMat)}));
    *)

    VAR yfirst := -translates;

    CONST constWavAmp = TRUE;

    PROCEDURE SplitParamVec (x: V.T; wavAmp: R.T): MatchCoef =
      BEGIN
        IF NOT constWavAmp THEN wavAmp := x[LAST(x^)]; END;
        RETURN
          MatchCoef{NEW(S.T).fromArray(
                      SUBARRAY(x^, FIRST(x^), NUMBER(x^) - 1), yfirst),
                    wavAmp, R.One};
      END SplitParamVec;

    <*UNUSED*>
    PROCEDURE ComputeOptCritDeriv (x: V.T): FnD.T RAISES {NA.Error} =
      VAR
        (*SplitParamVec may return initWavelet0Amp as waveletAmp and this
           won't work if we compute the real derivative instead of a finite
           difference.*)
        mc := SplitParamVec(x, initWavelet0Amp);
        derdist := DeriveDist(normalMat, targetCor, targetNormSqr, mc.lift);
        derwavdist := ExtendDervTarget(
                        derdist, mc.lift.getData(), mc.wavelet0Amp,
                        waveletVec, waveletCor, targetVec);
      BEGIN
        (*
        IO.Put(
          Fmt.FN("y %s, cf %s\n", ARRAY OF TEXT{SF.Fmt(y), RF.Fmt(cf)}));
        *)
        RETURN FnD.Add(
                 derwavdist, FnD.Scale(DeriveWSSE(hdualvan, gdual0,
                                                  mc.lift, mc.wavelet0Amp),
                                       smoothWeightFade));
      END ComputeOptCritDeriv;

    PROCEDURE ComputeOptCritDiff (x: V.T): FnD.T RAISES {NA.Error} =

      PROCEDURE SquareSmoothEstimate (x: V.T): R.T =
        VAR
          hsums := GetLiftedPrimalGeneratorMask(
                     hdualnovan, gdual0novan,
                     SplitParamVec(x, initWavelet0Amp)).wrapCyclic(
                     3).getData();

        BEGIN
          (*
          IO.Put(Fmt.FN("Compute SSE of %s, sum %s\n",ARRAY OF TEXT{VF.Fmt(hsums),RF.Fmt(V.Sum(hsums^))}));
          *)
          RETURN ComputeSSE(hsums^);
        END SquareSmoothEstimate;

      PROCEDURE TransitionSpecRad (x: V.T): R.T RAISES {NA.Error} =
        VAR
          hprimal := GetLiftedPrimalGeneratorMask(
                       hdualnovan, gdual0novan,
                       SplitParamVec(x, initWavelet0Amp));
        BEGIN
          (*
          IO.Put("TransitionSpecRad "&Fmt.Int(ncall)&"\n");
          INC(ncall);
          *)
          RETURN Refn.TransitionSpecRad(hprimal);
        END TransitionSpecRad;

      PROCEDURE TransitionBSpline (x: V.T): R.T RAISES {NA.Error} =
        VAR
          hprimal := GetLiftedPrimalGeneratorMask(
                       hdualnovan, gdual0novan,
                       SplitParamVec(x, initWavelet0Amp));
        BEGIN
          RETURN EigenDistBSpline(Refn.TransitionEV(hprimal).eigenvalues);
        END TransitionBSpline;

      PROCEDURE TransitionFrobenius (x: V.T): R.T =
        VAR
          hprimal := GetLiftedPrimalGeneratorMask(
                       hdualnovan, gdual0novan,
                       SplitParamVec(x, initWavelet0Amp));
          hh := hprimal;
          (*frob0 := M.Trace(M.MulMMA(Refn.RadicBandMatrix(hh)));*)
          frob1           := R.Zero;
          alter: [0 .. 1] := 1;
          n               := hh.getLast();
        BEGIN
          (*because of the symmetry we can content ourselves with the half
             autocorrelated mask*)
          FOR i := hh.getFirst() TO -1 DO
            VAR val := hh.getValue(i);
            BEGIN
              frob1 := frob1 + FLOAT(n + alter, R.T) * val * val;
              alter := 1 - alter;
            END;
          END;
          VAR val := hh.getValue(0);
          BEGIN
            frob1 := R.Two * frob1 + FLOAT(n + alter, R.T) * val * val;
          END;
          (*
          IO.Put(Fmt.FN("Frobenius norm %s =?= %s\n",
                        ARRAY OF TEXT{RF.Fmt(frob0), RF.Fmt(frob1)}));
          *)
          RETURN frob1;
        END TransitionFrobenius;

      VAR
        mc := SplitParamVec(x, initWavelet0Amp);
        derdist := DeriveDist(normalMat, targetCor, targetNormSqr, mc.lift);

        dx  := V.New(NUMBER(x^));
        dxv := VT.Norm1(x) * difdist;
        (*dx := V.Scale(x, 1.0D-2);*)
        derwavdist, dersmooth: FnD.T;

      <*FATAL Thread.Alerted, Wr.Failure*>
      BEGIN
        IF constWavAmp THEN
          VAR zerovec := V.New(mc.lift.getNumber());
          BEGIN
            FOR i := FIRST(zerovec^) TO LAST(zerovec^) DO
              zerovec[i] := R.Zero;
            END;
            derwavdist :=
              FnD.T{zeroth := derdist.zeroth, first :=
                    V.FromVectorArray(
                      ARRAY OF V.T{derdist.first, V.FromScalar(R.Zero)}),
                    second := M.FromMatrixArray(
                                ARRAY [0 .. 1], [0 .. 1] OF
                                  M.T{ARRAY OF
                                        M.T{derdist.second,
                                            M.ColumnFromArray(zerovec^)},
                                      ARRAY OF
                                        M.T{M.RowFromArray(zerovec^),
                                            M.FromScalar(R.One)}})};
          END;
        ELSE
          derwavdist :=
            ExtendDervTarget(derdist, mc.lift.getData(), mc.wavelet0Amp,
                             waveletVec, waveletCor, targetVec);
        END;
        IO.Put(
          Fmt.FN("ComputeOptCritDiff for x=%s", ARRAY OF TEXT{VF.Fmt(x)}));
        FOR i := FIRST(dx^) TO LAST(dx^) DO dx[i] := dxv END;
        CASE 3 OF
        | 0 =>
            dersmooth := Fn.EvalCentralDiff2(SquareSmoothEstimate, x, dx);
        | 1 => dersmooth := Fn.EvalCentralDiff2(TransitionSpecRad, x, dx);
        | 2 => dersmooth := Fn.EvalCentralDiff2(TransitionBSpline, x, dx);
        | 3 =>
            dersmooth := Fn.EvalCentralDiff2(TransitionFrobenius, x, dx);
        ELSE
          <*ASSERT FALSE*>
        END;
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
      maxIter    = 20;
      smoothFac  = 2.0D0;
      maxSubIter = 30;

      ymin = -3.5D0;
      ymax = 3.5D0;

      tol     = 1.0D-4;
      difdist = 1.0D-5;

    VAR
      (*
      x := V.FromVectorArray(
             ARRAY OF V.T{V.New(2 * translates), V.FromScalar(R.One)});
      *)
      (*
      x := V.FromVectorArray(
             ARRAY OF
               V.T{V.ArithSeq(2 * translates, -0.45D0, 0.1D0),
                   V.FromScalar(R.One)});
      *)
      (* use this initialization if you want to compare the results with
         MatchPattern

         x := V.New(2 * translates + 1); *)
      initlift := NEW(S.T).init(yfirst, 2 * translates);
      initderdist := DeriveDist(
                       normalMat, targetCor, targetNormSqr, initlift);
      initderwavdist := ExtendDervTarget(
                          initderdist, initlift.getData(), R.Zero,
                          waveletVec, waveletCor, targetVec);
      x := V.Neg(LA.LeastSquares(initderwavdist.second,
                                 ARRAY OF V.T{initderwavdist.first})[0].x);
      initWavelet0Amp := x[LAST(x^)];

      smoothWeightFade := smoothWeight / RIntPow.Power(smoothFac, maxIter);

    BEGIN
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
        VAR
          mc := SplitParamVec(x, initWavelet0Amp);
          gdual := gdual0.superpose(
                     hdualvan.convolve(
                       mc.lift.scale(R.One / mc.wavelet0Amp).upsample(2)));
        BEGIN
          PL.StartPage();
          WP.PlotWaveletsYLim(hdual, gdual, levels, ymin, ymax);
          (*PL.StopPage();*)
        END;
        smoothWeightFade := smoothWeightFade * smoothFac;
      END;
      PL.Exit();
      RETURN SplitParamVec(x, initWavelet0Amp);
    END;
  END MatchPatternSmooth;

PROCEDURE TestMatchPatternSmooth (target: S.T;
                                  levels, smooth, vanishing, translates: CARDINAL;
                                  smoothWeight: R.T)
  RAISES {BSpl.DifferentParity} =
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>
  VAR
    hdual  := BSpl.GeneratorMask(smooth);
    gdual0 := BSpl.WaveletMask(smooth, vanishing);
    hdualvan := SIntPow.MulPower(
                  hdual, NEW(S.T).fromArray(
                           ARRAY OF R.T{RT.Half, R.Zero, -RT.Half}),
                  vanishing).translate(2 - smooth - vanishing);
    hdualnovan := BSpl.GeneratorMask(smooth + vanishing).translate(
                    2 - smooth - vanishing).scale(
                    RIntPow.Power(R.Two, vanishing)); (*compensate factors
                                                         (0.5,0.5) from the
                                                         mask*)
    gdual0novan := BSpl.WaveletMask(smooth + vanishing, 0);

    (*
    mc := MatchCoef{NEW(S.T).fromArray(
                      ARRAY OF R.T{1.0D0, 0.0D0, 0.0D0, 1.0D0}), 1.5D0};
    *)
    mc := MatchPatternSmooth(target, hdual, gdual0, hdualvan, hdualnovan,
                             gdual0novan, levels, translates, smoothWeight);
    vanatom := NEW(S.T).fromArray(ARRAY OF R.T{RT.Half, -RT.Half});
    s := SIntPow.MulPower(
           mc.lift.translate((2 - smooth - vanishing) DIV 2), vanatom,
           vanishing);
    gdual0a := gdual0.scale(mc.wavelet0Amp);
    gduala  := gdual0a.superpose(s.upsample(2).convolve(hdual));
    gdual := gdual0.superpose(
               s.upsample(2).convolve(hdual.scale(R.One / mc.wavelet0Amp)));
    unit          := IIntPow.Power(2, levels);
    twopow        := FLOAT(unit, R.T);
    grid          := R.One / twopow;
    psidual0      := Refn.Refine(gdual0a, hdual, levels);
    psidual       := Refn.Refine(gduala, hdual, levels);
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
      Fmt.FN("optimal lift %s,\ncyclic wrap of gdual %s\n\n"
             (* & "hsdual\n%s\n%s\n\n" & "gdual0\n%s\n%s\n\n" &
                "gdual\n%s\n%s\n"*),
             ARRAY OF
               TEXT{SF.Fmt(s), SF.Fmt(gdual.alternate().wrapCyclic(3))
               (*, SF.Fmt(s.upsample(2).convolve(hdual)),
                  SF.Fmt(SIntPow.MulPower(mc.lift.upsample(2).convolve(
                  hdualnovan), vanatom, vanishing)), SF.Fmt(gdual0),
                  SF.Fmt(SIntPow.MulPower(gdual0novan, vanatom,
                  vanishing)), SF.Fmt(gdual), SF.Fmt(SIntPow.MulPower(
                  GetLiftedPrimalGeneratorMask( hdualnovan, gdual0novan,
                  mc).alternate(), vanatom, vanishing))*)}));
    CASE 2 OF
    | 0 => PL.Init(); WP.PlotWavelets(hdual, gdual, levels); PL.Exit();
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

PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  TYPE
    Example = {matchBSpline, matchBSplineVan, matchBSplineWavelet,
               matchRamp, matchRampSmooth, matchSincSmooth, matchLongRamp,
               testSSE, testInverseDSSE, testDeriveWSSE};
  BEGIN
    CASE Example.matchBSplineWavelet OF
    | Example.matchBSpline =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(4), 7).translate(-50), 6,
          4, 0, 5);
    | Example.matchBSplineVan =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(1), 7).translate(10), 6, 4,
          2, 5);
    | Example.matchRamp =>
        (* The figures given here previously was wrong because the
           generator was convolved with (1,0,-1) instead of (1,-1)
           translates 5, size 1917, residuum 0.00340514677538585,
           V11{0.186869299925214, 0.269986933917237, 0.670508585560263,
           0.649776682132423, -0.175806649674353, -0.875993675942413,
           -0.856283049545732, -0.458477950438848, -0.31397715987086,
           -0.11516417311729, ...} 0.330691666379811 *)
        MatchPattern(
          NEW(S.T).fromArray(
            V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^, -256), 6, 4, 6, 5);
    | Example.matchRampSmooth =>
        (*
          TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), 6, 4, 2, 5, 50.0D0);
          TestMatchPatternSmooth(NEW(S.T).fromArray(
                                   V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                   -256), 6, 4, 4, 5, 20.0D0);
        *)
        TestMatchPatternSmooth(NEW(S.T).fromArray(
                                 V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                 -256), 6, 4, 6, 5, 5.0D0);
    | Example.matchBSplineWavelet =>
        (*
          MatchPattern(
            Refn.Refine(
              BSpl.WaveletMask(2, 8), BSpl.GeneratorMask(2), 6).scale(
              64.0D0).translate(10), 6, 2, 8, 5);
        *)
        TestMatchPatternSmooth(Refn.Refine(BSpl.WaveletMask(2, 8),
                                           BSpl.GeneratorMask(2), 6).scale(
                                 64.0D0).translate(50), 6, 2, 8, 5, 1.0D0);
    | Example.matchSincSmooth =>
        TestMatchPatternSmooth(
          NEW(S.T).fromArray(V.Neg(SincVector(2048, 64))^, 64 - 2048), 6,
          4, 6, 10, 1.0D-3);
    | Example.matchLongRamp =>
        (*matching a pattern with 1 vanishing moment with a wavelet of 9
           vanishing moments can't work obviously*)
        MatchPattern(NEW(S.T).fromArray(
                       V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,
                       64 - 1024), 6, 3, 9, 5);
      (*
      TestMatchPatternSmooth(
        NEW(S.T).fromArray(
          V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^,32 -1024), 6, 3, 1,
        5, 0.0D-4);
      *)
    | Example.testSSE =>
        TestSSE(V.FromArray(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0}));
        TestSSE(V.FromArray(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0}));
    | Example.testInverseDSSE =>
        TestInverseDSSE(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 0.1D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0});
    | Example.testDeriveWSSE => TestDeriveWSSE();
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestMatchWavelet.
