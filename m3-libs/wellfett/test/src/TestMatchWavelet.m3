MODULE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVectorFast AS V;
IMPORT LongRealVectorSupport AS VS;
IMPORT LongRealVectorTrans AS VT;

IMPORT LongRealMatrixFast AS M;
IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealFunctionalDeriv2 AS FD;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealMatrixFmtLex AS MF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
IMPORT NADefinitions AS NA;

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
                          ARRAY OF R.T{1.0D0, 0.0D0, -1.0D0}, -2),
                 vanishing);
    phivan := Refn.Refine(vancore.scale(RT.SqRtTwo), hdual, levels);
    psi    := Refn.Refine(gdual.scale(R.One / RT.SqRtTwo), hdual, levels);

    unit   := IIntPow.Power(2, levels);
    twonit := 2 * unit;
    first  := MIN(psi.getFirst(), phivan.getFirst() - twonit * translates);
    last := MAX(
              psi.getLast(), phivan.getLast() + twonit * (translates - 1));
    size := last - first + 1;

    twopow   := FLOAT(unit, R.T);
    grid     := R.One / twopow;
    abscissa := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

    targetvec := V.New(size);
    basis     := M.New(2 * translates + 1, size);

    coef: LA.LS;

  CONST
    ymin = -1.5D0;
    ymax = 1.5D0;

  BEGIN
    PL.Init();
    PL.SetEnvironment(
      abscissa[FIRST(abscissa^)], abscissa[LAST(abscissa^)], ymin, ymax);

    psi.scale(twopow).clipToArray(first, basis[LAST(basis^)]);
    PL.SetColor0(1);
    PL.PlotLines(abscissa^, basis[LAST(basis^)]);
    PL.SetColor0(2);
    FOR j := -translates TO translates - 1 DO
      phivan.scale(twopow).clipToArray(
        first - twonit * j, basis[j + translates]);
      PL.PlotLines(abscissa^, basis[j + translates]);
    END;

    target.clipToArray(first, targetvec^);
    PL.SetColor0(3);
    PL.PlotLines(abscissa^, targetvec^);

    IO.Put(Fmt.FN("normal matrix %s, right hand side %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(M.MulMMA(basis)),
                         VF.Fmt(M.MulV(basis, targetvec))}));

    coef := LA.LeastSquaresGen(
              basis, ARRAY OF V.T{targetvec},
              flags := LA.LSGenFlagSet{LA.LSGenFlag.transposed})[0];

    IO.Put(Fmt.FN("translates %s, size %s, residuum %s, %s\n",
                  ARRAY OF
                    TEXT{Fmt.Int(translates), Fmt.Int(size),
                         RF.Fmt(coef.res), VF.Fmt(coef.x)}));

    PL.SetColor0(4);
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
          V.Add(y, LA.LeastSquaresGen(
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
                      s            : S.T; ): FD.T RAISES {NA.Error} =
  VAR
    normals := M.MulV(normalMat, s.getData());
    dist := V.Inner(s.getData(), V.Sub(normals, V.Scale(targetCor, R.Two)))
              + targetNormSqr;

  BEGIN
    RETURN FD.T{zeroth := dist, first :=
                  V.Scale(V.Sub(normals, targetCor), R.Two), second :=
                  M.Scale(normalMat, R.Two)};
  END DeriveDist;

PROCEDURE DeriveSSE (hdual, gdual0, s: S.T; ): FD.T RAISES {NA.Error} =
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
      FD.T{zeroth := ComputeSSE(hsums^), first :=
             M.MulV(dsums, ComputeDSSE(hsums^)), second :=
             M.Mul(M.Mul(dsums, ComputeDDSSE(hsums^)), M.Transpose(dsums))};
  END DeriveSSE;

PROCEDURE DeriveWSSE (hdual, gdual0, s: S.T; c: R.T): FD.T
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

    RETURN
      FD.T{
        zeroth := sse, first := M.MulV(dcsums, dsse), second :=
        M.Add(M.Mul(M.Mul(dcsums, ddsse), M.Transpose(dcsums)),
              M.FromMatrixArray(
                ARRAY OF
                  ARRAY OF M.T{
                  ARRAY [0 .. 1] OF
                    M.T{M.NewZero(s.getNumber(), s.getNumber()),
                        M.ColumnFromVector(dgsums)},
                  ARRAY [0 .. 1] OF
                    M.T{M.RowFromVector(dgsums),
                        M.FromScalar(V.Inner(dsse, csums) * -2.0D0 / c)}}))};
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
    derArr := NEW(REF ARRAY OF FD.T, s.getNumber() + 1);
  BEGIN
    FOR i := 0 TO s.getNumber() - 1 DO
      derArr[i] :=
        DeriveWSSE(hdual, gdual0, s.superpose(sdelta.translate(i)), c);
    END;
    derArr[LAST(derArr^)] := DeriveWSSE(hdual, gdual0, s, c + delta);
    PutDervDif(der, derArr^, delta);
  END TestDeriveWSSE;


PROCEDURE PutDervDif (READONLY der   : FD.T;
                      READONLY derArr: ARRAY OF FD.T;
                               delta : R.T              ) =
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
PROCEDURE ExtendDervTarget (READONLY x        : FD.T;
                                     lift     : V.T;
                                     targetAmp: R.T;
                                     target   : V.T;
                                     targetCor: V.T;
                                     wavelet0 : V.T     ): FD.T
  RAISES {NA.Error} =
  BEGIN
    RETURN
      FD.T{
        zeroth := x.zeroth + R.Two * targetAmp * V.Inner(targetCor, lift)
                    + targetAmp * targetAmp * V.Inner(target, target)
                    - R.Two * targetAmp * V.Inner(wavelet0, target),
        first :=
        V.FromVectorArray(
          ARRAY OF
            V.T{
            V.Add(x.first, V.Scale(targetCor, targetAmp * R.Two)),
            V.FromScalar(R.Two * (V.Inner(lift, targetCor)
                                    + targetAmp * V.Inner(target, target)
                                    - V.Inner(wavelet0, target)))}),
        second :=
        M.FromMatrixArray(
          ARRAY [0 .. 1], [0 .. 1] OF
            M.T{
            ARRAY OF
              M.T{x.second, M.ColumnFromArray(V.Scale(targetCor, R.Two)^)},
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
  MatchCoef = RECORD
                lift: S.T;
                amp : R.T;       (*coefficient of the target function*)
              END;

PROCEDURE MatchPatternSmooth (target                : S.T;
                              hdual, gdual, hdualvan: S.T;
                              levels, translates    : CARDINAL;
                              smoothWeight          : R.T;      ):
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
      der := FD.Add(
               DeriveDist(normalMat, waveletCor, waveletNormSqr, y),
               FD.Scale(DeriveSSE(hdualvan, gdual, y), smoothWeight));
      derArr := NEW(REF ARRAY OF FD.T, NUMBER(der.first^));
      extder := ExtendDervTarget(
                  der, y.getData(), cf, targetVec, targetCor, waveletVec);
      extderArr := NEW(REF ARRAY OF FD.T, NUMBER(extder.first^));

    BEGIN
      FOR j := 0 TO LAST(derArr^) DO
        VAR
          yp := y.superpose(NEW(S.T).fromArray(
                              ARRAY OF R.T{delta}, j + y.getFirst()));
        BEGIN
          derArr[j] :=
            FD.Add(
              DeriveDist(normalMat, waveletCor, waveletNormSqr, yp),
              FD.Scale(DeriveSSE(hdualvan, gdual, yp), smoothWeight));
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
    generatorvan := Refn.Refine(hdualvan.scale(RT.SqRtTwo), hdual, levels);
    wavelet := Refn.Refine(gdual.scale(R.One / RT.SqRtTwo), hdual, levels);

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

    CONST tol = 1.0D-10;
    VAR
      yfirst := -translates;
      y      := NEW(S.T).fromArray(V.ArithSeq(2 * translates)^, yfirst);
      cf     := R.One;
    (* use this initialization if you want to compare the results with
       MatchPattern

       y := NEW(S.T).fromArray(V.New(2 * translates)^, yfirst); cf :=
       R.Zero; *)
    (*
      y := V.FromVectorArray(
             ARRAY OF V.T{V.ArithSeq(2 * translates), V.FromScalar(R.One)});
    *)

    BEGIN
      FOR j := 0 TO 100 DO
        VAR
          (*
          der := FD.Add(
                   DeriveDist(normalMat, targetCor, targetNormSqr, y),
                   FD.Scale(DeriveSSE(hdualvan, gdual, y), smoothWeight));
          extder := ExtendDervTarget(der, y.getData(), cf, V.Neg(waveletVec),
                                     V.Neg(waveletCor), V.Neg(targetVec));
          *)
          (* changed role of 'target' and 'wavelet'

             der := FD.Add( DeriveDist(normalMat, waveletCor,
             waveletNormSqr, y), FD.Scale(DeriveSSE(hdualvan, gdual, y),
             smoothWeight)); extder := ExtendDervTarget(der, y.getData(),
             cf, targetVec, targetCor, waveletVec); *)
          derdist := DeriveDist(normalMat, targetCor, targetNormSqr, y);
          derwavdist := ExtendDervTarget(derdist, y.getData(), cf,
                                         waveletVec, waveletCor, targetVec);
          der := FD.Add(derwavdist,
                         FD.Scale(DeriveWSSE(hdualvan, gdual, y, cf),
                                   smoothWeight));

        (* targetdiff := V.Sub(targetVec, M.MulTV(basis, y.getData()));
           targetdist := V.Inner(targetdiff, targetdiff); *)
        BEGIN
          (*
          IO.Put(
            Fmt.FN(
              "normal matrix %s, right hand side %s =?= %s, dist %s\n",
              ARRAY OF
                TEXT{MF.Fmt(M.Scale(der.second, RT.Half)),
                     VF.Fmt(V.Scale(der.first, RT.Half)),
                     VF.Fmt(M.MulV(basis, targetVec)), RF.Fmt(der.zeroth)}));
          *)

          IF VT.Norm1(der.first) <= tol * RT.Abs(der.zeroth) THEN
            RETURN MatchCoef{y, cf};
          END;
          (*
            IO.Put(Fmt.FN("derivatives %s, %s, %s\n",
                          ARRAY OF
                            TEXT{RF.Fmt(der.zeroth), VF.Fmt(der.first),
                                 MF.Fmt(der.second)}));
          *)
          VAR
            vec := LA.LeastSquaresGen(
                     der.second, ARRAY OF V.T{V.Neg(der.first)})[0].x;
          BEGIN
            cf := cf + vec[LAST(vec^)];
            y := y.superpose(NEW(S.T).fromArray(SUBARRAY(vec^, FIRST(vec^),
                                                         NUMBER(vec^) - 1),
                                                yfirst));
            (*
            IO.Put(
              Fmt.FN("y %s, cf %s\n", ARRAY OF TEXT{SF.Fmt(y), RF.Fmt(cf)}));
            *)
          END;
        END;
      END;
      RAISE NA.Error(NA.Err.not_converging);
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
                           ARRAY OF R.T{1.0D0, 0.0D0, -1.0D0}, -2),
                  vanishing);
    mc := MatchPatternSmooth(target, hdual, gdual0, hdualvan, levels,
                             translates, smoothWeight);
    s := SIntPow.MulPower(
           mc.lift, NEW(S.T).fromArray(ARRAY OF R.T{1.0D0, -1.0D0}, -1),
           vanishing);
    gdual0a := gdual0.scale(mc.amp / RT.SqRtTwo);
    gduala := gdual0a.superpose(
                s.upsample(2).convolve(hdual.scale(RT.SqRtTwo)));
    gdual := gdual0.superpose(
               s.upsample(2).convolve(hdual.scale(R.Two / mc.amp)));
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
      Fmt.FN(
        "optimal lift %s,\ncyclic wrap of gdual %s\n",
        ARRAY OF TEXT{SF.Fmt(s), SF.Fmt(gdual.alternate().wrapCyclic(3))}));
    PL.Init();
    CASE 1 OF
    | 0 => WP.PlotWavelets(hdual, gdual, levels);
    | 1 =>
        PL.SetEnvironment(
          MIN(lefttarget, MIN(leftpsidual, leftpsidual0)),
          MAX(righttarget, MAX(rightpsidual, rightpsidual0)), ymin, ymax);
        PL.SetColor0(3);
        PL.PlotLines(V.ArithSeq(target.getNumber(), lefttarget, grid)^,
                     target.getData()^);
        PL.SetColor0(1);
        PL.PlotLines(V.ArithSeq(psidual0.getNumber(), leftpsidual0, grid)^,
                     psidual0.getData()^);
        PL.SetColor0(4);
        PL.PlotLines(V.ArithSeq(psidual.getNumber(), leftpsidual, grid)^,
                     psidual.getData()^);
    ELSE
      <*ASSERT FALSE*>
    END;
    PL.Exit();
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
  BEGIN
    CASE 4 OF
    | 0 =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(4), 7).translate(-50), 6,
          4, 0, 5);
    | 1 =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(1), 7).translate(10), 6, 4,
          2, 5);
    | 2 =>
        (* The figures given here previously was wrong because the
           generator was convolved with (1,0,-1) instead of (1,-1)
           translates 5, size 1917, residuum 0.00340514677538585,
           V11{0.186869299925214, 0.269986933917237, 0.670508585560263,
           0.649776682132423, -0.175806649674353, -0.875993675942413,
           -0.856283049545732, -0.458477950438848, -0.31397715987086,
           -0.11516417311729, ...} 0.330691666379811 *)
        MatchPattern(
          NEW(S.T).fromArray(
            V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^, -256), 6, 4, 2, 5);
    | 3 =>
        TestMatchPatternSmooth(NEW(S.T).fromArray(
                                 V.ArithSeq(512, -1.0D0, 2.0D0 / 512.0D0)^,
                                 -256), 6, 4, 2, 5, 0.0D0);
    | 4 =>
        MatchPattern(
          NEW(S.T).fromArray(V.Neg(SincVector(2048, 64))^, 64 - 2048), 6, 4, 2, 10);
    | 5 =>
        TestMatchPatternSmooth(
          NEW(S.T).fromArray(
            V.ArithSeq(2048, -1.0D0, 2.0D0 / 2048.0D0)^, -1024), 6, 3, 9,
          1, 100.0D0);
    | 6 =>
        TestSSE(V.FromArray(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0}));
        TestSSE(V.FromArray(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0}));
    | 7 =>
        TestInverseDSSE(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 0.1D0});
        TestInverseDSSE(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0});
    | 8 => TestDeriveWSSE();
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestMatchWavelet.
