MODULE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT LongRealTrans AS RT;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVectorFast AS V;
IMPORT LongRealVectorSupport AS VS;
IMPORT LongRealVectorTrans AS VT;

IMPORT LongRealMatrixFast AS M;
IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealMatrixFmtLex AS MF;
IMPORT LongRealSignalFmtLex AS SF;
(*IMPORT LongRealWaveletPlot AS WP;*)
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
    vancore := SIntPow.MulPower(hdual, NEW(S.T).fromArray(
                                         ARRAY OF R.T{1.0D0, -1.0D0}, -1),
                                vanishing);
    phivan := Refn.Refine(vancore.scale(RT.SqRtTwo), hdual, levels);
    psi    := Refn.Refine(gdual.scale(R.One / RT.SqRtTwo), hdual, levels);

    unit   := IIntPow.Power(2, levels);
    twonit := 2 * unit;
    first  := MIN(psi.getFirst(), phivan.getFirst() - twonit * translates);
    last := MAX(
              psi.getLast(), phivan.getLast() + twonit * (translates - 1));
    size := last - first + 1;

    grid     := R.One / FLOAT(unit, R.T);
    abscissa := V.ArithSeq(size, FLOAT(first, R.T) * grid, grid);

    targetvec := V.New(size);
    basis     := M.New(2 * translates + 1, size);

    coef: LA.LS;

  BEGIN
    PL.Init();
    PL.SetEnvironment(
      abscissa[FIRST(abscissa^)], abscissa[LAST(abscissa^)], -1.5D0 * grid,
      1.5D0 * grid);

    psi.clipToArray(first, basis[LAST(basis^)]);
    PL.SetColor0(1);
    PL.PlotLines(abscissa^, basis[LAST(basis^)]);
    PL.SetColor0(2);
    FOR j := -translates TO translates - 1 DO
      phivan.clipToArray(first - twonit * j, basis[j + translates]);
      PL.PlotLines(abscissa^, basis[j + translates]);
    END;

    target.clipToArray(first, targetvec^);
    PL.SetColor0(3);
    PL.PlotLines(abscissa^, targetvec^);

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


PROCEDURE ComputeRho (READONLY y: ARRAY [0 .. 2] OF R.T): R.T =
  <*FATAL NA.Error*>
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    dif := 3.0D0 * p2 - p12;
  BEGIN
    RETURN dif * dif + 2.0D0 * p12 * p12;
  END ComputeRho;

PROCEDURE ComputeDRho (READONLY y: ARRAY [0 .. 2] OF R.T): V.T =
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
  END ComputeDRho;

(*
PROCEDURE ComputeDDRho (READONLY y: ARRAY [0 .. 2] OF R.T): M.T =
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    c0  := 6.0D0 * p12 - 2.0D0 * p2;
    c1  := 2.0D0*(3.0D0 * p2 - p12);

    z := M.New(NUMBER(y), NUMBER(y));
  BEGIN
    FOR i := 0 TO LAST(y) DO
      FOR j := i TO LAST(y) DO
        z[i, j] := c0 - 4.0D0 * p1 * (y[i] + y[j]) + 3.0D0 * y[i] * y[j];
        z[j, i] := z[i, j];
      END;
      z[i, i] := z[i, i] + c1;
    END;
    RETURN M.Scale(z, 6.0D0);
  END ComputeDDRho;
*)

PROCEDURE ComputeDDRho (READONLY y: ARRAY [0 .. 2] OF R.T): M.T =
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
  END ComputeDDRho;

PROCEDURE TestRho (x: V.T) =
  VAR
    dx0 := V.FromArray(ARRAY OF R.T{1.0D-8, 0.0D0, 0.0D0});
    dx1 := V.FromArray(ARRAY OF R.T{0.0D0, 1.0D-8, 0.0D0});
    dx2 := V.FromArray(ARRAY OF R.T{0.0D0, 0.0D0, 1.0D-8});
  BEGIN
    VAR
      rho     := ComputeRho(x^);
      rho0    := ComputeRho(V.Add(x, dx0)^);
      rho1    := ComputeRho(V.Add(x, dx1)^);
      rho2    := ComputeRho(V.Add(x, dx2)^);
      gradrho := V.Scale(ComputeDRho(x^), 1.0D-8);
    BEGIN
      IO.Put(
        Fmt.FN("rho %s, difrho={%s,%s,%s}, approxdiff=%s\n",
               ARRAY OF
                 TEXT{RF.Fmt(rho), RF.Fmt(rho0 - rho), RF.Fmt(rho1 - rho),
                      RF.Fmt(rho2 - rho), VF.Fmt(gradrho)}));
    END;
    VAR
      gradrho   := ComputeDRho(x^);
      gradrho0  := ComputeDRho(V.Add(x, dx0)^);
      gradrho1  := ComputeDRho(V.Add(x, dx1)^);
      gradrho2  := ComputeDRho(V.Add(x, dx2)^);
      jacobirho := M.Scale(ComputeDDRho(x^), 1.0D-8);
    BEGIN
      IO.Put(
        Fmt.FN("gradrho %s, difgradrho={%s,%s,%s}, approxdiff=%s\n",
               ARRAY OF
                 TEXT{VF.Fmt(gradrho), VF.Fmt(V.Sub(gradrho0, gradrho)),
                      VF.Fmt(V.Sub(gradrho1, gradrho)),
                      VF.Fmt(V.Sub(gradrho2, gradrho)), MF.Fmt(jacobirho)}));
    END;
  END TestRho;

PROCEDURE InverseDRho (x: V.T): V.T RAISES {NA.Error} =
  (*Find the parameter vector y for which DRho(y)=x*)
  CONST tol = 1.0D-14;
  (*VAR y := V.New(3);*)
  VAR y := x;                    (*zero is a bad initial value*)
  BEGIN
    FOR j := 0 TO 100 DO
      VAR ax := ComputeDRho(y^);
      BEGIN
        IF VT.Norm1(V.Sub(ax, x)) <= tol * VT.Norm1(x) THEN RETURN y; END;
        y :=
          V.Add(y, LA.LeastSquaresGen(
                     ComputeDDRho(y^), ARRAY OF V.T{V.Sub(x, ax)})[0].x);
        (*
          IO.Put(Fmt.FN("y %s, DRho(y) %s\n",
                       ARRAY OF TEXT{VF.Fmt(y), VF.Fmt(ComputeDRho(y^))}));
        *)
      END;
    END;
    RAISE NA.Error(NA.Err.not_converging);
  END InverseDRho;

PROCEDURE TestInverseDRho (READONLY x0: ARRAY [0 .. 2] OF R.T) =
  <*FATAL NA.Error*>
  VAR
    x := V.FromArray(x0);
    y := InverseDRho(x);
  BEGIN
    IO.Put(
      Fmt.FN("x %s, ComputeDRho(y) %s, y %s\n",
             ARRAY OF TEXT{VF.Fmt(x), VF.Fmt(ComputeDRho(y^)), VF.Fmt(y)}));
  END TestInverseDRho;

PROCEDURE MatchPatternSmooth (target: S.T;
                              levels, smooth, vanishing, translates: CARDINAL)
  RAISES {BSpl.DifferentParity} =
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure*>

  TYPE
    Deriv2 = RECORD
               zeroth: R.T;
               first : V.T;
               second: M.T;
             END;

  PROCEDURE Derivatives (s: S.T): Deriv2 =
    VAR
      gduallifted := gdual.superpose(s.upsample(2).convolve(hdual));
      hprimal     := gduallifted.alternate();
      gprimal     := hdual.alternate();
      (*
            hsums       := hprimal.translate(-2*s.getFirst()).wrapCyclic(3).getData();
      *)
      hsums := hprimal.wrapCyclic(3).getData();
      dsums := M.Cyclic(gprimal.translate(2 * s.getFirst()).wrapCyclic(
                          3).getData(), s.getNumber(), -1);
      normals := M.MulV(normalMat, s.getData());
      dist := V.Inner(
                s.getData(), V.Sub(normals, V.Scale(targetCor, R.Two)))
                + V.Inner(targetVec, targetVec);
      linpart := Deriv2{zeroth := dist, first :=
                        V.Scale(V.Sub(normals, targetCor), R.Two),
                        second := M.Scale(normalMat, R.Two)};
      polypart := Deriv2{zeroth := ComputeRho(hsums^), first :=
                         M.MulV(dsums, ComputeDRho(hsums^)), second :=
                         M.Mul(M.Mul(dsums, ComputeDDRho(hsums^)),
                               M.Transpose(dsums))};
    BEGIN
      IO.Put(MF.Fmt(dsums) & "\n");
      RETURN polypart;
      (*
            RETURN Deriv2{zeroth := linpart.zeroth + polypart.zeroth, first :=
                          V.Add(linpart.first, polypart.first), second :=
                          M.Add(linpart.second, polypart.second)};
      *)
    END Derivatives;

  VAR
    hdual := BSpl.GeneratorMask(smooth);
    gdual := BSpl.WaveletMask(smooth, vanishing);
    vancore := SIntPow.MulPower(hdual, NEW(S.T).fromArray(
                                         ARRAY OF R.T{1.0D0, -1.0D0}, -1),
                                vanishing);
    phivan := Refn.Refine(vancore.scale(RT.SqRtTwo), hdual, levels);
    psi    := Refn.Refine(gdual.scale(R.One / RT.SqRtTwo), hdual, levels);

    unit   := IIntPow.Power(2, levels);
    twonit := 2 * unit;
    first  := MIN(psi.getFirst(), phivan.getFirst() - twonit * translates);
    last := MAX(
              psi.getLast(), phivan.getLast() + twonit * (translates - 1));
    size := last - first + 1;

    targetVec := V.Sub(target.clipToVector(first, size),
                       psi.clipToVector(first, size));
    basis          := M.New(2 * translates, size);
    normalMat: M.T;
    targetCor: V.T;

  BEGIN
    FOR j := -translates TO translates - 1 DO
      phivan.clipToArray(first - twonit * j, basis[j + translates]);
      (*
            targetCor[j + translates] :=
              VS.Inner(basis[j + translates], targetVec^);
      *)
    END;

    normalMat := M.MMA(basis);
    targetCor := M.MulV(basis, targetVec);

    VAR
      y := NEW(S.T).fromArray(
             ARRAY OF R.T{0.2D0, -0.3D0, 0.0D0, -0.1D0}, -2);
      der := Derivatives(y);
      der0 := Derivatives(
                y.superpose(NEW(S.T).fromArray(ARRAY OF R.T{1.0D-8}, -2)));
      der1 := Derivatives(
                y.superpose(NEW(S.T).fromArray(ARRAY OF R.T{1.0D-8}, -1)));
      der2 := Derivatives(
                y.superpose(NEW(S.T).fromArray(ARRAY OF R.T{1.0D-8}, 0)));
      der3 := Derivatives(
                y.superpose(NEW(S.T).fromArray(ARRAY OF R.T{1.0D-8}, 1)));
    BEGIN
      IO.Put(VF.Fmt(V.Scale(der.first, 1.0D-8)) & "\n");
      IO.Put(
        Fmt.FN("der0 %s, %s\n", ARRAY OF
                                  TEXT{RF.Fmt(der0.zeroth - der.zeroth),
                                       RF.Fmt(der.first[0] * 1.0D-8)}));
      IO.Put(
        Fmt.FN("der1 %s, %s\n", ARRAY OF
                                  TEXT{RF.Fmt(der1.zeroth - der.zeroth),
                                       RF.Fmt(der.first[1] * 1.0D-8)}));
      IO.Put(
        Fmt.FN("der2 %s, %s\n", ARRAY OF
                                  TEXT{RF.Fmt(der2.zeroth - der.zeroth),
                                       RF.Fmt(der.first[2] * 1.0D-8)}));
      IO.Put(
        Fmt.FN("der3 %s, %s\n", ARRAY OF
                                  TEXT{RF.Fmt(der3.zeroth - der.zeroth),
                                       RF.Fmt(der.first[3] * 1.0D-8)}));
      IO.Put(
        Fmt.FN("der0 %s, %s\n", ARRAY OF
                                  TEXT{VF.Fmt(V.Sub(der0.first, der.first)),
                                       VF.Fmt(V.Scale(M.GetRow(der.second,0), 1.0D-8))}));
      IO.Put(
        Fmt.FN("der1 %s, %s\n", ARRAY OF
                                  TEXT{VF.Fmt(V.Sub(der1.first, der.first)),
                                       VF.Fmt(V.Scale(M.GetRow(der.second,1), 1.0D-8))}));
      IO.Put(
        Fmt.FN("der2 %s, %s\n", ARRAY OF
                                  TEXT{VF.Fmt(V.Sub(der2.first, der.first)),
                                       VF.Fmt(V.Scale(M.GetRow(der.second,2), 1.0D-8))}));
      IO.Put(
        Fmt.FN("der3 %s, %s\n", ARRAY OF
                                  TEXT{VF.Fmt(V.Sub(der3.first, der.first)),
                                       VF.Fmt(V.Scale(M.GetRow(der.second,3), 1.0D-8))}));
    END;

    (*
        (* CONST tol = 1.0D-14;*)
        (*VAR y := NEW(S.T).fromArray(V.New(2 * translates)^, -translates);*)
        VAR y := NEW(S.T).fromArray(V.ArithSeq(2 * translates)^, -translates);
        BEGIN
          FOR j := 0 TO 10 DO
            VAR
              der        := Derivatives(y);
              targetdiff := V.Sub(targetVec, M.MulTV(basis, y.getData()));
              targetdist := V.Inner(targetdiff, targetdiff);
            BEGIN
              IO.Put(Fmt.FN("derivatives %s, %s, %s\n",
                            ARRAY OF
                              TEXT{RF.Fmt(der.zeroth), VF.Fmt(der.first),
                                   MF.Fmt(der.second)}));
              y := y.superpose(NEW(S.T).fromVector(
                                 LA.LeastSquaresGen(
                                   der.second, ARRAY OF V.T{V.Neg(der.first)})[
                                   0].x, first := y.getFirst()));
              (*
                IO.Put(Fmt.FN("y %s, DRho(y) %s\n",
                             ARRAY OF TEXT{VF.Fmt(y), VF.Fmt(ComputeDRho(y^))}));
              *)
            END;
          END;
          (* RAISE NA.Error(NA.Err.not_converging); *)
        END;
    *)
  END MatchPatternSmooth;

(*
PROCEDURE MaximizeSmoothness (x: V.T) =
  VAR id := M.NewOne(3);
  BEGIN

  END MaximizeSmoothness;
*)

PROCEDURE Test () =
  <*FATAL BSpl.DifferentParity*>
  BEGIN
    CASE 3 OF
    | 0 =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(4), 7).translate(-50), 6,
          4, 0, 5);
    | 1 =>
        MatchPattern(
          Refn.Refine(S.One, BSpl.GeneratorMask(1), 7).translate(10), 6, 4,
          2, 5);
    | 2 =>
        MatchPattern(
          NEW(S.T).fromArray(
            V.ArithSeq(512, -0.01D0, 0.02D0 / 512.0D0)^, -256), 6, 4, 2, 5);
    | 3 =>
        MatchPatternSmooth(
          NEW(S.T).fromArray(
            V.ArithSeq(512, -0.01D0, 0.02D0 / 512.0D0)^, -256), 6, 4, 2, 2);
    | 4 =>
        TestRho(V.FromArray(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0}));
        TestRho(V.FromArray(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0}));
    | 5 =>
        TestInverseDRho(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0});
        TestInverseDRho(ARRAY OF R.T{1.0D0, 1.0D0, 0.1D0});
        TestInverseDRho(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0});
      (*
          | 6 =>
              MaximizeSmoothness(V.FromArray(ARRAY OF R.T{0.9D0, 0.7D0, -0.6D0}));
      *)
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestMatchWavelet.
