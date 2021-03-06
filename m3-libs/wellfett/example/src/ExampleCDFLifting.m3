MODULE ExampleCDFLifting;

IMPORT LongRealBasic AS R;
IMPORT LongRealSignal AS S;

IMPORT LongRealBSplineWavelet AS BSpline;

IMPORT LongRealFmtLex AS RF, LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WPlot, LongRealPLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;

<* UNUSED *>
PROCEDURE ShowFilters () =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    IO.Put(Fmt.FN("dual generator: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpline.GeneratorMask(2))}));
    IO.Put(Fmt.FN("dual wavelet: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpline.WaveletMask(2, 2))}));
  END ShowFilters;

PROCEDURE LiftCDFEven (order: R.T; num: CARDINAL) =
  VAR
    fac        := R.Two;
    orderupper := order;
    orderlower := order;
    g          := S.One;
    h          := NEW(S.T).fromArray(ARRAY OF R.T{order});
  BEGIN
    PL.Init();
    FOR i := 0 TO num - 1 DO
      VAR
        val := FLOAT(2 * i + 1, R.T);
        swap := g.scale(orderlower * orderupper).superpose(
                  h.convolve(
                    NEW(S.T).fromArray(ARRAY OF R.T{val, R.Zero, val}, -1)));
      BEGIN
        fac := fac * val;
        g := h;
        h := swap;
      END;
      <* FATAL Thread.Alerted, Wr.Failure *>
      VAR
        hscaled := h.scale(R.One / fac);
      BEGIN
        IO.Put(
          Fmt.FN("sum: %s, filter: %s\n",
                 ARRAY OF TEXT{RF.Fmt(hscaled.sum()), SF.Fmt(hscaled)}));
      END;
      PL.StartPage();
      (* WPlot.Biorthogonal(h, g, 6); *)
      WPlot.Biorthogonal(
        h.scale(R.One / h.sum()), g.scale(R.One / g.sum()), 6);
      PL.StopPage();
      orderupper := orderupper + R.Two;
      orderlower := orderlower - R.Two;
    END;
    PL.Exit();
  END LiftCDFEven;

PROCEDURE LiftCDFOdd (order: R.T; num: CARDINAL) =
  VAR
    frac       := R.One / order;
    orderupper := order + R.One;
    orderlower := order - R.One;
    g          := S.One;
    h          := S.One;
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    PL.Init();
    (*the first lifting step differs from the following ones*)
    VAR swap := h.superpose(g.scale(frac).translate(-1));
    BEGIN
      g := h;
      h := swap;
    END;
    IO.Put(Fmt.FN("filter: %s\n", ARRAY OF TEXT{SF.Fmt(h)}));
    FOR i := 0 TO num - 1 DO
      VAR
        val     := FLOAT(2 * i + 1, R.T);
        fractmp := R.One / (frac * (order - val) * (order + val));
        swap := g.superpose(
                  h.convolve(
                    NEW(S.T).fromArray(ARRAY OF
                                         R.T{(val + R.Two) * fractmp,
                                             R.Zero, val * fractmp}, -1)));
      BEGIN
        g := h;
        h := swap;
        frac := fractmp;
      END;
      VAR hscaled := h.scale(R.One / h.getValue(h.getFirst()));
      (*hscaled := h.scale(RT.Pow(R.Two,order));*)
      BEGIN
        IO.Put(
          Fmt.FN("sum: %s, filter: %s\n",
                 ARRAY OF TEXT{RF.Fmt(hscaled.sum()), SF.Fmt(hscaled)}));
      END;
      PL.StartPage();
      WPlot.Biorthogonal(h, g, 6);
      (* WPlot.Biorthogonal( h.scale(R.One / h.sum()), g.scale(R.One /
         g.sum()), 6); *)
      PL.StopPage();
      orderupper := orderupper + R.Two;
      orderlower := orderlower - R.Two;
    END;
    PL.Exit();
  END LiftCDFOdd;

PROCEDURE Run () =
  BEGIN
    CASE 2 OF
    | 0 =>
        CONST halforder = 4;
        BEGIN
          LiftCDFEven(FLOAT(halforder * 2, R.T), halforder + 2);
        END;
    | 1 => LiftCDFEven(10.71D0, 10);
    | 2 =>
        CONST halforder = 4;
        BEGIN
          LiftCDFOdd(FLOAT(halforder * 2 + 1, R.T), halforder);
        END;
    ELSE
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END ExampleCDFLifting.
