MODULE TestCDFLifting;

IMPORT LongRealBasic AS R;
IMPORT LongRealSignal AS S;

IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
(*IMPORT NADefinitions AS NA;*)

PROCEDURE ShowFilters () =
  <*FATAL BSpl.DifferentParity, Thread.Alerted, Wr.Failure*>
  BEGIN
    IO.Put(Fmt.FN("dual generator: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpl.GeneratorMask(2))}));
    IO.Put(Fmt.FN("dual wavelet: %s\n",
                  ARRAY OF TEXT{SF.Fmt(BSpl.WaveletMask(2, 2))}));
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
      VAR hscaled := h.scale(R.One / fac);
      BEGIN
        IO.Put(
          Fmt.FN("sum: %s, filter: %s\n",
                 ARRAY OF TEXT{RF.Fmt(hscaled.sum()), SF.Fmt(hscaled)}));
      END;
      (*WP.PlotWavelets(h, g, 6);*)
      WP.PlotWavelets(
        h.scale(R.One / h.sum()), g.scale(R.One / g.sum()), 6);
      FOR i := 0 TO 3 DO PL.Advance(i); PL.Clear(); END;
      orderupper := orderupper + R.Two;
      orderlower := orderlower - R.Two;
    END;
    PL.Exit();
  END LiftCDFEven;

PROCEDURE LiftCDFOdd (order: R.T; num: CARDINAL) =
  BEGIN
  END LiftCDFOdd;

PROCEDURE Test () =
  BEGIN
    CASE 0 OF
    | 0 =>
        CONST halforder = 4;
        BEGIN
          LiftCDFEven(FLOAT(halforder * 2, R.T), halforder + 2);
        END;
    | 1 => LiftCDFEven(10.71D0, 10);
    | 2 => LiftCDFOdd(3.0D0, 2);
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestCDFLifting.
