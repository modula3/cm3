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
    frac       := R.One / order;
    orderupper := order;
    orderlower := order;
    h, g       := S.One;
  BEGIN
    PL.Init();
    FOR i := 0 TO num - 1 DO
      VAR
        val := FLOAT(2 * i + 1, R.T) * frac;
        swap := g.superpose(h.convolve(
                              NEW(S.T).fromArray(
                                ARRAY OF R.T{val, R.Zero, val}, -1)));
      BEGIN
        g := h;
        h := swap;
      END;
      IO.Put(Fmt.FN("sum: %s, filter: %s\n",
                    ARRAY OF TEXT{RF.Fmt(h.sum()), SF.Fmt(h)}));
      WP.PlotWavelets(h, g, 6);
      FOR i := 0 TO 3 DO PL.Advance(i); PL.Clear(); END;
      orderupper := orderupper + R.Two;
      orderlower := orderlower - R.Two;
      frac := R.One / (frac * orderlower * orderupper);
    END;
    PL.Exit();
  END LiftCDFEven;

PROCEDURE LiftCDFOdd (order: R.T; num: CARDINAL) =
  BEGIN
  END LiftCDFOdd;

PROCEDURE Test () =
  BEGIN
    CASE 1 OF
    | 0 =>
        CONST halforder = 3;
        BEGIN
          LiftCDFEven(FLOAT(halforder * 2, R.T), halforder);
        END;
    | 1 => LiftCDFEven(10.71D0, 10);
    | 2 => LiftCDFOdd(3.0D0, 2);
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestCDFLifting.
